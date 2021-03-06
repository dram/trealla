#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <io.h>
#define snprintf _snprintf
#define fseeko _fseeki64
#define ftello _ftelli64
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

#define END_OF_FILE "end_of_file"

funcs g_bifs[MAX_BIFS] = {{0}};
size_t g_bifs_idx = 0;

#ifdef _WIN32
static char *strndup(const char *s, size_t n)
{
	size_t len = strlen(s);

	if (n < len)
		len = n;

	char *dstbuf = (char *)malloc(len + 1);
	dstbuf[len] = '\0';
	return (char *)memcpy(dstbuf, s, len);
}
#endif

#if USE_SSL && 0
static node *make_bignum(const node *v)
{
	node *n = term_make();
	n->flags |= TYPE_BIGNUM;

	if (is_integer(v))
		BN_set_word(n->val_bn, (nbr_t)v->val_i);
	else if (is_float(v))
		BN_set_word(n->val_bn, (nbr_t)v->val_f);
	else
		n->val_bn = BN_dup(v->val_bn);

	return n;
}
#endif

#if USE_SSL
static void put_bignum(tpl_query *q, unsigned point, node *v)
{
	node *n = term_make();
	n->flags |= TYPE_BIGNUM;
	n->val_bn = v->val_bn;
	v->flags = 0;
	v->val_bn = NULL;
	put_env(q, point, n, -1);
	n->refcnt--;
}
#endif

void reset_arg(tpl_query *q, const node *term, unsigned frame)
{
	env *e = get_env(q, frame + term->slot);

	if (e->term) {
		term_heapcheck(e->term);
		e->term = NULL;
	}

	e->context = 0;
}

#define S_NUMBERS (1000 * 100)

node *make_quick_int(nbr_t v)
{
	static node s_ints[S_NUMBERS + 1] = {{{0}}};

	if ((v >= 0) && (v <= S_NUMBERS)) {
		node *n = &s_ints[(size_t)v];
		n->flags = TYPE_INTEGER;
		n->val_i = v;
		return n;
	}

	return make_int(v);
}

static int expand_frame(tpl_query *q, unsigned cnt)
{
	extern int grow_environment(tpl_query * q);

	while ((q->c.env_point + cnt) >= q->envs_possible) {
		if (!grow_environment(q))
			return 0;
	}

	prepare_frame(q, cnt);
	q->c.env_point += cnt;
	choice *c = &q->choices[q->choice_point];
	c->frame_size += cnt;
	c->env_point += cnt;
	return 1;
}

static int collect_vars2(tpl_query *q, node *term, int depth)
{
	if (depth > MAX_UNIFY_DEPTH) {
		QABORT2(ABORT_MAXDEPTH, "COLLECT_VARS");
		return 0;
	}

	node *n = get_arg(q, term, q->latest_context);
	int cnt = 0;

	if (is_compound(n)) {
		unsigned save_context = q->latest_context;

		for (n = term_first(n); n; n = term_next(n)) {
			cnt += collect_vars2(q, n, depth + 1);
			q->latest_context = save_context;
		}
	}
	else if (is_var(n)) {
		env *e = get_env(q, q->latest_context + n->slot);

		if (!sl_get(q->d, (char *)e, NULL)) {
			sl_set(q->d, (char *)e, n);
			cnt++;
		}
	}

	return cnt;
}

static int collect_vars(tpl_query *q, node *n) { return collect_vars2(q, n, 1); }

node *copy_nbr(node *from)
{
	node *n = term_make();
	n->flags |= from->flags;

	if (is_stream(from)) {
		n->val_ptr = from->val_ptr;
		n->flags |= FLAG_CONST;
	}
	else if (is_float(from))
		n->val_f = from->val_f;
#if USE_SSL
	else if (is_bignum(from))
		n->val_bn = BN_dup(from->val_bn);
#endif
	else
		n->val_i = from->val_i;

	return n;
}

static node *copy_atom(node *from)
{
	node *n = term_make();
	n->flags |= from->flags;

	if (from->flags & FLAG_BLOB) {
		n->val_s = (char *)malloc(from->val_len + 1);
		memcpy(n->val_s, from->val_s, from->val_len);
		n->val_len = from->val_len;
		n->val_s[n->val_len] = '\0';
		n->flags &= ~FLAG_CONST;
	}
	else if (from->flags & FLAG_SMALL)
		strcpy(n->val_ch, from->val_ch);
	else if (from->flags & FLAG_CONST)
		n->val_s = from->val_s;
	else
		n->val_s = strdup(from->val_s);

	if (n->flags & FLAG_BUILTIN)
		n->bifptr = from->bifptr;
	else
		n->match = from->match;

	return n;
}

static node *copy_var(node *from)
{
	node *n = term_make();
	n->flags |= from->flags;
	n->val_s = from->val_s;
	n->slot = from->slot;
	return n;
}

node *copy_term2(tpl_query *q, node *from, int clone, int depth)
{
	if (depth > (1000*1000)) {
		QABORT2(ABORT_MAXDEPTH, "COPY_TERM");
		return 0;
	}

	if (is_number(from))
		return copy_nbr(from);

	if (is_atom(from))
		return copy_atom(from);

	if (is_var(from)) {
		if (clone)
			return copy_var(from);

		env *e = get_env(q, q->latest_context + from->slot);
		node *tmp;

		if (!q->d) {
			QABORT(ABORT_INVALIDARGMISSING);
			return 0;
		}

		if (!sl_get(q->d, (char *)e, (void **)&tmp))
			sl_set(q->d, (char *)e, tmp = make_var(q));
		else
			tmp = copy_var(tmp);

		return tmp;
	}

	if (!is_compound(from)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	node *n = make_compound();
	n->cpos = from->cpos;
	n->flags |= from->flags;
	n->bifptr = from->bifptr;
	n->frame_size = from->frame_size;
	from = term_first(from);
	int this_context = q->latest_context;

	while (from) {
		node *from2 = get_arg(q, from, this_context);
		node *tmp = copy_term2(q, from2, clone, depth + 1);
		term_append(n, tmp);
		from = term_next(from);
	}

	return n;
}

const funcs *get_bif(lexer *l, const char *functor)
{
	const funcs *fptr;
	const char *u = NULL;
	sl_start(&l->ns);

	do {
		char tmpbuf[(FUNCTOR_SIZE * 2) + 10];

		if (u != NULL)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", u, functor);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", functor);

		for (fptr = g_bifs; fptr->functor; fptr++) {
			if (!strcmp(fptr->functor, tmpbuf))
				return fptr;
		}
	} while ((u = sl_next(&l->ns, NULL)) != NULL);

	return fptr;
}

const funcs *get_bifarity(lexer *l, const char *functor, int arity)
{
	const funcs *fptr = NULL;
	const char *u = NULL;
	sl_start(&l->ns);

	do {
		char tmpbuf[(FUNCTOR_SIZE * 2) + 10];

		if (u != NULL)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", u, functor);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", functor);

		for (fptr = g_bifs; fptr->functor; fptr++) {
			if ((fptr->arity != -1) && (fptr->arity != arity))
				continue;

			if (!strcmp(fptr->functor, tmpbuf))
				return fptr;
		}
	} while ((u = sl_next(&l->ns, NULL)) != NULL);

	return fptr;
}

static int check_builtin(trealla *pl, const char *functarity)
{
	char tmpbuf[FUNCTOR_SIZE];
	tmpbuf[0] = '\0';
	int arity = 0;
	sscanf(functarity, "%1024[^/]/%d", tmpbuf, &arity);
	tmpbuf[sizeof(tmpbuf) - 1] = '\0';
	const funcs *fptr;

	for (fptr = g_bifs; fptr->functor; fptr++) {
		if (!strcmp(fptr->functor, tmpbuf) && ((fptr->arity == arity) || (fptr->arity == -1)))
			break;
	}

	return fptr->functor != NULL;
}

static int check_dynamic(module *db, const char *functarity)
{
	char *key = (char *)functarity;
	rule *r = NULL;

	if (sl_get(&db->rules, key, (void **)&r))
		return r->dynamic;

	return 0;
}

static int check_static(module *db, const char *functarity)
{
	char *key = (char *)functarity;
	rule *r = NULL;

	if (sl_get(&db->rules, key, (void **)&r))
		return !r->dynamic;

	return 0;
}

int bif_iso_true(tpl_query *q) { return 1; }

int bif_iso_fail(tpl_query *q) { return 0; }

static int bif_iso_halt(tpl_query *q)
{
	q->halt_code = 0;
	q->halt = ABORT_HALT;
	q->did_getc = 0;
	q->did_halt = 1;
	return 0;
}

static int bif_iso_halt_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	q->halt_code = term1->val_i;
	q->halt = ABORT_HALT;
	q->did_halt = 1;
	return 0;
}

int bif_iso_cut(tpl_query *q)
{
	trust_me(q);
	return 1;
}

int bif_xtra_cutfail(tpl_query *q)
{
	trust_me(q);
	return 0;
}

static int bif_iso_repeat(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);

	try_me_nofollow(q);
	return 1;
}

int bif_iso_and(tpl_query *q)
{
	if (q->retry)
		return 0;

	node *args = get_args(q);
	node *term1 = get_callable(term1);

	if (q->c.curr_term->flags & FLAG_PROMOTED) {
		allocate_frame(q);
		try_me_nochoice(q);
	}

	q->c.curr_term = term1;
	return call(q);
}

static int bif_iso_not(tpl_query *q)
{
	if (q->retry)
		return 1;

	node *args = get_args(q);
	node *term1 = get_callable(term1);
	allocate_frame(q);
	try_me(q);
	q->c.curr_term = term1;
	return call(q);
}

static int bif_iso_do(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_list_or_callable(term1);
	trust_me(q);
	q->c.curr_term = term1;
	return call(q);
}

int bif_iso_or(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term1x = get_callable(term1x); // make_true
	node *term2 = get_callable(term2);

	if (!q->retry) {
		allocate_frame(q);
		try_me(q);
		q->c.curr_term = term1;
	}
	else {
		q->retry = 0;
		try_me_nochoice(q);
		q->c.curr_term = term2;
	}

	return call(q);
}

static int bif_iso_once(tpl_query *q)
{
	if (q->retry)
		return 0;

	node *args = get_args(q);
	node *term1 = get_callable(term1);
	allocate_frame(q);
	try_me(q);
	q->c.curr_term = term1;
	return call(q);
}

static int bif_iso_call(tpl_query *q)
{
	if (q->retry)
		return 0;

	node *args = get_args(q);
	node *var = get_var(var); // FLAG_HIDDEN
	node *term1 = get_callable(term1);
	allocate_frame(q);
	try_me(q);
	q->c.curr_term = term1;
	return call(q);
}

static int bif_iso_calln(tpl_query *q)
{
	if (q->retry)
		return 0;

	node *args = get_args(q);
	node *var = get_var(var); // FLAG_HIDDEN
	node *term1 = get_callable(term1);
	node *param = get_next_arg(q, &args);
	node *s;

	if (is_structure(term1)) {
		s = clone_term(q, term1);
	}
	else {
		s = make_compound();
		term_append(s, clone_term(q, term1));
	}

	while (param) {
		term_append(s, clone_term(q, param));
		param = get_next_arg(q, &args);
	}

	const char *functor = VAL_S(term1);
	int arity = term_arity(s);
	s->bifptr = get_bifarity(q->lex, functor, arity)->bifptr;

	if (!s->bifptr)
		s->match = xref_term(q->lex, term1, arity);
	else
		s->flags |= FLAG_BUILTIN;

	put_env(q, q->c.curr_frame + var->slot, s, q->c.curr_frame);
	term_heapcheck(s);
	allocate_frame(q);
	try_me(q);
	q->c.curr_term = s;
	return call(q);
}

static int check_vars(tpl_query *q, node *n);

static int check_vars_compound(tpl_query *q, node *n)
{
	unsigned save_context = q->latest_context;

	for (n = term_first(n); n; n = term_next(n)) {
		if (check_vars(q, n))
			return 1;

		q->latest_context = save_context;
	}

	return 0;
}

static int check_vars(tpl_query *q, node *n)
{
	n = get_arg(q, n, q->latest_context);

	if (is_compound(n))
		return check_vars_compound(q, n);
	else if (is_var(n))
		return 1;

	return 0;
}

static int bif_iso_ground(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return !check_vars(q, term1);
}

static int bif_iso_var(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_var(term1);
}

static int bif_iso_nonvar(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return !is_var(term1);
}

static int bif_iso_atomic(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_atomic(term1);
}

static int bif_iso_atom(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_atom(term1);
}

static int bif_iso_number(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_number(term1);
}

static int bif_iso_compound(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_compound(term1);
}

static int bif_iso_callable(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_callable(term1);
}

int bif_iso_unify(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	int context1 = q->latest_context;
	node *term2 = get_term(term2);
	int context2 = q->latest_context;
	q->latest_context = context1;
	return unify(q, term1, context1, term2, context2);
}

static int bif_iso_notunify(tpl_query *q) { return !bif_iso_unify(q); }

static int compare_terms(tpl_query *q, node *term1, node *term2, int mode);
enum { CMP_NONE, CMP_LT, CMP_LE, CMP_EQ };

static int compare_compounds(tpl_query *q, node *term1, node *term2, int mode)
{
	node *n1 = term_first(term1), *n2 = term_first(term2);

	while (n1 && n2) {
		int status = compare_terms(q, n1, n2, mode);

		if (status > 0)
			return 1;

		if ((mode == CMP_EQ) && (status != 0))
			return -1;

		if ((mode == CMP_LE) && (status < 0))
			return -1;

		if ((mode == CMP_LT) && (status < 0))
			return -1;

		n1 = term_next(n1);
		n2 = term_next(n2);
	}

	return 0;
}

static int compare_terms(tpl_query *q, node *term1, node *term2, int mode)
{
	node *n1 = get_arg(q, term1, q->c.curr_frame);
	node *n2 = get_arg(q, term2, q->c.curr_frame);

	if ((is_integer(n1) || is_bignum(n1)) && (is_integer(n2) || is_bignum(n2))) {
		if (get_word(n1) < get_word(n2))
			return -1;

		if (get_word(n1) == get_word(n2))
			return 0;

		return 1;
	}
	else if (is_float(n1) && is_float(n2)) {
		if (n1->val_f < n2->val_f)
			return -1;

		if (n1->val_f == n2->val_f)
			return 0;

		return 1;
	}
	else if (is_var(n1) && is_var(n2)) {
		if (n1->slot < n2->slot)
			return -1;

		if (n1->slot == n2->slot)
			return 0;

		return 1;
	}
	else if (is_atom(n1) && is_atom(n2)) {
		return strcmp(VAL_S(n1), VAL_S(n2));
	}
	else if (is_compound(n1) && is_compound(n2)) {
		return compare_compounds(q, n1, n2, mode);
	}

	char tmpbuf1[FUNCTOR_SIZE];
	term_sprint(tmpbuf1, sizeof(tmpbuf1), q->pl, q, n1, 0);
	char tmpbuf2[FUNCTOR_SIZE];
	term_sprint(tmpbuf2, sizeof(tmpbuf2), q->pl, q, n2, 0);
	return strcmp(tmpbuf1, tmpbuf2);
}

static int bif_iso_slt(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	return compare_terms(q, term1, term2, CMP_LT) < 0;
}

static int bif_iso_sle(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	return compare_terms(q, term1, term2, CMP_LE) <= 0;
}

static int bif_iso_seq(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	return compare_terms(q, term1, term2, CMP_EQ) == 0;
}

static int bif_iso_sgt(tpl_query *q) { return !bif_iso_sle(q); }
static int bif_iso_sge(tpl_query *q) { return !bif_iso_slt(q); }
static int bif_iso_sne(tpl_query *q) { return !bif_iso_seq(q); }

static int bif_iso_atom_length(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int_or_var(term2);
	return unify_int(q, term2, UTF8LEN(term1));
}

static int bif_iso_atom_concat(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	size_t len = LEN(term1) + LEN(term2);
	char *tmp = (char *)malloc(len + 1);

	if (LEN(term1) > 0)
		memcpy(tmp, VAL_S(term1), LEN(term1));

	if (LEN(term2) > 0)
		memcpy(tmp + LEN(term1), VAL_S(term2), LEN(term2));

	tmp[len] = '\0';
	node *n;

#ifndef ISO_ONLY
	if (is_blob(term1) || is_blob(term2))
		n = make_blob(tmp, len);
	else
#endif
		n = make_atom(tmp);

	put_env(q, q->c.curr_frame + term3->slot, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_iso_set_prolog_flag(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	const char *flag = VAL_S(term1);

	if (!strcmp(flag, "char_conversion"))
		q->lex->flag_char_conversion = !strcmp(VAL_S(term2), "on") ? 1 : !strcmp(VAL_S(term2), "off") ? 0 : 0;
	else if (!strcmp(flag, "debug"))
		q->lex->flag_debug = !strcmp(VAL_S(term2), "on") ? 1 : !strcmp(VAL_S(term2), "off") ? 0 : 0;
	else if (!strcmp(flag, "double_quotes"))
		q->lex->flag_double_quotes =
		    !strcmp(VAL_S(term2), "atom") ? 1 : !strcmp(VAL_S(term2), "chars") ? 2 : !strcmp(VAL_S(term2), "codes") ? 0 : 0;
	else if (!strcmp(flag, "unknown"))
		q->lex->flag_unknown =
		    !strcmp(VAL_S(term2), "error") ? 1 : !strcmp(VAL_S(term2), "warning") ? 2 : !strcmp(VAL_S(term2), "fail") ? 0 : 0;
	else
		return 0;

	q->pl->flag_unknown = q->lex->flag_unknown;
	q->pl->flag_character_escapes = q->lex->flag_character_escapes;
	q->pl->flag_char_conversion = q->lex->flag_char_conversion;
	q->pl->flag_double_quotes = q->lex->flag_double_quotes;
	q->pl->flag_debug = q->lex->flag_debug;
	return 1;
}

static int bif_iso_current_prolog_flag(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	const char *flag = VAL_S(term1);

	if (!strcmp(flag, "max_integer"))
		return unify_int(q, term2, LONG_MAX);
	else if (!strcmp(flag, "min_integer"))
		return unify_int(q, term2, LONG_MIN);
	else if (!strcmp(flag, "max_arity"))
		return unify_int(q, term2, MAX_FRAME_SIZE - 1);
	else if (!strcmp(flag, "char_conversion"))
		return unify_const_atom(q, term2, q->lex->flag_char_conversion ? "on" : "off");
	else if (!strcmp(flag, "debug"))
		return unify_const_atom(q, term2, q->lex->flag_debug ? "on" : "off");
	else if (!strcmp(flag, "double_quotes"))
		return unify_const_atom(q, term2,
		                        q->lex->flag_double_quotes == 1
		                            ? "atom"
		                            : q->lex->flag_double_quotes == 2 ? "chars"
		                                                              : q->lex->flag_double_quotes == 0 ? "codes" : "codes"
		                       );
	else if (!strcmp(flag, "unknown"))
		return unify_const_atom(q, term2, q->lex->flag_unknown == 1 ? "error" : q->lex->flag_unknown == 2 ? "warning" : "fail"
		                        );
	else if (!strcmp(flag, "bounded"))
		return unify_const_atom(q, term2, g_force_unbounded ? "false" : "true");
	else if (!strcmp(flag, "integer_rounding_function"))
		return unify_const_atom(q, term2, "down");
	else if (!strcmp(flag, "debug"))
		return unify_const_atom(q, term2, q->lex->flag_debug ? "on" : "off");

	return 0;
}

static int bif_iso_current_op(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);
	node *term2 = get_atom_or_var(term2);
	node *term3 = get_atom(term3);

	// FIXME to backtrack

	const op *cur_op =
	    is_atom(term2) ? get_op_2(q->c.curr_db, VAL_S(term3), VAL_S(term2)) : get_op(q->c.curr_db, VAL_S(term3), 0);

	if (!cur_op->fun)
		return 0;

	if (!unify_const_atom(q, term2, cur_op->spec))
		return 0;

	if (!unify_int(q, term1, (int)cur_op->priority))
		return 0;

	return 1;
}

static int bif_iso_current_predicate(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_compound(term1);
	const char *functor = term_functor(term1);

	if (strcmp(functor, "/")) {
		QABORT(ABORT_INVALIDARGNOTPREDICATEINDICATOR);
		return 0;
	}

	node *fa = term_firstarg(term1);
	node *ta = term_next(fa);

	if (!is_atom(fa)) {
		QABORT(ABORT_INVALIDARGNOTATOM);
		return 0;
	}

	if (!is_integer(term_next(fa))) {
		QABORT(ABORT_INVALIDARGNOTINT);
		return 0;
	}

	functor = VAL_S(fa);
	int arity = get_word(ta);
	char tmpbuf[FUNCTOR_SIZE];
	sprintf(tmpbuf, "%s/%d", functor, arity);

	if (check_dynamic(q->c.curr_db, tmpbuf))
		return 1;

	if (check_dynamic(&q->pl->db, tmpbuf))
		return 1;

	if (check_static(q->c.curr_db, tmpbuf))
		return 1;

	if (check_static(&q->pl->db, tmpbuf))
		return 1;

	if (check_builtin(q->pl, tmpbuf))
		return 1;

	return 0;
}

static int bif_iso_open_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	const char *filename = VAL_S(term1);
	const char *mode = VAL_S(term2);
	const char *type = "text";
	char tmpbuf[40];
	strcpy(tmpbuf, !strcmp(mode, "append") ? "a" : !strcmp(mode, "update") ? "r+" : !strcmp(mode, "write") ? "w+" : "r");

	FILE *fp = fopen(filename, tmpbuf);

	if (!fp) {
		QABORT(ABORT_NOTEXISTFILE);
		return 0;
	}

	stream *sp = calloc(1, sizeof(stream));
	sp->fptr = fp;
	sp->filename = strdup(filename);
	sp->mode = strdup(mode);
	sp->type = strdup(type);
	node *n = make_stream(sp);
	n->flags |= FLAG_FILE;
	put_env(q, q->c.curr_frame + term3->slot, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_iso_open_4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	node *term4 = get_atom_or_list(term4);
	const char *filename = VAL_S(term1);
	const char *mode = VAL_S(term2);
	const char *type = "text";
	char tmpbuf[40];
	strcpy(tmpbuf, !strcmp(mode, "append") ? "a" : !strcmp(mode, "update") ? "r+" : !strcmp(mode, "write") ? "w+" : "r");
	node *l = term4;

	if (is_list(l)) {
		node *opt = term_firstarg(l);

		if (is_atom(opt)) {
			if (!strcmp(VAL_S(opt), "type(binary)")) {
				strcat(tmpbuf, "b");
				type = "binary";
			}
		}

		l = term_next(l);
	}

	FILE *fp = fopen(filename, tmpbuf);

	if (!fp) {
		QABORT(ABORT_NOTEXISTFILE);
		return 0;
	}

	stream *sp = calloc(1, sizeof(stream));
	sp->fptr = fp;
	sp->filename = strdup(filename);
	sp->mode = strdup(mode);
	sp->type = strdup(type);
	node *n = make_stream(sp);
	n->flags |= FLAG_FILE;
	put_env(q, q->c.curr_frame + term3->slot, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_iso_close(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	stream *sp = term1->val_str;

#ifndef ISO_ONLY
	if (is_socket(term1)) {
		term1->flags &= ~FLAG_SOCKET;
		session_close((session *)sp->sptr);
	}
	else
#endif
	    if (is_file(term1)) {
		term1->flags &= ~FLAG_FILE;
		fclose(sp->fptr);
	}

	term1->flags &= ~FLAG_STREAM;
	free(term1->val_str);
	term1->val_str = NULL;
	return 1;
}

static FILE *get_output_stream(node *n)
{
	if (is_stream(n))
		return n->val_str->fptr;

	if (!strcmp(VAL_S(n), "user_error"))
		return stderr;

	if (!strcmp(VAL_S(n), "user_output"))
		return stdout;

	return stdout;
}

static FILE *get_input_stream(node *n)
{
	if (is_stream(n))
		return n->val_str->fptr;

	if (!strcmp(VAL_S(n), "user_input"))
		return stdin;

	return stdin;
}

static int bif_iso_write_term_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	node *term3 = get_atom_or_list(term3);
	int quoted = 0, nl = 0, fs = 0;

	if (is_atom(term2)) {
		if (strcmp(VAL_S(term2), "[]"))
			return 0;
	}
	else {
		char tmpbuf[1024];
		term_sprint(tmpbuf, sizeof(tmpbuf), q->pl, q, term2, 0);

		if (strstr(tmpbuf, "quoted(true)"))
			quoted = 1;

		if (strstr(tmpbuf, "fullstop(true)"))
			fs = 1;

		if (strstr(tmpbuf, "nl(true)"))
			nl = 1;

		if (strstr(tmpbuf, "ignore_ops(true)"))
			q->ignore_ops = 1;
	}

	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, quoted);
	q->ignore_ops = 0;

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	if (fs) {
		tmpbuf[len++] = '.';
		tmpbuf[len] = '\0';
	}

	if (nl) {
		tmpbuf[len++] = '\n';
		tmpbuf[len] = '\0';
	}

	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	free(tmpbuf);
	return ok >= 0;
}

static int bif_iso_write_term(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_atom_or_list(term2);
	int quoted = 0, nl = 0, fs = 0;

	if (is_atom(term2)) {
		if (strcmp(VAL_S(term2), "[]"))
			return 0;
	}
	else {
		char tmpbuf[1024];
		term_sprint(tmpbuf, sizeof(tmpbuf), q->pl, q, term2, 0);

		if (strstr(tmpbuf, "quoted(true)"))
			quoted = 1;

		if (strstr(tmpbuf, "fullstop(true)"))
			fs = 1;

		if (strstr(tmpbuf, "nl(true)"))
			nl = 1;

		if (strstr(tmpbuf, "ignore_ops(true)"))
			q->ignore_ops = 1;
	}

	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, quoted);
	q->ignore_ops = 0;

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	if (fs) {
		tmpbuf[len++] = '.';
		tmpbuf[len] = '\0';
	}

	if (nl) {
		tmpbuf[len++] = '\n';
		tmpbuf[len] = '\0';
	}

	fwrite(tmpbuf, 1, len, q->curr_stdout);
	free(tmpbuf);
	return 1;
}

static int bif_iso_write_canonical_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 2);

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	free(tmpbuf);
	return ok >= 0;
}

static int bif_iso_write_canonical(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	term_print(q->pl, q, term1, 2);

	if (q->halt)
		return 0;

	return 1;
}

static int bif_iso_writeq_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 1);

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	free(tmpbuf);
	return ok >= 0;
}

static int bif_iso_writeq(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	term_print(q->pl, q, term1, 1);

	if (q->halt)
		return 0;

	return 1;
}

static int bif_iso_write_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	char *tmpbuf;
	size_t len;

	if (is_atom(term2)) {
		tmpbuf = VAL_S(term2);
		len = LEN(term2);
	}
	else {
		size_t max_len = PRINTBUF_SIZE;
		tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 0);

		if (q->halt) {
			free(tmpbuf);
			return 0;
		}
	}

	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	if (!is_atom(term2))
		free(tmpbuf);

	return ok >= 0;
}

static int bif_iso_write(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	char *tmpbuf;
	size_t len;

	if (is_atom(term1)) {
		tmpbuf = VAL_S(term1);
		len = LEN(term1);
	}
	else {
		size_t max_len = PRINTBUF_SIZE;
		tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);

		if (q->halt) {
			free(tmpbuf);
			return 0;
		}
	}

	fwrite(tmpbuf, 1, len, q->curr_stdout);

	if (!is_atom(term1))
		free(tmpbuf);

	return 1;
}

static int bif_iso_nl_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, "\n", 1);
	else
#endif
		ok = fwrite("\n", 1, 1, get_output_stream(term1));

	return ok > 0;
}

static int bif_iso_nl(tpl_query *q)
{
	fwrite("\n", 1, 1, q->curr_stdout);
	return 1;
}

static int bif_read_term(tpl_query *q, char *line, node *term1, node *opts)
{
	if (!*line)
		return 0;

	char *tmpbuf = (char *)malloc(strlen(line)+10);
	node *term = NULL, *save_term;
	int clause = 0;

	if (strstr(line, ":-")) {
		sprintf(tmpbuf, "%s", line);
		clause = 1;
	}
	else
		sprintf(tmpbuf, "?- %s", line);

	lexer l;
	lexer_init(&l, q->pl);
	l.fp = q->curr_stdin;
	lexer_parse(&l, l.r, tmpbuf, &tmpbuf);
	save_term = term = NLIST_FRONT(&l.val_l);
	free(tmpbuf);

	if (l.error) {
		printf("ERROR: error make_rule: %s\n", line);
		lexer_done(&l);
		return 0;
	}

	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	int cnt = collect_vars(q, term);
	sl_clear(&vars, NULL);

	if (cnt) {
		expand_frame(q, cnt);
		node *tmp = copy_term(q, term);
		term_heapcheck(term);
		save_term = term = tmp;
	}

	sl_done(&vars, NULL);
	q->d = NULL;
	lexer_done(&l);
	term = clause ? term : term_firstarg(term);
	int ok = unify_term(q, term1, term, q->c.env_point);
	term_heapcheck(save_term);
	return ok;
}

static int bif_iso_read_term_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_next_arg(q, &args);
	char *line;

	if (term2) {
		if (is_atom(term2) && strcmp(VAL_S(term2), "[]")) {
			QABORT(ABORT_INVALIDARGNOTLIST);
			return 0;
		}
		else if (!is_atom(term2) && !is_list(term2)) {
			QABORT(ABORT_INVALIDARGNOTLIST);
			return 0;
		}
	}

	if (!(line = trealla_readline(q->lex, q->curr_stdin, 1)))
		return unify_const_atom(q, term1, END_OF_FILE);

	int ok = bif_read_term(q, line, term1, term2);
	free(line);
	return ok;
}

static int bif_iso_read_term_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	node *term3 = get_next_arg(q, &args);
	stream *sp = term1->val_str;
	char *line = NULL;

	if (term3) {
		if (is_atom(term3) && strcmp(VAL_S(term3), "[]")) {
			QABORT(ABORT_INVALIDARGNOTLIST);
			return 0;
		}
		else if (!is_atom(term3) && !is_list(term3)) {
			QABORT(ABORT_INVALIDARGNOTLIST);
			return 0;
		}
	}

#ifndef ISO_ONLY
	if (is_socket(term1)) {
		if (!session_readmsg((session *)sp->sptr, &line)) {
			q->is_yielded = 1;
			return 0;
		}

		if (session_on_disconnect((session *)sp->sptr))
			return unify_const_atom(q, term2, END_OF_FILE);
	}
	else
#endif
	{
		if (!(line = trealla_readline(q->lex, get_input_stream(term1), 1)))
			return unify_const_atom(q, term2, END_OF_FILE);
	}

	int ok = bif_read_term(q, line, term2, term3);
	free(line);
	return ok;
}

static int bif_iso_flush_output_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	fflush(get_output_stream(term1));
	return 1;
}

static int bif_iso_flush_output(tpl_query *q)
{
	fflush(q->curr_stdout);
	return 1;
}

static int bif_iso_at_end_of_stream_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	stream *sp = term1->val_str;

#ifndef ISO_ONLY
	if (is_socket(term1) && sp->sptr)
		return session_on_disconnect((session *)sp->sptr);
	else
#endif
	    if (is_file(term1) && sp->fptr)
		return feof(sp->fptr) > 0;

	return 0;
}

static int bif_iso_at_end_of_stream(tpl_query *q) { return feof(q->curr_stdin) > 0; }

static int bif_iso_set_stream_position(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_int(term2);
	stream *sp = term1->val_str;
	return !fseeko(sp->fptr, term2->val_i, SEEK_SET);
}

static int bif_iso_stream_property_position(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_int(q, q->c.curr_frame + term2->slot, ftello(sp->fptr));
	return 1;
}

static int bif_iso_stream_property_file_name(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->c.curr_frame + term2->slot, strdup(sp->filename));
	return 1;
}

static int bif_iso_stream_property_mode(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->c.curr_frame + term2->slot, strdup(sp->mode));
	return 1;
}

static int bif_iso_stream_property_type(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->c.curr_frame + term2->slot, strdup(sp->type));
	return 1;
}

static int bif_iso_put_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *src = VAL_S(term1);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	int len = put_char_utf8(tmpbuf, ch);
	fwrite(tmpbuf, 1, len, q->curr_stdout);
	return 1;
}

static int bif_iso_put_char_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_atom(term2);
	const char *src = VAL_S(term2);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	int len = put_char_utf8(tmpbuf, ch);
	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	return ok > 0;
}

static int bif_iso_put_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	char tmpbuf[20];
	sprintf(tmpbuf, "%c", (int)term1->val_i);
	fwrite(tmpbuf, 1, 1, q->curr_stdout);
	return 1;
}

static int bif_iso_put_byte_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int(term2);
	int ok;
	char tmpbuf[20];
	sprintf(tmpbuf, "%c", (int)term1->val_i);

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, 1);
	else
#endif
		ok = fwrite(tmpbuf, 1, 1, get_output_stream(term1));

	return ok > 0;
}

static int bif_iso_put_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	char tmpbuf[20];
	int len = put_char_utf8(tmpbuf, term1->val_i);
	fwrite(tmpbuf, 1, len, q->curr_stdout);
	return 1;
}

static int bif_iso_put_code_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int(term2);
	char tmpbuf[20];
	int len = put_char_utf8(tmpbuf, term2->val_i);
	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	return ok > 0;
}

static int bif_iso_get_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

	q->did_getc = 1;
	int ch = getc_utf8(q->curr_stdin);

	if (ch == EOF)
		q->did_getc = 0;
	else if (ch == '\n')
		q->did_getc = 0;

	return unify_int(q, term1, ch);
}

static int bif_iso_get_code_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int_or_var(term2);
	int ch = getc_utf8(get_input_stream(term1));
	return unify_int(q, term2, ch);
}

static int bif_iso_get_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

	int ch = fgetc(q->curr_stdin);
	return unify_int(q, term1, ch);
}

static int bif_iso_get_byte_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int_or_var(term2);
	int ch = fgetc(get_input_stream(term1));
	return unify_int(q, term2, ch);
}

static int bif_iso_get_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

	q->did_getc = 1;
	int ch = getc_utf8(q->curr_stdin);

	if (ch == EOF) {
		int ok = unify_const_atom(q, term1, END_OF_FILE);
		q->did_getc = 0;
		return ok;
	}

	if (ch == '\n')
		q->did_getc = 0;

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	return unify_atom(q, term1, strdup(tmpbuf));
}

static int bif_iso_get_char_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_atom_or_var(term2);
	int ch = getc_utf8(get_input_stream(term1));

	if (ch == EOF)
		return unify_const_atom(q, term2, END_OF_FILE);

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	return unify_atom(q, term2, strdup(tmpbuf));
}

static int bif_iso_peek_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);
	int ch = fgetc(q->curr_stdin);

	if (ch == EOF)
		return 0;

	ungetc(ch, q->curr_stdin); // FIXME
	return unify_int(q, term1, ch);
}

static int bif_iso_peek_code_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int_or_var(term2);
	int ch = fgetc(get_input_stream(term1));

	if (ch == EOF)
		return 0;

	ungetc(ch, get_input_stream(term1)); // FIXME
	return unify_int(q, term2, ch);
}

static int bif_iso_peek_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	int ch = fgetc(q->curr_stdin);

	if (ch == EOF)
		return 0;

	ungetc(ch, q->curr_stdin); // FIXME
	char tmpbuf[2];
	tmpbuf[0] = (char)ch;
	tmpbuf[1] = '\0';
	return unify_atom(q, term1, strdup(tmpbuf));
}

static int bif_iso_peek_byte_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_atom_or_var(term2);
	int ch = fgetc(get_input_stream(term1));

	if (ch == EOF)
		return 0;

	ungetc(ch, get_input_stream(term1)); // FIXME
	char tmpbuf[2];
	tmpbuf[0] = (char)ch;
	tmpbuf[1] = '\0';
	return unify_atom(q, term1, strdup(tmpbuf));
}

static int bif_iso_peek_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	int ch = getc_utf8(q->curr_stdin);

	if (ch == EOF)
		return unify_const_atom(q, term1, END_OF_FILE);

	ungetc(ch, q->curr_stdin); // FIXME
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	return unify_atom(q, term1, strdup(tmpbuf));
}

static int bif_iso_peek_char_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_atom_or_var(term2);
	int ch = getc_utf8(get_input_stream(term1));

	if (ch == EOF)
		return unify_const_atom(q, term2, END_OF_FILE);

	ungetc(ch, get_input_stream(term1)); // FIXME
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	return unify_atom(q, term2, strdup(tmpbuf));
}

static int bif_iso_number_codes(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nbr_or_var(term1);
	int save_context = q->latest_context;
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_list(term2)) {
		int is_real = 0;
		node *l = term2;
		char tmpbuf[FUNCTOR_SIZE + 10];
		char *dst = tmpbuf;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, this_context);

			if (!is_integer(n) && !is_bignum(n)) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			if ((dst - tmpbuf) == FUNCTOR_SIZE) {
				QABORT(ABORT_ARGTOOBIG);
				return 0;
			}

			int i = get_word(n);

			if (i == '.')
				is_real = 1;
			else {
				i -= '0';

				if ((i < 0) || (i > 9)) {
					QABORT(ABORT_INVALIDARGNOTINT);
					return 0;
				}
			}

			*dst++ = (char)(int)get_word(n);
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
		}

		*dst = '\0';
		q->curr_context = save_context;
		int ok;

		if (is_real)
			ok = unify_float(q, term1, atof(tmpbuf));
		else
			ok = unify_int(q, term1, atoll(tmpbuf));

		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	char tmpbuf[FUNCTOR_SIZE + 10];
	term_sprint(tmpbuf, sizeof(tmpbuf), q->pl, q, term1, 1);
	const char *src = tmpbuf;

	while (*src) {
		node *tmp = make_int((int)*src);
		term_append(l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	term_append(l, make_const_atom("[]"));
	int ok = unify_term(q, term2, save_l, -1);
	term_heapcheck(save_l);
	return ok;
}

static int bif_iso_number_chars(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nbr_or_var(term1);
	int save_context = q->latest_context;
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_list(term2)) {
		int is_real = 0;
		node *l = term2;
		char tmpbuf[FUNCTOR_SIZE + 10];
		char *dst = tmpbuf;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, this_context);

			if (!is_atom(n)) {
				QABORT(ABORT_INVALIDARGNOTATOM);
				return 0;
			}

			if ((dst - tmpbuf) == FUNCTOR_SIZE) {
				QABORT(ABORT_ARGTOOBIG);
				return 0;
			}

			if (VAL_S(n)[0] == '.')
				is_real = 1;
			else {
				char i = VAL_S(n)[0] - '0';

				if ((i < 0) || (i > 9)) {
					QABORT(ABORT_INVALIDARGNOTINT);
					return 0;
				}
			}

			*dst++ = VAL_S(n)[0];
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
		}

		*dst = '\0';
		q->curr_context = save_context;
		int ok;

		if (is_real)
			ok = unify_float(q, term1, atof(tmpbuf));
		else
			ok = unify_int(q, term1, atoll(tmpbuf));

		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	char tmpbuf[FUNCTOR_SIZE + 10];
	term_sprint(tmpbuf, sizeof(tmpbuf), q->pl, q, term1, 1);
	const char *src = tmpbuf;

	while (*src) {
		node *tmp = make_atom(strndup(src, 1));
		term_append(l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	term_append(l, make_const_atom("[]"));
	int ok = unify_term(q, term2, save_l, -1);
	term_heapcheck(save_l);
	return ok;
}

static int bif_iso_atom_chars(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_list(term2)) {
		size_t buflen = FUNCTOR_SIZE;
		char *dstbuf = malloc(buflen);
		char *dst = dstbuf;
		node *l = term2;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, this_context);

			if (!is_atom(n)) {
				QABORT(ABORT_INVALIDARGNOTATOM);
				return 0;
			}

			size_t save_len = dst - dstbuf;
			size_t len2 = LEN(n) + 1;

			if ((buflen - save_len) < len2) {
				buflen *= 2;
				buflen += len2;
				dstbuf = realloc(dstbuf, buflen);
				dst = dstbuf + save_len;
			}

			const char *src = VAL_S(n);
			int ch = get_char_utf8(&src);
			dst += put_char_utf8(dst, ch);
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
		}

		*dst = '\0';
		int ok = unify_atom(q, term1, strdup(dstbuf));
		free(dstbuf);
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	const char *src = VAL_S(term1);

	while (*src) {
		int ch = get_char_utf8(&src);
		char tmpbuf[20];
		put_char_utf8(tmpbuf, ch);
		node *tmp = make_atom(strdup(tmpbuf));
		term_append(l, tmp);

		if (!*src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	term_append(l, make_const_atom("[]"));
	int ok = unify_term(q, term2, save_l, -1);
	term_heapcheck(save_l);
	return ok;
}

static int bif_iso_atom_codes(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_list(term2)) {
		size_t buflen = FUNCTOR_SIZE;
		char *dstbuf = malloc(buflen);
		char *dst = dstbuf;
		node *l = term2;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, this_context);

			if (!is_integer(n) && !is_bignum(n)) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			nbr_t v = get_word(n);

			if (v <= 0) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			size_t save_len = dst - dstbuf;
			size_t len2 = 8; // Allow for utf8 char

			if ((buflen - save_len) < len2) {
				buflen *= 2;
				buflen += len2;
				dstbuf = realloc(dstbuf, buflen);
				dst = dstbuf + save_len;
			}

			dst += put_char_utf8(dst, v);
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
		}

		*dst = '\0';
		int ok = unify_atom(q, term1, strdup(dstbuf));
		free(dstbuf);
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	const char *src = VAL_S(term1);

	while (*src) {
		int ch = get_char_utf8(&src);
		node *tmp = make_int(ch);
		term_append(l, tmp);

		if (!*src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	term_append(l, make_const_atom("[]"));
	int ok = unify_term(q, term2, save_l, -1);
	term_heapcheck(save_l);
	return ok;
}

static int bif_iso_char_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_int_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_integer(term2)) {
		char tmpbuf[2];
		tmpbuf[0] = (char)term2->val_i;
		tmpbuf[1] = '\0';
		int ok = unify_atom(q, term1, strdup(tmpbuf));
		return ok;
	}

	return unify_int(q, term2, VAL_S(term1)[0]);
}

static int bif_iso_set_input(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	stream *sp = term1->val_str;
	q->curr_stdin = sp->fptr;
	return 1;
}

static int bif_iso_set_output(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	stream *sp = term1->val_str;
	q->curr_stdout = sp->fptr;
	return 1;
}

static int bif_iso_current_input(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	stream *sp = calloc(1, sizeof(stream));
	sp->fptr = q->curr_stdin;
	sp->filename = strdup("stdin");
	sp->mode = strdup("read");
	sp->type = strdup("text");
	node *tmp = make_stream(sp);
	tmp->flags |= FLAG_FILE;
	put_env(q, q->c.curr_frame + term1->slot, tmp, -1);
	term_heapcheck(tmp);
	return 1;
}

static int bif_iso_current_output(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	stream *sp = calloc(1, sizeof(stream));
	sp->fptr = q->curr_stdout;
	sp->filename = strdup("stdout");
	sp->mode = strdup("append");
	sp->type = strdup("text");
	node *tmp = make_stream(sp);
	tmp->flags |= FLAG_FILE;
	put_env(q, q->c.curr_frame + term1->slot, tmp, -1);
	term_heapcheck(tmp);
	return 1;
}

static int bif_iso_copy_term(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	unsigned context1 = q->latest_context;
	node *term2 = get_var(term2);
	q->latest_context = context1;
	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	int cnt = collect_vars(q, term1);
	sl_clear(&vars, NULL);

	if (cnt) {
		if (!expand_frame(q, cnt))
			return 0;
	}

	node *tmp = copy_term(q, term1);
	sl_done(&vars, NULL);
	q->d = NULL;
	put_env(q, q->c.curr_frame + term2->slot, tmp, is_compound(tmp) ? q->c.curr_frame : -1);
	term_heapcheck(tmp);
	return 1;
}

static void rebase(lexer *l, node *term)
{
	for (node *n = term_first(term); n; n = term_next(n)) {
		if (is_compound(n))
			rebase(l, n);
		else if (l && is_var(n))
			attach_vars(l, n);
	}
}

int bif_asserta(tpl_query *q, node *n)
{
	int persist;
	lexer l;
	lexer_init(&l, q->pl);
	l.db = q->c.curr_db;
	asserta_index(&l, n, 1, &persist, q->in_tran);
	lexer_done(&l);
	return persist;
}

int bif_iso_asserta(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = term_functor(term1);
	else if (!is_builtin(term1))
		functor = VAL_S(term1);
	else
		return 0;

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTA;
	n->cpos = q->c.curr_term->cpos;

#ifndef ISO_ONLY
	if (q->in_tran) {
		node *tmp = term_make();
		tmp->n1 = n;
		NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
	}
	else
#endif
	{
		DBLOCK(q->c.curr_db);
		bif_asserta(q, n);
		DBUNLOCK(q->c.curr_db);
	}

	return 1;
}

int bif_assertz(tpl_query *q, node *n)
{
	int persist;
	lexer l;
	lexer_init(&l, q->pl);
	l.db = q->c.curr_db;
	assertz_index(&l, n, 1, &persist, q->in_tran);
	lexer_done(&l);
	return persist;
}

int bif_iso_assertz(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = term_functor(term1);
	else if (!is_builtin(term1))
		functor = VAL_S(term1);
	else
		return 0;

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTZ;
	n->cpos = q->c.curr_term->cpos;

#ifndef ISO_ONLY
	if (q->in_tran) {
		node *tmp = term_make();
		tmp->n1 = n;
		NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
	}
	else
#endif
	{
		DBLOCK(q->c.curr_db);
		bif_assertz(q, n);
		DBUNLOCK(q->c.curr_db);
	}

	return 1;
}

int bif_retract(tpl_query *q, node *n, node *n2)
{
	int persist;
	lexer l;
	lexer_init(&l, q->pl);
	l.db = q->c.curr_db;
	retract_index(&l, n, n2, &persist, q->in_tran);
	lexer_done(&l);

	// if (!(n2->flags & FLAG_DBS_RETRACTALL))
	//	term_heapcheck(n2);

	return persist;
}

static int bif_retract2(tpl_query *q, int wait)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	unsigned context1 = q->latest_context;
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = term_functor(term1);
	else if (!is_builtin(term1))
		functor = VAL_S(term1);
	else
		return 0;

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	node *save_n = n;
	node *head = term_firstarg(n);
	node *tmp = head;
	int arity = 0;

	if (is_compound(head)) {
		arity = term_arity(head);
		tmp = term_first(head);
	}

	functor = VAL_S(tmp);
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	rule *r = NULL;
	int did_lock = 0;

	if (!q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		return 0;
	}

	if (!r->dynamic) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	node *match = NULL;
	const void *key = NULL;
	char tmpbuf2[KEY_SIZE + 10];
	sbiter *idx_iter; // First-arg iterator
	int use_iter = 0;

	if (r->idx) {
		node *fa = term_firstarg(term1);
		node *fval = get_arg(q, fa, q->curr_context);

		if (!is_var(fval)) {
			use_iter = 1;

#ifndef ISO_ONLY
			if (r->numeric) {
				if (!is_integer(fval)) {
					printf("ERROR: index type mismatch\n");
					return 0;
				}

				key = (void *)(size_t)fval->val_i;
			}
			else
#endif
				key = make_key(q->pl, tmpbuf2, fval);

			idx_iter = sb_findkey(r->idx, key);

			if (!sb_nextkey(idx_iter, key, (void **)&match))
				match = NULL;
		}
		else
			match = NLIST_FRONT(&r->val_l);
	}
	else
		match = NLIST_FRONT(&r->val_l);

	if (!q->retry)
		allocate_frame(q);

	while (match) {
		if (is_hidden(match) || is_deleted(match)) {
			if (use_iter) {
				if (!sb_nextkey(idx_iter, key, (void **)&match))
					break;
			}
			else
				match = term_next(match);

			continue;
		}

		node *save_match = match, *save_head = NULL;

#ifndef ISO_ONLY
		if (is_storage(match)) {
			node *head = term_firstarg(match);
			node *tmp_arg1 = term_firstarg(head);
			node *tmp_rest = term_next(tmp_arg1);
			save_head = dbs_read_entry(q->c.curr_db, tmp_rest->val_i);

			if (!save_head) {
				printf("ERROR: accessing %s storage fpos=%lld", q->c.curr_db->name, (long long)tmp_rest->val_i);
				break;
			}

			match = term_firstarg(save_head);
			n = term_firstarg(save_n);
		}
#endif

		// printf("*** (%u) ", q->c.frame_size); term_print(q->pl, NULL,
		// n, 0);
		// printf(" <==> "); term_print(q->pl, NULL, match, 0); printf("
		// (%u)\n",
		// q->c.frame_size);

		int ok = unify(q, n, context1, match, q->c.env_point);

		if (save_head)
			term_heapcheck(save_head);

		if (!ok) {
			reallocate_frame(q);

			if (use_iter) {
				if (!sb_nextkey(idx_iter, key, (void **)&match))
					break;
			}
			else
				match = term_next(save_match);

			continue;
		}

		save_match->flags |= FLAG_DBS_RETRACT | FLAG_DELETED;
		save_n->flags |= FLAG_DBS_RETRACT;

		if (term_next(match) || wait)
			try_me_nofollow(q);

#ifndef ISO_ONLY
		if (q->in_tran) {
			node *tmp = term_make();
			tmp->n1 = save_match;
			tmp->n2 = save_n;
			NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
		}
		else
#endif
			bif_retract(q, save_match, save_n);

		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		return 1;
	}

	term_heapcheck(save_n);

	if (!wait) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		return 0;
	}

#ifndef ISO_ONLY
	sl_set(&r->procs, (const char *)q, NULL);

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	PIDLOCK(q->pl);

	if (q->tmo_msecs > 0) {
		q->tmo_when_msecs = gettimeofday_usec() / 1000;
		q->tmo_when_msecs += q->tmo_msecs;
		q->is_idle = 1;
		sl_set(&q->pl->idle, (const char *)q, NULL);
	}

	return process_yield_locked(q);
#else
	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	return 0;
#endif
}

int bif_iso_retract(tpl_query *q) { return bif_retract2(q, 0); }

#ifndef ISO_ONLY
static int bif_xtra_retractw(tpl_query *q) { return bif_retract2(q, 1); }
#endif

static int bif_iso_retractall(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = VAL_S(term_first(term1));
	else
		functor = VAL_S(term1);

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	node *save_n = n;
	node *head = term_firstarg(n);
	node *tmp = head;
	int arity = 0;

	if (is_compound(head)) {
		arity = term_arity(head);
		tmp = term_first(head);
	}

	functor = VAL_S(tmp);
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	rule *r = NULL;
	int did_lock = 0;

	if (!q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		return 1;
	}

	if (!r->dynamic) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		QABORT(ABORT_NOTDYNAMIC);
		return 1;
	}

	if (!NLIST_COUNT(&r->val_l)) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		term_heapcheck(save_n);
		return 1;
	}

	node *match = NULL;
	const void *key = NULL;
	char tmpbuf2[KEY_SIZE + 10];
	sbiter *idx_iter; // First-arg iterator
	int use_iter = 0;

	if (r->idx) {
		node *fa = term_firstarg(term1);
		node *fval = get_arg(q, fa, q->curr_context);

		if (!is_var(fval)) {
			use_iter = 1;

#ifndef ISO_ONLY
			if (r->numeric) {
				if (!is_integer(fval)) {
					printf("ERROR: index type mismatch\n");
					return 0;
				}

				key = (void *)(size_t)fval->val_i;
			}
			else
#endif
				key = make_key(q->pl, tmpbuf2, fval);

			idx_iter = sb_findkey(r->idx, key);

			if (!sb_nextkey(idx_iter, key, (void **)&match))
				match = NULL;
		}
		else
			match = NLIST_FRONT(&r->val_l);
	}
	else
		match = NLIST_FRONT(&r->val_l);

	allocate_frame(q);

	while (match) {
		if (is_hidden(match) || is_deleted(match)) {
			if (use_iter) {
				if (!sb_nextkey(idx_iter, key, (void **)&match))
					break;
			}
			else
				match = term_next(match);

			continue;
		}

		node *save_next = term_next(match);
		node *save_match = match, *save_head = NULL;

#ifndef ISO_ONLY
		if (is_storage(match)) {
			node *head = term_firstarg(match);
			node *tmp_arg1 = term_firstarg(head);
			node *tmp_rest = term_next(tmp_arg1);
			save_head = dbs_read_entry(q->c.curr_db, tmp_rest->val_i);

			if (!save_head) {
				printf("ERROR: accessing %s storage fpos=%lld", q->c.curr_db->name, (long long)tmp_rest->val_i);
				break;
			}

			match = term_firstarg(save_head);
			n = term_firstarg(save_n);
		}
#endif

		if (unify_term(q, n, match, q->c.curr_frame)) {
			match->flags |= FLAG_DBS_RETRACT;
			save_match->flags |= FLAG_DBS_RETRACT | FLAG_DELETED;
			save_n->flags |= FLAG_DBS_RETRACT | FLAG_DBS_RETRACTALL;

#ifndef ISO_ONLY
			if (q->in_tran) {
				node *tmp = term_make();
				tmp->n1 = save_match;
				tmp->n2 = save_n;
				NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
			}
			else
#endif
				bif_retract(q, save_match, save_n);
		}

		if (save_head)
			term_heapcheck(save_head);

		if (use_iter) {
			if (!sb_nextkey(idx_iter, key, (void **)&match))
				break;
		}
		else
			match = save_next;

		reallocate_frame(q);
	}

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	term_heapcheck(save_n);
	return 1;
}

static int bif_iso_abolish(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_structure(term1);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 1);
	printf("DEBUG: *** %s\n", tmpbuf);
	rule *r = NULL;
	int did_lock = 0;

	if (!q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		return 1;
	}

	if (!r->dynamic) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		QABORT(ABORT_NOTDYNAMIC);
		return 1;
	}

	for (node *match = NLIST_FRONT(&r->val_l); match; match = term_next(match)) {
		if (is_fact(match))
			term_heapcheck(match);
		else
			match->flags |= FLAG_DELETED;
	}

	if (sl_del(&q->c.curr_db->rules, tmpbuf, (void **)&r))
		free(r);

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	free(tmpbuf);
	return 1;
}

#ifndef ISO_ONLY
static int bif_xtra_asserta_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = term_functor(term1);
	else if (!is_builtin(term1))
		functor = VAL_S(term1);
	else
		return 0;

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTA;
	n->cpos = q->c.curr_term->cpos;

	if (q->in_tran) {
		node *tmp = term_make();
		tmp->n1 = n;
		NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
	}
	else {
		if (!q->in_tran)
			DBLOCK(q->c.curr_db);

		bif_asserta(q, n);

		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);
	}

	put_ptr(q, q->c.curr_frame + term2->slot, n);
	return 1;
}

static int bif_xtra_assertz_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = term_functor(term1);
	else if (!is_builtin(term1))
		functor = VAL_S(term1);
	else
		return 0;

	if (strcmp(functor, ":-") && strcmp(functor, "-->")) {
		n = make_compound();
		n->flags |= FLAG_CLAUSE | FLAG_FACT;
		term_append(n, make_const_atom(":-"));
		term_append(n, clone_term(q, term1));
		term_append(n, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTZ;
	n->cpos = q->c.curr_term->cpos;

	if (q->in_tran) {
		node *tmp = term_make();
		tmp->n1 = n;
		NLIST_PUSH_BACK(&q->c.curr_db->tran_queue, tmp);
	}
	else {
		if (!q->in_tran)
			DBLOCK(q->c.curr_db);

		bif_assertz(q, n);

		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);
	}

	put_ptr(q, q->c.curr_frame + term2->slot, n);
	return 1;
}

static int bif_xtra_erase_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_ptr(term1);
	node *n = term1->val_ptr;
	int did_lock = 0;

	if (!q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	bif_retract(q, n, n);

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	return 1;
}
#endif

static int bif_clause(tpl_query *q, int wait)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	unsigned context1 = q->latest_context;
	node *term2 = get_next_arg(q, &args);
	node *term3 = get_next_arg(q, &args);
#ifndef ISO_ONLY
	node *save_match = q->c.curr_match;
#endif
	node *head = NULL;
	rule *r = NULL;

	if (!q->retry) {
		const char *functor;
		int arity = 0;

		if (is_compound(term1)) {
			functor = term_functor(term1);
			arity = term_arity(term1);
		}
		else if (!is_builtin(term1))
			functor = VAL_S(term1);
		else
			return 0;

		char tmpbuf[FUNCTOR_SIZE + 10];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
		int did_lock = 0;

		if (!q->in_tran) {
			did_lock = 1;
			DBLOCK(q->c.curr_db);
		}

		if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
			if (did_lock)
				DBUNLOCK(q->c.curr_db);

			return 0;
		}

		if (!NLIST_COUNT(&r->val_l)) {
			if (did_lock)
				DBUNLOCK(q->c.curr_db);

#ifndef ISO_ONLY
			save_match = q->c.curr_match = NULL;
#endif
		}
#ifndef ISO_ONLY
		else
			save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);
#endif

		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		allocate_frame(q);
		q->curr_rule = r;
	}
	else {
		r = q->curr_rule;

		if (!q->c.curr_match)
#ifndef ISO_ONLY
			save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);
#else
			;
#endif
		else {
			r = q->curr_rule;
#ifndef ISO_ONLY
			save_match = q->c.curr_match;
#endif
			q->c.curr_match = term_next(q->c.curr_match);
		}
	}

	while (q->c.curr_match) {
		if (is_hidden(q->c.curr_match) || is_deleted(q->c.curr_match)) {
			q->c.curr_match = term_next(q->c.curr_match);
			continue;
		}

		head = term_firstarg(q->c.curr_match);
		unsigned frame_size = q->c.curr_match->frame_size;
		prepare_frame(q, frame_size);

		node *save_head = NULL;

#ifndef ISO_ONLY
		if (is_storage(q->c.curr_match)) {
			node *tmp_arg1 = term_firstarg(head);
			node *tmp_rest = term_next(tmp_arg1);
			save_head = dbs_read_entry(q->c.curr_db, tmp_rest->val_i);

			if (!save_head) {
				printf("ERROR: accessing %s storage fpos=%lld", q->c.curr_db->name, (long long)tmp_rest->val_i);
				break;
			}

			head = term_firstarg(save_head);
		}
#endif
		// printf("***(%u) ", q->c.frame_size); term_print(q->pl, NULL,
		// term1,
		// 0);
		// printf(" <==> "); term_print(q->pl, NULL, head, 0); printf("
		// (%u)\n",
		// q->c.curr_match->frame_size);

		if (!unify(q, term1, context1, head, q->c.env_point)) {
			if (save_head)
				term_heapcheck(save_head);

			reallocate_frame(q);
			q->c.curr_match = term_next(q->c.curr_match);
			continue;
		}

		if (term3 && is_ptr(term3) && (term3->val_ptr != q->c.curr_match)) {
			if (save_head)
				term_heapcheck(save_head);

			reallocate_frame(q);
			q->c.curr_match = term_next(q->c.curr_match);
			continue;
		}

		if (save_head)
			term_heapcheck(save_head);

		break;
	}

	if (!q->c.curr_match && !wait)
		return 0;

#ifndef ISO_ONLY
	int is_eof = !q->c.curr_match;

	if (is_eof && wait) {
		q->c.curr_match = save_match;
		try_me_nofollow(q);
		DBLOCK(q->c.curr_db);
		sl_set(&r->procs, (const char *)q, NULL);
		DBUNLOCK(q->c.curr_db);
		PIDLOCK(q->pl);

		if (q->tmo_msecs > 0) {
			q->tmo_when_msecs = gettimeofday_usec() / 1000;
			q->tmo_when_msecs += q->tmo_msecs;
			q->is_idle = 1;
			sl_set(&q->pl->idle, (const char *)q, NULL);
		}

		return process_yield_locked(q);
	}
#endif

	try_me_nofollow(q);

	if (term3 && is_var(term3))
		put_ptr(q, q->c.curr_frame + term3->slot, q->c.curr_match);

	if (!term2)
		return 1;

	node *body = term_next(head);
	return unify_term(q, term2, body, q->c.curr_frame);
}

static int bif_iso_clause(tpl_query *q) { return bif_clause(q, 0); }

#ifndef ISO_ONLY
static int bif_xtra_clausew(tpl_query *q) { return bif_clause(q, 1); }
#endif

static int nodecmp(const void *p1, const void *p2)
{
	node *term1 = *(node **)p1;
	node *term2 = *(node **)p2;

	if (is_integer(term1)) {
		if (is_integer(term2)) {
			if (term1->val_i < term2->val_i)
				return -1;

			if (term1->val_i > term2->val_i)
				return 1;

			return 0;
		}

		if (is_float(term2)) {
			if (term1->val_i < term2->val_f)
				return -1;

			if (term1->val_i > term2->val_f)
				return 1;

			return 0;
		}
	}
	else if (is_float(term1)) {
		if (is_integer(term2)) {
			if (term1->val_f < term2->val_i)
				return -1;

			if (term1->val_f > term2->val_i)
				return 1;

			return 0;
		}

		if (is_float(term2)) {
			if (term1->val_f < term2->val_f)
				return -1;

			if (term1->val_f > term2->val_f)
				return 1;

			return 0;
		}
	}
	else if (is_atom(term1) && is_atom(term2))
		return strcmp(VAL_S(term1), VAL_S(term2));
	else {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf1 = (char *)malloc(max_len + 1);
		char *dst = tmpbuf1;
		term_sprint2(&tmpbuf1, &max_len, &dst, NULL, NULL, term1, 0);
		max_len = PRINTBUF_SIZE;
		char *tmpbuf2 = (char *)malloc(max_len + 1);
		dst = tmpbuf2;
		term_sprint2(&tmpbuf2, &max_len, &dst, NULL, NULL, term2, 0);
		int ok = strcmp(tmpbuf1, tmpbuf2);
		free(tmpbuf1);
		free(tmpbuf2);
		return ok;
	}

	return 0;
}

static int bif_iso_sort(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	int save_context = q->latest_context;
	node *term2 = get_atom_or_list_or_var(term2);
	q->latest_context = save_context;

	if (is_list(term1)) {
		node *l = term1;
		size_t cnt = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			node *n = get_arg(q, head, q->latest_context);

			if (!is_atomic(n))
				return 0;

			cnt++;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node **base = (node **)malloc(sizeof(node *) * cnt);
		l = term1;
		q->latest_context = save_context;
		size_t idx = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			node *n = get_arg(q, head, q->latest_context);
			base[idx++] = n;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		qsort(base, cnt, sizeof(node *), nodecmp);
		l = make_list();
		node *tmp = l;

		for (int i = 0; i < cnt; i++) {
			if (i < (cnt - 1))
				if (!nodecmp(&base[i], &base[i + 1]))
					continue;

			term_append(tmp, clone_term(q, base[i]));

			if (i == (cnt - 1))
				break;

			node *tmp2;
			term_append(tmp, tmp2 = make_list());
			tmp = tmp2;
		}

		term_append(tmp, make_const_atom("[]"));
		free(base);
		int ok = unify_term(q, term2, l, q->c.curr_frame);
		term_heapcheck(l);
		return ok;
	}

	return unify_term(q, term2, term1, q->c.curr_frame);
}

static int keycmp(const void *p1, const void *p2)
{
	node *term1 = *(node **)p1;
	node *term2 = *(node **)p2;
	node *n1 = term_first(term1);
	node *n2 = term_first(term2);
	return nodecmp(&n1->hdr.next, &n2->hdr.next);
}

static int bif_iso_keysort(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	int save_context = q->latest_context;
	node *term2 = get_atom_or_list_or_var(term2);
	q->latest_context = save_context;

	if (is_list(term1)) {
		node *l = term1;
		size_t cnt = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			node *n = get_arg(q, head, q->latest_context);

			if (!is_compound(n))
				return 0;

			if ((term_arity(n) != 2) || !is_atom(term_first(n)) || strcmp(VAL_S(term_first(n)), "-"))
				return 0;

			cnt++;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node **base = (node **)malloc(sizeof(node *) * cnt);
		q->latest_context = save_context;
		l = term1;
		size_t idx = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			node *n = get_arg(q, head, q->latest_context);
			base[idx++] = n;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		qsort(base, cnt, sizeof(node *), keycmp);
		l = make_list();
		node *tmp = l;

		for (size_t i = 0; i < cnt; i++) {
			term_append(tmp, clone_term(q, base[i]));

			if (i == (cnt - 1))
				break;

			node *tmp2;
			term_append(tmp, tmp2 = make_list());
			tmp = tmp2;
		}

		term_append(tmp, make_const_atom("[]"));
		free(base);
		int ok = unify_term(q, term2, l, q->c.curr_frame);
		term_heapcheck(l);
		return ok;
	}

	return unify_term(q, term2, term1, q->c.curr_frame);
}

static int bif_iso_arg(tpl_query *q)
{
	node *args = get_args(q);
	node *orig_term1 = term_next(args);
	node *term1 = get_int_or_var(term1);
	node *term2 = get_compound(term2);
	unsigned save_context2 = q->latest_context;
	node *term3 = get_term(term3);

#if USE_SSL
	if (is_bignum(term1) && (BN_get_word(term1->val_bn) <= 0))
		return 0;
#endif

	if (is_integer(term1) && (term1->val_i <= 0))
		return 0;

	int idx;

	if (is_var(term1)) {
		idx = 1;
		put_int(q, q->c.curr_frame + orig_term1->slot, idx);
		allocate_frame(q);
	}
	else if (q->retry) {
		idx = get_word(term1) + 1;
		reset_arg(q, orig_term1, q->c.curr_frame);
		put_int(q, q->c.curr_frame + orig_term1->slot, idx);
	}
	else
		idx = get_word(term1);

	node *n = term_first(term2);
	n = term_next(n);

	for (int i = 1; (i < idx) && n; i++)
		n = term_next(n);

	if (!n)
		return 0;

	if (is_var(term1) || q->retry)
		try_me_nofollow(q);

	node *term = get_arg(q, n, save_context2);
	return unify_term(q, term, term3, save_context2);
}

static int bif_iso_univ(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	unsigned save_context = q->latest_context;
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGNOTLIST);
		return 0;
	}

	if (is_compound(term1)) {
		node *l = make_list();
		node *n = term_first(term1);
		node *save_l = l;

		while (n) {
			term_append(l, clone_term(q, n));

			if (!term_next(n))
				break;

			node *tmp = make_list();
			term_append(l, tmp);
			l = tmp;
			n = term_next(n);
		}

		term_append(l, make_const_atom("[]"));
		int ok = unify_term(q, term2, save_l, save_context);
		term_heapcheck(save_l);
		return ok;
	}

	if (is_atomic(term1)) {
		node *l = make_list();
		term_append(l, clone_term(q, term1));
		term_append(l, make_const_atom("[]"));
		int ok = unify_term(q, term2, l, q->c.curr_frame);
		term_heapcheck(l);
		return ok;
	}

	if (is_var(term1) && is_list(term2)) {
		node *s = make_compound();
		node *l = term2;
		int first = 1;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, q->latest_context);

			if (first && is_var(n)) {
				QABORT(ABORT_INVALIDARGNOTGROUNDED);
				term_heapcheck(s);
				return 0;
			}

			term_append(s, clone_term(q, n));
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
			first = 0;
		}

		if (term_arity(s) == 0)
			put_env(q, q->c.curr_frame + term1->slot, term_first(s), q->c.curr_frame);
		else
			put_env(q, q->c.curr_frame + term1->slot, s, q->c.curr_frame);

		term_heapcheck(s);
		return 1;
	}

	return 0;
}

static int bif_iso_functor(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *term3 = get_term(term3);
	node *nf = NULL;
	int arity = 0;

	if (is_atom(term2) && (is_integer(term3) || is_bignum(term3))) {
		nbr_t v = get_word(term3);

		if (v > 0) {
			if (is_var(term1)) {
				if (!expand_frame(q, v))
					return 0;

				node *s = make_compound();
				term_append(s, clone_term(q, term2));

				for (int i = 0; i < v; i++)
					term_append(s, make_var(q));

				put_env(q, q->c.curr_frame+term1->slot, s, q->c.curr_frame);
				term_heapcheck(s);
				return 1;
			}
			else {
				return !strcmp(term_functor(term1), VAL_S(term2)) && (term_arity(term1) == v);
			}
		}
		else if (v == 0)
			return unify_term(q, term1, term2, -1);

		return 0;
	}

	if (is_atomic(term1)) {
		nf = term1;
	}
	else if (is_compound(term1)) {
		nf = term_first(term1);
		arity = term_arity(term1);
	}
	else
		return 0;

	node *na = make_quick_int(arity);
	int ok1 = unify_term(q, nf, term2, q->c.curr_frame);
	int ok2 = unify_term(q, na, term3, q->c.curr_frame);
	term_heapcheck(na);
	return ok1 && ok2;
}

static int bif_iso_length(tpl_query *q)
{
	node *args = get_args(q);
	node *orig_term1 = term_next(args);
	node *term1 = get_term(term1);
	int save_context = q->latest_context;
	node *orig_term2 = term_next(args);
	node *term2 = get_int_or_var(term2);

	if (!is_var(term1) && !is_atom(term1) && !is_list(term1)) {
		QABORT(ABORT_INVALIDARGNOTVARORLIST);
		return 0;
	}

	if (is_atom(term1) && strcmp(VAL_S(term1), "[]")) {
		QABORT(ABORT_INVALIDARGNOTVARORLIST);
		return 0;
	}

	if (is_atom(term1))
		return unify_term(q, term2, make_quick_int(0), -1);

	if (is_list(term1) && !q->retry) {
		int cnt = 0;
		q->latest_context = save_context;
		node *l = term1;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			// node *n = get_arg(q, head, q->latest_context);
			cnt++;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		if (is_var(term2))
			put_int(q, q->c.curr_frame + term2->slot, cnt);
		else
			return term2->val_i == cnt;

		return 1;
	}

	if (is_list(term1))
		reset_arg(q, orig_term1, q->c.curr_frame);

	if (!is_var(orig_term1))
		return 0;

	if (is_var(term2)) {
		put_int(q, q->c.curr_frame + orig_term2->slot, 0);
		allocate_frame(q);
		try_me_nofollow(q);
		node *tmp = make_const_atom("[]");
		put_env(q, q->c.curr_frame + orig_term1->slot, tmp, -1);
		term_heapcheck(tmp);
		return 1;
	}

	int cnt = term2->val_i;

	if (q->retry) {
		reset_arg(q, orig_term2, q->c.curr_frame);
		put_int(q, q->c.curr_frame + orig_term2->slot, ++cnt);
		try_me_nofollow(q);
	}

	if (cnt == 0) {
		node *tmp = make_const_atom("[]");
		put_env(q, q->c.curr_frame + orig_term1->slot, tmp, -1);
		term_heapcheck(tmp);
		return 1;
	}

	node *l = make_list();
	node *save_l = l;

	if (!expand_frame(q, cnt)) {
		term_heapcheck(save_l);
		return 0;
	}

	for (int i = 0; i < cnt; i++) {
		term_append(l, make_var(q));

		if (i == (cnt - 1))
			break;

		node *tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	term_append(l, make_const_atom("[]"));
	put_env(q, q->c.curr_frame + orig_term1->slot, save_l, q->c.curr_frame);
	term_heapcheck(save_l);
	return 1;
}

static int bif_iso_term_variables(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_list_or_var(term2);

	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	int cnt = collect_vars(q, term1);

	if (q->halt)
		return 0;

	if (!cnt) {
		q->d = NULL;
		sl_done(&vars, NULL);
		return unify_const_atom(q, term2, "[]");
	}

	node *l = make_list();
	node *save_l = l;
	node *n;
	sl_start(&vars);

	while ((sl_next(&vars, (void **)&n)) != NULL) {
		term_append(l, copy_term(q, n));

		if (!vars.iter)
			break;

		node *tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	q->d = NULL;
	sl_done(&vars, NULL);
	term_append(l, make_const_atom("[]"));
	int ok = unify_term(q, term2, save_l, q->latest_context);
	term_heapcheck(save_l);
	return ok;
}

static int bif_iso_findall(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_var(term3);
	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	int did_lock = 0;

	if (is_dynamic(term2) && !q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	node *from = clone_term(q, term2);
	begin_query(subq, from);
	int ok = query_run(subq);
	node *acc = NULL, *end = NULL;

	while (ok && !g_abort) {
		node *from = get_arg(subq, term1, FUDGE_FACTOR);
		node *res = clone_term(subq, from);

		if (!end) {
			acc = make_list();
			term_append(acc, res);
			end = acc;
		}
		else {
			node *tmp = make_list();
			term_append(tmp, res);
			term_append(end, tmp);
			end = tmp;
		}

		ok = query_continue(subq);
	}

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	if (end)
		term_append(end, make_const_atom("[]"));
	else
		acc = make_const_atom("[]");

	put_env(q, q->c.curr_frame+term3->slot, acc, -1);
	term_heapcheck(acc);
	term_heapcheck(from);
	query_destroy(subq);
	return 1;
}

static int bif_iso_bagof(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_term(var); // FLAG_HIDDEN
	node *term1 = get_term(term1);
	node *term2 = get_structure(term2);
	node *term3 = get_term(term3);
	unsigned isfree[MAX_FRAME_SIZE] = {0};
	stream *sp;

	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	collect_vars(q, term1);
	node *n;
	sl_start(&vars);

	while ((sl_next(&vars, (void **)&n)) != NULL) {
		if (is_var(n))
			isfree[n->slot] = 1;
	}

	sl_clear(&vars, NULL);
	q->d = NULL;

	if (!q->retry) {
		if (!is_var(var)) {
			QABORT(ABORT_INVALIDARGNOTVAR);
			return 0;
		}

		sp = calloc(1, sizeof(stream));
		sp->kvs = malloc(sizeof(skiplist));
		sl_init(sp->kvs, 0, NULL, NULL);
		node *n = make_stream(sp);
		put_env(q, q->c.curr_frame + var->slot, n, -1);
		term_heapcheck(n);
		allocate_frame(q);
	}
	else
		sp = var->val_str;

	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	node *subqgoal, *term = term2;

	while (!strcmp(term_functor(term), "^")) {
		node *n = term_firstarg(term);

		if (is_var(n))
			isfree[n->slot] = 1;

		term = term_next(n);
	}

	subqgoal = term;
	int did_lock = 0;

	if (is_dynamic(term2) && !q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	begin_query(subq, subqgoal);
	int ok = query_run(subq);

	if (!ok) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		query_destroy(subq);
		return 0;
	}

	node *acc = NULL, *end = NULL;

	while (ok && !g_abort) {
		if (sl_get(sp->kvs, (void *)subq->c.curr_match, NULL)) {
			ok = query_continue(subq);
			continue;
		}

		sl_set(sp->kvs, (void *)subq->c.curr_match, NULL);
		node *from = get_arg(subq, term1, FUDGE_FACTOR);
		node *res = clone_term(subq, from);

		if (!end) {
			acc = make_list();
			term_append(acc, res);
			end = acc;
		}
		else {
			node *tmp = make_list();
			term_append(end, tmp);
			term_append(tmp, res);
			end = tmp;
		}

		for (unsigned i = 0; i < q->c.frame_size; i++) {
			if (!isfree[i])
				subq->pins |= 1 << i;
		}

		ok = query_continue(subq);
	}

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	if (end)
		term_append(end, make_const_atom("[]"));
	else
		acc = make_const_atom("[]");

	if (!is_atom(acc))
		ok = unify_term(q, term3, acc, -1);
	else
		ok = 0;

	if (ok)
		try_me(q);

	term_heapcheck(acc);
	query_destroy(subq);
	return ok;
}

static int bif_iso_setof(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_term(var); // FLAG_HIDDEN
	node *term1 = get_term(term1);
	node *term2 = get_structure(term2);
	node *term3 = get_term(term3);
	unsigned isfree[MAX_FRAME_SIZE] = {0};
	stream *sp;

	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	collect_vars(q, term1);
	node *n;
	sl_start(&vars);

	while ((sl_next(&vars, (void **)&n)) != NULL) {
		if (is_var(n))
			isfree[n->slot] = 1;
	}

	sl_clear(&vars, NULL);
	q->d = NULL;

	if (!q->retry) {
		if (!is_var(var)) {
			QABORT(ABORT_INVALIDARGNOTVAR);
			return 0;
		}

		sp = calloc(1, sizeof(stream));
		sp->kvs = malloc(sizeof(skiplist));
		sl_init(sp->kvs, 0, NULL, NULL);
		node *n = make_stream(sp);
		put_env(q, q->c.curr_frame + var->slot, n, -1);
		term_heapcheck(n);
		allocate_frame(q);
	}
	else
		sp = var->val_str;

	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	node *subqgoal, *term = term2;

	while (!strcmp(term_functor(term), "^")) {
		node *n = term_firstarg(term);

		if (is_var(n))
			isfree[n->slot] = 1;

		term = term_next(n);
	}

	subqgoal = term;
	int did_lock = 0;

	if (is_dynamic(term2) && !q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	begin_query(subq, subqgoal);
	int ok = query_run(subq);

	if (!ok) {
		if (did_lock)
			DBUNLOCK(q->c.curr_db);

		query_destroy(subq);
		return 0;
	}

	node *acc = NULL, *end = NULL;

	while (ok && !g_abort) {
		if (sl_get(sp->kvs, (void *)subq->c.curr_match, NULL)) {
			ok = query_continue(subq);
			continue;
		}

		sl_set(sp->kvs, (void *)subq->c.curr_match, NULL);
		node *from = get_arg(subq, term1, FUDGE_FACTOR);
		node *res = clone_term(subq, from);

		if (!end) {
			acc = make_list();
			term_append(acc, res);
			end = acc;
		}
		else {
			node *tmp = make_list();
			term_append(end, tmp);
			term_append(tmp, res);
			end = tmp;
		}

		for (unsigned i = 0; i < q->c.frame_size; i++) {
			if (!isfree[i])
				subq->pins |= 1 << i;
		}

		ok = query_continue(subq);
	}

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	if (end)
		term_append(end, make_const_atom("[]"));
	else
		acc = make_const_atom("[]");

	if (!is_atom(acc)) {
		node *l = acc;
		size_t cnt = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			cnt++;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node **base = (node **)malloc(sizeof(node *) * cnt);
		l = acc;
		// q->latest_context = save_context;
		size_t idx = 0;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			node *n = get_arg(q, head, q->latest_context);
			base[idx++] = n;
			node *tail = term_next(head);
			l = get_arg(q, tail, q->latest_context);
		}

		qsort(base, cnt, sizeof(node *), nodecmp);
		l = make_list();
		node *tmp = l;

		for (int i = 0; i < cnt; i++) {
			if (i < (cnt - 1))
				if (!nodecmp(&base[i], &base[i + 1]))
					continue;

			term_append(tmp, clone_term(q, base[i]));

			if (i == (cnt - 1))
				break;

			node *tmp2;
			term_append(tmp, tmp2 = make_list());
			tmp = tmp2;
		}

		term_append(tmp, make_const_atom("[]"));
		free(base);
		ok = unify_term(q, term3, l, -1);
		term_heapcheck(l);
	}
	else
		ok = 0;

	if (ok)
		try_me(q);

	term_heapcheck(acc);
	query_destroy(subq);
	return ok;
}

static int bif_iso_sub_atom(tpl_query *q) // NOT YET IMPLEMENTED
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *term3 = get_term(term3);
	node *term4 = get_term(term4);
	node *term5 = get_term(term5);
	return 0;
}

static void eval_nbr(tpl_query *q, const node *n)
{
	if (is_integer(n)) {
		q->nv.val_i = n->val_i;
		q->nv.flags = TYPE_INTEGER;
	}
	else if (is_float(n)) {
		q->nv.val_f = n->val_f;
		q->nv.flags = TYPE_FLOAT;
	}
#if USE_SSL
	else if (is_bignum(n)) {
		q->nv.val_bn = BN_dup(n->val_bn);
		q->nv.flags = TYPE_BIGNUM;
	}
#endif
	else
		q->nv.flags = 0;
}

static void eval(tpl_query *q, node **args);

static void eval_term(tpl_query *q, node *n)
{
	if (!n->bifptr) {
		// eval_term(q, term_first(n)); // Assume empty parens.
		q->nv.flags = 0;
		return;
	}

	node *save = q->c.curr_term;
	q->c.curr_term = n;

	if (is_builtin(n)) {
		n->bifptr(q);

#ifdef DEBUG
		g_s_resolves++;
#endif

		q->c.curr_term = save;
		return;
	}

	// UDFs...

	allocate_frame(q);
	try_me(q);
	q->c.curr_term = n;
	query_inline(q);
	trust_me(q);
	q->c.curr_term = save;
}

static void eval(tpl_query *q, node **args)
{
	node *tmp = get_next_arg(q, args);
	q->eval++;

	if (is_number(tmp))
		eval_nbr(q, tmp);
	else if (is_callable(tmp))
		eval_term(q, tmp);

	q->eval--;
}

static int bif_iso_is(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nbr_or_var(term1);
	eval(q, &args);

	if (is_var(term1)) {
		if (q->nv.flags & TYPE_INTEGER)
			put_int(q, q->c.curr_frame + term1->slot, q->nv.val_i);
		else if (q->nv.flags & TYPE_FLOAT)
			put_float(q, q->c.curr_frame + term1->slot, q->nv.val_f);
#if USE_SSL
		else if (q->nv.flags & TYPE_BIGNUM) {
			put_bignum(q, q->c.curr_frame + term1->slot, &q->nv);
		}
#endif
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}

		return 1;
	}

	if (is_integer(term1) && (q->nv.flags & TYPE_INTEGER))
		return term1->val_i == q->nv.val_i;
	else if (is_float(term1) && (q->nv.flags & TYPE_FLOAT))
		return term1->val_f == q->nv.val_f;
#if USE_SSL
	else if (is_bignum(term1) && (q->nv.flags & TYPE_BIGNUM))
		return !BN_cmp(term1->val_bn, q->nv.val_bn);
	else if (is_bignum(term1) && (q->nv.flags & TYPE_INTEGER)) {
		node nv1;
		nv1.val_bn = BN_new();
		BN_set_word(nv1.val_bn, q->nv.val_i);
		int ok = !BN_cmp(term1->val_bn, nv1.val_bn);
		BN_free(nv1.val_bn);
		return ok;
	}
#endif

	QABORT(ABORT_TYPEERROR);
	return 0;
}

static int bif_iso_integer(tpl_query *q)
{
	node *args = get_args(q);

	if (q->eval) {
		eval(q, &args);

		if (q->nv.flags & TYPE_FLOAT) {
			q->nv.val_i = (nbr_t)q->nv.val_f;
			q->nv.flags = TYPE_INTEGER;
		}
#if USE_SSL
		else if (q->nv.flags & TYPE_BIGNUM) {
			q->nv.val_i = BN_get_word(q->nv.val_bn);
			q->nv.flags = TYPE_INTEGER;
		}
#endif
		else if (q->nv.flags != TYPE_INTEGER) {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}

		return 1;
	}
	else {
		node *term1 = get_term(term1);
		return is_integer(term1) ? 1 : 0;
	}
}

static int bif_iso_float(tpl_query *q)
{
	node *args = get_args(q);

	if (q->eval) {
		eval(q, &args);

		if (q->nv.flags & TYPE_INTEGER) {
			q->nv.val_f = (flt_t)q->nv.val_i;
			q->nv.flags = TYPE_FLOAT;
		}
#if USE_SSL
		else if (q->nv.flags & TYPE_BIGNUM) {
			q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
			q->nv.flags = TYPE_FLOAT;
		}
#endif
		else if (q->nv.flags != TYPE_FLOAT) {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}

		return 1;
	}
	else {
		node *term1 = get_term(term1);
		return is_float(term1) ? 1 : 0;
	}
}

int bif_iso_reverse(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_i = -q->nv.val_i;
	else if (q->nv.flags & TYPE_FLOAT)
		q->nv.val_f = -q->nv.val_f;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		BN_set_negative(q->nv.val_bn, !BN_is_negative(q->nv.val_bn));
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	return 1;
}

static int bif_iso_add(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_INTEGER)
			q->nv.val_i = nv1.val_i + nv2.val_i;
		else if (nv2.flags & TYPE_FLOAT) {
			q->nv.val_f = nv1.val_i + nv2.val_f;
			nv1.flags = TYPE_FLOAT;
		}
#if USE_SSL
		else if (nv2.flags & TYPE_BIGNUM) {
			q->nv.val_bn = nv2.val_bn;
			BN_add_word(q->nv.val_bn, nv1.val_i);
			nv1.flags = TYPE_BIGNUM;
		}
#endif
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
	else if (nv1.flags & TYPE_FLOAT) {
		if (nv2.flags & TYPE_FLOAT)
			q->nv.val_f = nv1.val_f + nv2.val_f;
		else if (nv2.flags & TYPE_INTEGER)
			q->nv.val_f = nv1.val_f + (flt_t)nv2.val_i;
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			BN_add(q->nv.val_bn, nv1.val_bn, nv2.val_bn);
			BN_free(nv1.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			q->nv.val_bn = nv1.val_bn;
			BN_add_word(q->nv.val_bn, nv2.val_u);
		}
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = nv1.flags;
	return 1;
}

static int bif_iso_subtract(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_INTEGER)
			q->nv.val_i = nv1.val_i - nv2.val_i;
		else if (nv2.flags & TYPE_FLOAT) {
			q->nv.val_f = (flt_t)nv1.val_i - nv2.val_f;
			nv1.flags = TYPE_FLOAT;
		}
#if USE_SSL
		else if (nv2.flags & TYPE_BIGNUM) {
			q->nv.val_bn = nv2.val_bn;
			BN_sub_word(q->nv.val_bn, nv1.val_i);
			nv1.flags = TYPE_BIGNUM;
		}
#endif
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
	else if (nv1.flags & TYPE_FLOAT) {
		if (nv2.flags & TYPE_FLOAT)
			q->nv.val_f = nv1.val_f - nv2.val_f;
		else if (nv2.flags & TYPE_INTEGER)
			q->nv.val_f = nv1.val_f - (flt_t)nv2.val_i;
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			BN_sub(q->nv.val_bn, nv1.val_bn, nv2.val_bn);
			BN_free(nv1.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			q->nv.val_bn = nv1.val_bn;
			BN_sub_word(q->nv.val_bn, nv2.val_u);
		}
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = nv1.flags;
	return 1;
}

static int bif_iso_multiply(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_INTEGER)
			q->nv.val_i = nv1.val_i * nv2.val_i;
		else if (nv2.flags & TYPE_FLOAT) {
			q->nv.val_f = (flt_t)nv1.val_i * nv2.val_f;
			nv1.flags = TYPE_FLOAT;
		}
#if USE_SSL
		else if (nv2.flags & TYPE_BIGNUM) {
			q->nv.val_bn = nv2.val_bn;
			BN_mul_word(q->nv.val_bn, nv1.val_i);
			nv1.flags = TYPE_BIGNUM;
		}
#endif
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
	else if (nv1.flags & TYPE_FLOAT) {
		if (nv2.flags & TYPE_FLOAT)
			q->nv.val_f = nv1.val_f * nv2.val_f;
		else if (nv2.flags & TYPE_INTEGER)
			q->nv.val_f = nv1.val_f * (flt_t)nv2.val_i;
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			if (!q->ctx)
				q->ctx = BN_CTX_new();

			BN_mul(q->nv.val_bn, nv1.val_bn, nv2.val_bn, q->ctx);
			BN_free(nv1.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			q->nv.val_bn = nv1.val_bn;
			BN_mul_word(q->nv.val_bn, nv2.val_u);
		}
		else {
			QABORT(ABORT_TYPEERROR);
			return 0;
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = nv1.flags;
	return 1;
}

static int bif_iso_divide(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER)) {
		if (nv2.val_i == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_f = (flt_t)nv1.val_i / (flt_t)nv2.val_i;
	}
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT)) {
		if (nv2.val_f == (flt_t)0.0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_f = (flt_t)nv1.val_i / nv2.val_f;
	}
#if USE_SSL
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_BIGNUM)) {
		if (BN_is_zero(nv2.val_bn)) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}

		q->nv.val_f = (flt_t)nv1.val_i / (flt_t)BN_get_word(nv2.val_bn);
	}
#endif
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT)) {
		if (nv2.val_f == (flt_t)0.0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_f = nv1.val_f / nv2.val_f;
	}
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER)) {
		if (nv2.val_i == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_f = nv1.val_f / (flt_t)nv2.val_i;
	}
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			if (BN_is_zero(nv2.val_bn)) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			if (!q->ctx)
				q->ctx = BN_CTX_new();

			node nv0;
			nv0.val_bn = BN_new();
			nbr_t divisor = BN_get_word(nv2.val_bn);
			BN_div(q->nv.val_bn, nv0.val_bn, nv1.val_bn, nv2.val_bn, q->ctx);
			q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn) + ((flt_t)BN_get_word(nv0.val_bn) / (flt_t)divisor);
			BN_free(nv1.val_bn);
			BN_free(nv0.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			if (nv2.val_i == 0) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			nbr_t rem = BN_div_word(nv1.val_bn, nv2.val_i);
			q->nv.val_f = (flt_t)BN_get_word(nv1.val_bn) + ((flt_t)rem / (flt_t)nv2.val_i);
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_divint(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER)) {
		if (nv2.val_i == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_i = nv1.val_i / nv2.val_i;
	}
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT)) {
		if (nv2.val_f == (flt_t)0.0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_i = nv1.val_i / nv2.val_f;
	}
#if USE_SSL
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_BIGNUM)) {
		if (BN_is_zero(nv2.val_bn)) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}

		q->nv.val_i = nv1.val_i / BN_get_word(nv2.val_bn);
	}
#endif
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT)) {
		if (nv2.val_f == (flt_t)0.0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_i = (nbr_t)nv1.val_f / nv2.val_f;
	}
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER)) {
		if (nv2.val_i == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_i = (nbr_t)nv1.val_f / nv2.val_i;
	}
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			if (BN_is_zero(nv2.val_bn)) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			if (!q->ctx)
				q->ctx = BN_CTX_new();

			node nv0;
			nv0.val_bn = BN_new();
			BN_div(q->nv.val_bn, nv0.val_bn, nv1.val_bn, nv2.val_bn, q->ctx);
			BN_free(nv1.val_bn);
			BN_free(nv0.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			if (nv2.val_i == 0) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			BN_div_word(nv1.val_bn, nv2.val_i);
			q->nv.val_bn = nv1.val_bn;
		}

		q->nv.flags = TYPE_BIGNUM;
		return 1;
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_rem(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER)) {
		if (nv2.val_i == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}
		q->nv.val_i = nv1.val_i % nv2.val_i;
	}
#if USE_SSL
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_BIGNUM)) {
		if (BN_is_zero(nv2.val_bn)) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}

		q->nv.val_i = nv1.val_i % BN_get_word(nv2.val_bn);
	}
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			if (BN_is_zero(nv2.val_bn)) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			if (!q->ctx)
				q->ctx = BN_CTX_new();

			node nv0;
			nv0.val_bn = BN_new();
			BN_div(nv0.val_bn, q->nv.val_bn, nv1.val_bn, nv2.val_bn, q->ctx);
			BN_free(nv1.val_bn);
			BN_free(nv0.val_bn);
			q->nv.flags = TYPE_BIGNUM;
		}
		else if (nv2.flags & TYPE_INTEGER) {
			if (nv2.val_i == 0) {
				QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
				return 0;
			}

			q->nv.val_bn = nv1.val_bn;
			nbr_t rem = BN_mod_word(q->nv.val_bn, nv2.val_i);
			BN_free(nv1.val_bn);
			q->nv.val_i = rem;
			q->nv.flags = TYPE_INTEGER;
		}

		return 1;
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_nlt(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;
	int ok = 0;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_i < nv2.val_i;
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT))
		ok = (flt_t)nv1.val_i < nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT))
		ok = nv1.val_f < nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_f < (flt_t)nv2.val_i;
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			ok = BN_cmp(nv1.val_bn, nv2.val_bn) < 0;
			BN_free(nv1.val_bn);
			BN_free(nv2.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			nbr_t val = BN_get_word(nv1.val_bn);
			ok = val < nv2.val_i;
			BN_free(nv1.val_bn);
		}
	}
	else if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_BIGNUM) {
			nbr_t val = BN_get_word(nv2.val_bn);
			ok = nv1.val_i < val;
			BN_free(nv2.val_bn);
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = 0;
	return ok;
}

static int bif_iso_nle(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;
	int ok = 0;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_i <= nv2.val_i;
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT))
		ok = (flt_t)nv1.val_i <= nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT))
		ok = nv1.val_f <= nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_f <= (flt_t)nv2.val_i;
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			ok = BN_cmp(nv1.val_bn, nv2.val_bn) <= 0;
			BN_free(nv1.val_bn);
			BN_free(nv2.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			nbr_t val = BN_get_word(nv1.val_bn);
			ok = val <= nv2.val_i;
			BN_free(nv1.val_bn);
		}
	}
	else if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_BIGNUM) {
			nbr_t val = BN_get_word(nv2.val_bn);
			ok = nv1.val_i <= val;
			BN_free(nv2.val_bn);
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = 0;
	return ok;
}

static int bif_iso_neq(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;
	int ok = 0;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_i == nv2.val_i;
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT))
		ok = (flt_t)nv1.val_i == nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT))
		ok = nv1.val_f == nv2.val_f;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER))
		ok = nv1.val_f == (flt_t)nv2.val_i;
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM) {
		if (nv2.flags & TYPE_BIGNUM) {
			ok = BN_cmp(nv1.val_bn, nv2.val_bn) == 0;
			BN_free(nv1.val_bn);
			BN_free(nv2.val_bn);
		}
		else if (nv2.flags & TYPE_INTEGER) {
			nbr_t val = BN_get_word(nv1.val_bn);
			ok = val == nv2.val_i;
			BN_free(nv1.val_bn);
		}
	}
	else if (nv1.flags & TYPE_INTEGER) {
		if (nv2.flags & TYPE_BIGNUM) {
			nbr_t val = BN_get_word(nv2.val_bn);
			ok = nv1.val_i == val;
			BN_free(nv2.val_bn);
		}
	}
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = 0;
	return ok;
}

static int bif_iso_ngt(tpl_query *q) { return !bif_iso_nle(q); }
static int bif_iso_nge(tpl_query *q) { return !bif_iso_nlt(q); }
static int bif_iso_nne(tpl_query *q) { return !bif_iso_neq(q); }

static int bif_iso_compare(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	node *term3 = get_term(term3);

	if (term1->bifptr == bif_iso_nlt)
		return compare_terms(q, term2, term3, CMP_LT) < 0;
	else if (term1->bifptr == bif_iso_ngt)
		return compare_terms(q, term2, term3, CMP_LE) > 0;
	else if (term1->bifptr == bif_iso_unify)
		return compare_terms(q, term2, term3, CMP_EQ) == 0;

	QABORT(ABORT_INVALIDOPUNKNOWN);
	return 0;
}

static int bif_iso_shiftleft(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		q->nv.val_u = nv1.val_u << nv2.val_u;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_shiftright(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		q->nv.val_u = nv1.val_u >> nv2.val_u;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_bitand(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		q->nv.val_u = nv1.val_u & nv2.val_u;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_bitor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		q->nv.val_u = nv1.val_u | nv2.val_u;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_xor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		q->nv.val_u = nv1.val_u ^ nv2.val_u;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_complement(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags != TYPE_INTEGER) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_u = ~q->nv.val_u;
	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_sin(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = sin(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_asin(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = asin(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_cos(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = cos(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_acos(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = acos(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_tan(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = tan(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_atan(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = atan(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_atan_2(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (nv1.flags & TYPE_INTEGER)
		nv1.val_f = (flt_t)nv1.val_i;
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM)
		nv1.val_f = (flt_t)BN_get_word(nv1.val_bn);
#endif
	else if (!(nv1.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	if (nv2.flags & TYPE_INTEGER)
		nv2.val_f = (flt_t)nv2.val_i;
#if USE_SSL
	else if (nv2.flags & TYPE_BIGNUM)
		nv2.val_f = (flt_t)BN_get_word(nv2.val_bn);
#endif
	else if (!(nv2.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = atan2(nv1.val_f, nv2.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_pow(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (nv1.flags & TYPE_INTEGER)
		nv1.val_f = (flt_t)nv1.val_i;
#if USE_SSL
	else if (nv1.flags & TYPE_BIGNUM)
		nv1.val_f = (flt_t)BN_get_word(nv1.val_bn);
#endif
	else if (!(nv1.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	if (nv2.flags & TYPE_INTEGER)
		nv2.val_f = (flt_t)nv2.val_i;
#if USE_SSL
	else if (nv2.flags & TYPE_BIGNUM)
		nv2.val_f = (flt_t)BN_get_word(nv2.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = pow(nv1.val_f, nv2.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_powi(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if ((nv1.flags != TYPE_INTEGER) || (nv2.flags != TYPE_INTEGER))
		return bif_iso_pow(q);

	if (nv2.val_i < 0) {
		QABORT(ABORT_RESULTOVERFLOW);
		return 0;
	}

	q->nv.val_i = (nbr_t)pow((flt_t)nv1.val_i, (flt_t)nv2.val_i);
	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_exp(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = exp(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_sqrt(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = sqrt(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_log(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else if (!(q->nv.flags & TYPE_FLOAT)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = log(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_abs(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_FLOAT)
		q->nv.val_f = q->nv.val_f < (flt_t)0.0 ? -q->nv.val_f : q->nv.val_f;
	else if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_i = q->nv.val_i < 0 ? -q->nv.val_i : q->nv.val_i;
#if USE_SSL
	else if (q->nv.flags & TYPE_BIGNUM)
		q->nv.val_f = (flt_t)BN_get_word(q->nv.val_bn);
#endif
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	return 1;
}

static int bif_iso_max(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;
	node *np = NULL;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		np = nv1.val_i > nv2.val_i ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT))
		np = nv1.val_i > nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT))
		np = nv1.val_f > nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER))
		np = nv1.val_f > nv2.val_i ? &nv1 : &nv2;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv = *np;
	return 1;
}

static int bif_iso_min(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;
	node *np = NULL;

	if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_INTEGER))
		np = nv1.val_i < nv2.val_i ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_INTEGER) && (nv2.flags & TYPE_FLOAT))
		np = nv1.val_i < nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_FLOAT))
		np = nv1.val_f < nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.flags & TYPE_FLOAT) && (nv2.flags & TYPE_INTEGER))
		np = nv1.val_f < nv2.val_i ? &nv1 : &nv2;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv = *np;
	return 1;
}

static int bif_iso_sign(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_INTEGER)
		q->nv.val_i = q->nv.val_i < 0 ? -1 : q->nv.val_i > 0 ? 1 : 0;
	else if (q->nv.flags & TYPE_FLOAT)
		q->nv.val_f = q->nv.val_f < (flt_t)0.0 ? (flt_t)-1.0 : q->nv.val_f > (flt_t)0.0 ? (flt_t)1.0 : (flt_t)0.0;
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	return 1;
}

static int bif_iso_float_fractional_part(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags != TYPE_FLOAT) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f -= (nbr_t)q->nv.val_f;
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_float_integer_part(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags != TYPE_FLOAT) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f = (nbr_t)q->nv.val_f;
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_floor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags != TYPE_FLOAT) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_i = floor(q->nv.val_f);
	q->nv.flags = TYPE_INTEGER;
	return 1;
}

static int bif_iso_round(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_FLOAT) {
		q->nv.flags = TYPE_INTEGER;
		q->nv.val_i = (nbr_t)(q->nv.val_f + (flt_t)0.5);
	}
	else if (q->nv.flags != TYPE_INTEGER) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	return 1;
}

static int bif_iso_ceiling(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags != TYPE_FLOAT) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.val_f -= (flt_t)ceil(q->nv.val_f);
	q->nv.flags = TYPE_FLOAT;
	return 1;
}

static int bif_iso_truncate(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.flags & TYPE_FLOAT) {
		q->nv.val_i = (nbr_t)q->nv.val_f;
		q->nv.flags = TYPE_INTEGER;
	}
	else if (q->nv.flags != TYPE_INTEGER) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	return 1;
}

#ifndef ISO_ONLY
static int bif_xtra_between_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_atom_or_int(term2);
	node *orig_term3 = term_next(args);
	node *term3;
	nbr_t maxn;

	if (!q->retry) {
		if (is_atom(term2) && strcmp(VAL_S(term2), "inf") && strcmp(VAL_S(term2), "infinite")) {
			QABORT(ABORT_INVALIDARGNOTINT);
			return 0;
		}
	}

	if (is_atom(term2))
		maxn = LONG_MAX;
	else
		maxn = get_word(term2);

	if (!q->retry) {
		term3 = get_term(term3);

		if (is_var(term3)) {
			nbr_t v = get_word(term1);
			put_int(q, q->c.curr_frame + term3->slot, v);
		}
		else {
			nbr_t v = get_word(term3);
			return (v >= get_word(term1)) && (v <= get_word(term2));
		}

		allocate_frame(q);
	}
	else {
		term3 = get_next_arg(q, &args);
		nbr_t v = term3->val_i + 1;

		if (v > maxn)
			return 0;

		reset_arg(q, orig_term3, q->c.curr_frame);
		put_int(q, q->c.curr_frame + orig_term3->slot, v);
	}

	try_me_nofollow(q);
	return 1;
}

static int bif_xtra_phrase(tpl_query *q)
{
	if (q->retry)
		return 0;

	node *args = get_args(q);
	node *var = get_var(var); // FLAG_HIDDEN
	node *term1 = get_callable(term1);
	node *param = get_next_arg(q, &args);
	node *tmp = NULL;

	if (is_atom(term1)) {
		tmp = make_compound();
		term_append(tmp, clone_term(q, term1));
	}
	else
		tmp = clone_term(q, term1);

	term_append(tmp, clone_term(q, param));
	param = get_next_arg(q, &args);
	int made = 0;

	if (param == NULL) {
		param = make_const_atom("[]");
		made = 1;
	}

	term_append(tmp, clone_term(q, param));

	if (made)
		term_heapcheck(param);

	const char *functor = term_functor(tmp);
	int arity = term_arity(tmp);
	tmp->bifptr = get_bifarity(q->lex, functor, arity)->bifptr;

	if (!tmp->bifptr)
		tmp->match = xref_term(q->lex, tmp, arity);
	else
		tmp->flags |= FLAG_BUILTIN;

	put_env(q, q->c.curr_frame + var->slot, tmp, -1);
	term_heapcheck(tmp);
	allocate_frame(q);
	try_me(q);
	q->c.curr_term = tmp;
	return call(q);
}

static int bif_xtra_time_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	trust_me(subq);
	begin_query(subq, term1);
	int ok = query_run(subq);
	printf("(%.3lf s) %s\n", query_elapsed(subq), ok ? "true" : "false");
	query_destroy(subq);
	return ok;
}

static int bif_xtra_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	trust_me(subq);
	begin_query(subq, term1);
	int ok = query_run(subq);
	put_float(q, q->c.curr_frame + term2->slot, query_elapsed(subq));
	query_destroy(subq);
	return ok;
}

static int bif_xtra_term_to_atom_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nonvar(term1);
	node *term2 = get_atom_or_var(term2);
	int ok;

	if (!is_atom(term1)) {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
		ok = unify_atom(q, term2, strdup(tmpbuf));
		free(tmpbuf);
	}
	else
		ok = unify_term(q, term1, term2, q->c.curr_frame);

	return ok;
}

static int bif_xtra_term_to_blob(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nonvar(term1);
	node *term2 = get_var(term2);
	int ok;

	if (is_atom(term1)) {
		node *n = make_blob(strdup(VAL_S(term1)), strlen(VAL_S(term1)));
		ok = unify_term(q, term2, n, -1);
		term_heapcheck(n);
	}
	else if (!is_blob(term1)) {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
		node *n = make_blob(strdup(tmpbuf), dst - tmpbuf);
		ok = unify_term(q, term2, n, -1);
		term_heapcheck(n);
		free(tmpbuf);
	}
	else
		ok = unify_term(q, term1, term2, q->c.curr_frame);

	return ok;
}

static int bif_xtra_abolish_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int(term2);
	const char *functor = VAL_S(term1);
	int arity = (int)term2->val_i;
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	rule *r = NULL;

	if (!q->in_tran)
		DBLOCK(q->c.curr_db);

	if (!sl_get(&q->c.curr_db->rules, tmpbuf, (void **)&r)) {
		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);

		return 0;
	}

	if (!r->dynamic) {
		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);

		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	for (node *match = NLIST_FRONT(&r->val_l); match; match = term_next(match))
		match->flags |= FLAG_DELETED;

	if (sl_del(&q->c.curr_db->rules, tmpbuf, (void **)&r))
		free(r);

	if (!q->in_tran)
		DBUNLOCK(q->c.curr_db);

	return 1;
}

static int bif_xtra_is_list_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);

	if (is_atom(term1)) {
		if (!strcmp(VAL_S(term1), "[]"))
			return 1;
	}

	return is_list(term1);
}

static int bif_xtra_is_tuple_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_tuple(term1);
}

static int bif_xtra_is_struct_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_structure(term1);
}

static int bif_xtra_is_stream_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);

	if (!is_stream(term1))
		return 0;

	stream *sp = term1->val_str;

	if (!sp->fptr && !sp->sptr)
		return 0;

	return 1;
}

static int bif_xtra_consult_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return trealla_consult_file(q->pl, VAL_S(term1));
}

static int bif_xtra_deconsult_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return trealla_deconsult(q->pl, VAL_S(term1));
}

static int bif_xtra_reconsult_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	trealla_deconsult(q->pl, VAL_S(term1));
	return trealla_consult_file(q->pl, VAL_S(term1));
}

static int bif_xtra_maplist_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_atom_or_list(term2);
	node *l2 = term2;

	if (is_atom(term2) && !strcmp(VAL_S(term2), "[]"))
		return 1;

	while (is_list(l2) && !g_abort) {
		tpl_query *subq = query_create_subquery(q);

		if (!subq) {
			QABORT(ABORT_OUTOFMEMORY);
			return 0;
		}

		l2 = term_firstarg(l2);
		unsigned this_context = q->latest_context;
		node *s = NULL;

		if (is_structure(term1)) {
			s = clone_term(q, term1);
		}
		else {
			s = make_compound();

			if (is_builtin(term1))
				s->flags |= FLAG_BUILTIN;

			s->bifptr = term1->bifptr;
			term_append(s, clone_term(q, term1));
		}

		term_append(s, clone_term(q, get_arg(q, l2, this_context)));
		begin_query(subq, s);
		int ok = query_run(subq);
		query_destroy(subq);
		term_heapcheck(s);

		if (!ok)
			return 0;

		l2 = get_arg(q, term_next(l2), this_context);
	}

	return 1;
}

static int bif_xtra_maplist_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_atom_or_list(term2);
	node *term3 = get_atom_or_list(term3);
	node *l2 = term2;
	node *l3 = term3;

	if (is_atom(term2) && !strcmp(VAL_S(term2), "[]"))
		return 1;

	if (is_atom(term3) && !strcmp(VAL_S(term3), "[]"))
		return 1;

	while (is_list(l2) && is_list(l3) && !g_abort) {
		tpl_query *subq = query_create_subquery(q);

		if (!subq) {
			QABORT(ABORT_OUTOFMEMORY);
			return 0;
		}

		l2 = term_firstarg(l2);
		l3 = term_firstarg(l3);
		unsigned this_context = q->latest_context;
		node *s = NULL;

		if (is_structure(term1)) {
			s = clone_term(q, term1);
		}
		else {
			s = make_compound();

			if (is_builtin(term1))
				s->flags |= FLAG_BUILTIN;

			s->bifptr = term1->bifptr;
			term_append(s, clone_term(q, term1));
		}

		term_append(s, clone_term(q, get_arg(q, l2, this_context)));
		term_append(s, clone_term(q, get_arg(q, l3, this_context)));
		begin_query(subq, s);
		int ok = query_run(subq);
		query_destroy(subq);
		term_heapcheck(s);

		if (!ok)
			return 0;

		l2 = get_arg(q, term_next(l2), this_context);
		l3 = get_arg(q, term_next(l3), this_context);
	}

	return 1;
}

static int bif_xtra_findnsols_4(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_term(var); // FLAG_HIDDEN
	node *term1 = get_int(term1);
	node *term2 = get_term(term2);
	node *term3 = get_callable(term3);
	node *term4 = term_next(args);
	tpl_query *subq;
	stream *sp;

	if (!q->retry) {
		if (!is_var(var)) {
			QABORT(ABORT_INVALIDARGNOTVAR);
			return 0;
		}

		subq = query_create_subquery(q);

		if (!subq) {
			QABORT(ABORT_OUTOFMEMORY);
			return 0;
		}

		sp = calloc(1, sizeof(stream));
		sp->subqptr = subq;
		node *n = make_stream(sp);
		put_env(q, var->slot, n, -1);
		term_heapcheck(n);
		sp->subqgoal = clone_term(q, term3);
		begin_query(subq, sp->subqgoal);
		query_run(subq);
	}
	else {
		reset_arg(q, term4, q->c.curr_frame);
		sp = var->val_str;
		subq = sp->subqptr;
	}

	if (!q->retry)
		allocate_frame(q);

	try_me_nofollow(q);
	node *acc = make_const_atom("[]");
	node *end = NULL;
	int i = 1;

	while (subq->ok && !g_abort) {
		subq->latest_context = 0;
		node *res = clone_term(subq, term2);

		if (!end) {
			free(acc);
			acc = make_list();
			term_append(acc, res);
			end = acc;
		}
		else {
			node *tmp = make_list();
			term_append(end, tmp);
			term_append(tmp, res);
			end = tmp;
		}

		query_continue(subq);

		if (i++ == term1->val_i)
			break;
	}

	if (!subq->ok)
		trust_me(q);

	term_append(end, make_const_atom("[]"));
	return unify_term(q, term4, acc, q->latest_context);
}

static int bif_xtra_listing(tpl_query *q)
{
	const char *functor = NULL;

	if (is_compound(q->c.curr_term)) {
		node *args = get_args(q);
		node *term1 = get_next_arg(q, &args);

		if (term1) {
			if (is_atom(term1))
				;
			else if (is_structure(term1)) {
				term1 = term_firstarg(term1);
				functor = term_functor(term1);
			}

			functor = VAL_S(term1);
		}
	}

	// printf("DEBUG: listing module '%s'\n", q->c.curr_db->name);

	module *db = q->c.curr_db;
	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void **)&r) != NULL) {
		for (node *n = NLIST_FRONT(&r->val_l); n; n = term_next(n)) {
			if ((n->flags & FLAG_HIDDEN) && !functor)
				continue;

			if (functor) {
				node *head = term_firstarg(n);

				if (is_atom(head)) {
					if (strncmp(VAL_S(head), functor, strlen(functor)))
						continue;
				}
				else if (is_compound(head)) {
					if (strncmp(VAL_S(term_first(head)), functor, strlen(functor)))
						continue;
				}
			}

			term_print(q->pl, q, n, 1);
			printf(".\n");
		}
	}

	return 1;
}

static int bif_xtra_listing_canonical(tpl_query *q)
{
	const char *functor = NULL;

	if (is_compound(q->c.curr_term)) {
		node *args = get_args(q);
		node *term1 = get_next_arg(q, &args);

		if (term1) {
			if (!is_atom(term1)) {
				QABORT(ABORT_INVALIDARGNOTATOM);
				return 0;
			}
		}

		functor = VAL_S(term1);
	}

	// printf("DEBUG: listing module '%s'\n", q->c.curr_db->name);

	module *db = q->c.curr_db;
	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void **)&r) != NULL) {
		for (node *n = NLIST_FRONT(&r->val_l); n; n = term_next(n)) {
			if ((n->flags & FLAG_HIDDEN) && !functor)
				continue;

			if (functor) {
				node *head = term_firstarg(n);

				if (is_atom(head)) {
					if (strncmp(VAL_S(head), functor, strlen(functor)))
						continue;
				}
				else if (is_compound(head)) {
					if (strncmp(VAL_S(term_first(head)), functor, strlen(functor)))
						continue;
				}
			}

			term_print(q->pl, q, n, 2);
			printf(".\n");
		}
	}

	return 1;
}

#if USE_SSL
static int bif_xtra_unbounded_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);

	if (!q->nv.val_bn)
		q->nv.val_bn = BN_new();

	if (is_bignum(term1))
		BN_copy(q->nv.val_bn, term1->val_bn);
	else
		BN_set_word(q->nv.val_bn, term1->val_i);

	q->nv.flags = TYPE_BIGNUM;
	return 1;
}
#endif

static int bif_xtra_random_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	double v = (double)rand_r(&q->seed) / (double)RAND_MAX;
	put_float(q, q->c.curr_frame + term1->slot, v);
	return 1;
}

static uint32_t jenkins_one_at_a_time_hash(char *key)
{
	uint32_t hash = 0;

	while (*key != 0) {
		hash += *key++;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);
	return hash;
}

static int bif_xtra_term_hash_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);

	if (is_atom(term1) && !is_blob(term1)) {
		put_int(q, q->c.curr_frame + term2->slot, jenkins_one_at_a_time_hash(VAL_S(term1)));
	}
	else {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
		put_int(q, q->c.curr_frame + term2->slot, jenkins_one_at_a_time_hash(tmpbuf));
		free(tmpbuf);
	}

	return 1;
}

static int bif_xtra_atom_number_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_nbr_or_var(term2);
	const char *src = VAL_S(term1);
	nbr_t v = 0;
	int numeric = 0;
	parse_number(src, &v, &numeric);
	node *n;

	if (numeric == NUM_REAL)
		n = make_float(strtod(src, NULL));
#if USE_SSL
	else if (numeric == NUM_BIGNUM)
		n = make_bignum(src);
#endif
	else if (numeric == NUM_INT)
		n = make_quick_int(v);
	else
		return 0;

	if (numeric == NUM_HEX)
		n->flags |= FLAG_HEX;
	else if (numeric == NUM_OCTAL)
		n->flags |= FLAG_OCTAL;
	else if (numeric == NUM_BINARY)
		n->flags |= FLAG_BINARY;

	int ok = unify_term(q, term2, n, q->c.curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_xtra_read_term_from_atom_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	node *term3 = get_atom_or_list(term3);
	char *line = VAL_S(term1);
	return bif_read_term(q, line, term2, term3);
}

int bif_xtra_enter(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);

	if (!sl_get(&q->pl->mods, VAL_S(term1), (void **)&q->c.curr_db))
		q->c.curr_db = &q->pl->db;

	return 1;
}

static int bif_xtra_writeln_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
	*dst++ = '\n';
	*dst = '\0';
	fwrite(tmpbuf, 1, dst - tmpbuf, q->curr_stdout);
	free(tmpbuf);
	return 1;
}

static int bif_xtra_writeln_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 0);
	*dst++ = '\n';
	*dst = '\0';
	int ok;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, dst - tmpbuf);
	else
		ok = fwrite(tmpbuf, 1, dst - tmpbuf, sp->fptr);

	free(tmpbuf);
	return ok >= 0;
}

static int bif_xtra_unload_file(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	dir_unload_file(q->lex, term1);
	return 1;
}
#endif

static int bif_iso_op(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	return dir_op_3(q->lex, get_word(term1), VAL_S(term2), VAL_S(term3));
}

#if 0
static int bif_iso_include(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return dir_include(q->lex, term1);
}
#endif

static int bif_iso_dynamic(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_compound(term1);
	return dir_dynamic(q->lex, term1);
}

#ifndef ISO_ONLY
static int bif_xtra_using(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	dir_using(q->lex, term1);
	return 1;
}

static int bif_xtra_use_module(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	dir_use_module(q->lex, term1);
	return 1;
}

static int bif_xtra_trace_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);

	if (is_integer(term1))
		q->trace = get_word(term1);

	return unify_int(q, term1, q->trace);
}

static int bif_xtra_trace_0(tpl_query *q)
{
	q->trace = 1;
	return 1;
}

#ifndef ISO_ONLY
static int bif_linda_out(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_tuple(term1);
	return bif_iso_assertz(q);
}
#endif

static int bif_xtra_see_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);

		if (!strcmp(q->curr_stdin_name, filename))
			return 1;

		free(q->curr_stdin_name);
		FILE *fp = fopen(filename, "r");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			q->curr_stdout_name = strdup("user");
			q->curr_stdout = stdin;
			return 0;
		}

		stream *sp = (stream*)calloc(1, sizeof(stream));
		sp->filename = strdup(filename);
		sp->fptr = fp;
		q->curr_stdin_stream = sp;
		q->curr_stdin = fp;
		q->curr_stdin_name = strdup(filename);
	}
	else {
		free(q->curr_stdin_name);
		stream *sp = term1->val_str;
		q->curr_stdin_stream = sp;
		q->curr_stdin = sp->fptr;
		q->curr_stdin_name = strdup(sp->filename);
	}

	return 1;
}

static int bif_xtra_seeing_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);

	if (q->curr_stdin_stream) {
		node *n = make_stream(q->curr_stdin_stream);
		n->flags |= FLAG_FILE | FLAG_CONST;
		int ok = unify_term(q, term1, n, -1);
		term_heapcheck(n);
		return ok;
	}
	else
		return unify_atom(q, term1, strdup(q->curr_stdin_name));
}

static int bif_xtra_seen_0(tpl_query *q)
{
	if (q->curr_stdin != stdin) {
		free(q->curr_stdin_name);
		fclose(q->curr_stdin);
		q->curr_stdin_stream->fptr = NULL;
		q->curr_stdin_stream = NULL;
		q->curr_stdin_name = strdup("user");
		q->curr_stdin = stdin;
	}

	return 1;
}

static int bif_xtra_tell_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);

		if (!strcmp(q->curr_stdout_name, filename))
			return 1;

		free(q->curr_stdout_name);
		FILE *fp = fopen(filename, "w");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			q->curr_stdout_name = strdup("user");
			q->curr_stdout = stdout;
			return 0;
		}

		stream *sp = (stream*)calloc(1, sizeof(stream));
		sp->filename = strdup(filename);
		sp->fptr = fp;
		q->curr_stdout_stream = sp;
		q->curr_stdout = fp;
		q->curr_stdout_name = strdup(filename);
	}
	else {
		free(q->curr_stdout_name);
		stream *sp = term1->val_str;
		q->curr_stdout_stream = sp;
		q->curr_stdout = sp->fptr;
		q->curr_stdout_name = strdup(sp->filename);
	}

	return 1;
}

static int bif_xtra_append_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (q->curr_stdout_name)
		free(q->curr_stdout_name);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);
		FILE *fp = fopen(filename, "a");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			q->curr_stdout_name = strdup("user");
			q->curr_stdout = stdout;
			return 0;
		}

		q->curr_stdout_name = strdup(filename);
		q->curr_stdout = fp;
	}
	else {
		stream *sp = term1->val_str;
		q->curr_stdout_name = strdup(sp->filename);
		q->curr_stdout_stream = sp;
		q->curr_stdout = sp->fptr;
	}

	return 1;
}

static int bif_xtra_telling_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);

	if (q->curr_stdout_stream) {
		node *n = make_stream(q->curr_stdout_stream);
		n->flags |= FLAG_FILE | FLAG_CONST;
		int ok = unify_term(q, term1, n, -1);
		term_heapcheck(n);
		return ok;
	}
	else
		return unify_atom(q, term1, strdup(q->curr_stdout_name));
}

static int bif_xtra_told_0(tpl_query *q)
{
	if (q->curr_stdout != stdout) {
		free(q->curr_stdout_name);
		fclose(q->curr_stdout);
		q->curr_stdout_stream->fptr = NULL;
		q->curr_stdout_stream = NULL;
		q->curr_stdout_name = strdup("user");
		q->curr_stdout = stdout;
	}

	return 1;
}

static int bif_xtra_get_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);
	int ch;

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

LOOP:

	q->did_getc = 1;
	ch = getc_utf8(q->curr_stdin);

	if (ch == EOF)
		q->did_getc = 0;
	else {
		if (ch == '\n')
			q->did_getc = 0;

		if (isspace(ch))
			goto LOOP;
	}

	return unify_int(q, term1, ch);
}

static int bif_xtra_get_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int_or_var(term2);
	int ch;

LOOP:

	ch = getc_utf8(get_input_stream(term1));

	if (ch != EOF) {
		if (isspace(ch))
			goto LOOP;
	}

	return unify_int(q, term2, ch);
}

static int bif_xtra_tab_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	node *term2 = get_int(term2);
	stream *sp = term1->val_str;
	int n = get_word(term1);
	int ok = 1;

	for (int i = 0; ok && (i < n); i++) {
#ifndef ISO_ONLY
		if (is_socket(term1))
			ok = session_write((session *)sp->sptr, "\n", 1);
		else
#endif
			ok = fwrite(" ", 1, 1, sp->fptr);
	}

	return ok > 0;
}

static int bif_xtra_tab_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	int n = get_word(term1);
	int ok = 1;

	for (int i = 0; ok && (i < n); i++)
		ok = fwrite(" ", 1, 1, q->curr_stdout);

	return ok;
}

static int bif_xtra_memberchk_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	unsigned save_context = q->latest_context;
	node *term2 = get_atom_or_list(term2);
	node *l = term2;
	allocate_frame(q);

	while (is_list(l)) {
		node *head = term_firstarg(l);
		unsigned this_context = q->latest_context;
		node *n = get_arg(q, head, q->latest_context);

		if (unify(q, term1, save_context, n, q->latest_context)) {
			return 1;
		}

		reallocate_frame(q);
		node *tail = term_next(head);
		l = get_arg(q, tail, this_context);
	}

	return 0;
}

static int bif_xtra_predicate_property_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_atom_or_var(term2);

	char tmpbuf[FUNCTOR_SIZE];

	if (is_atom(term1))
		sprintf(tmpbuf, "%s/%d", VAL_S(term1), 0);
	else
		sprintf(tmpbuf, "%s/%d", term_functor(term1), term_arity(term1));

	const char *functarity = tmpbuf;

	if (check_builtin(q->pl, functarity))
		return unify_const_atom(q, term2, "built_in");

	if (check_dynamic(q->c.curr_db, functarity))
		return unify_const_atom(q, term2, "dynamic");

	if (check_static(q->c.curr_db, functarity))
		return unify_const_atom(q, term2, "static");

	return 0;
}

static int bif_xtra_forall_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_callable(term2);
	tpl_query *subq = query_create_subquery(q);

	if (!subq) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	int did_lock = 0;

	if (is_dynamic(term1) && !q->in_tran) {
		did_lock = 1;
		DBLOCK(q->c.curr_db);
	}

	node *t1 = clone_term(q, term1);
	begin_query(subq, t1);
	int ok = query_run(subq), subok = 1;

	while (ok && !g_abort) {
		subq->c.curr_frame = FUDGE_FACTOR;
		begin_query(subq, term2);
		run_me(subq);
		subok = subq->ok;

		if (!subq->ok)
			break;

		ok = query_continue(subq);
	}

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	query_destroy(subq);
	term_heapcheck(t1);
	return subok;
}

static int bif_xtra_getenv_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_var(term2);
	const char *value = getenv(VAL_S(term1));

	if (!value)
		return 0;

	return unify_atom(q, term2, strdup(value));
}

static int bif_xtra_setenv_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_int(term2);

	if (is_atom(term2)) {
		setenv(VAL_S(term1), VAL_S(term2), 1);
	}
	else {
		char tmpbuf[40];
		sprintf(tmpbuf, "%lld", (long long)term2->val_i);
		setenv(VAL_S(term1), tmpbuf, 1);
	}

	return 1;
}

static int bif_xtra_unsetenv_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	unsetenv(VAL_S(term1));
	return 1;
}

static int bif_xtra_exists_file_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) != 0)
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return 0;

	return 1;
}

static int bif_xtra_delete_file_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	remove(VAL_S(term1));
	return 1;
}

static int bif_xtra_rename_file_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	return !rename(VAL_S(term1), VAL_S(term2));
}

static int bif_xtra_make_directory_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) == 0)
		return 0;

	return !mkdir(filename, 0777);
}

static int bif_xtra_name_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2)) {
		QABORT(ABORT_INVALIDARGMISSING);
		return 0;
	}

	if (is_list(term2)) {
		size_t buflen = FUNCTOR_SIZE*2;
		char *dstbuf = malloc(buflen);
		char *dst = dstbuf;
		node *l = term2;

		while (is_list(l)) {
			node *head = term_firstarg(l);
			unsigned this_context = q->latest_context;
			node *n = get_arg(q, head, this_context);

			if (!is_integer(n) && !is_bignum(n)) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			nbr_t v = get_word(n);

			if (v <= 0) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			size_t save_len = dst - dstbuf;
			size_t len2 = 8; // Allow for utf8 char

			if ((buflen - save_len) < len2) {
				buflen *= 2;
				buflen += len2;
				dstbuf = realloc(dstbuf, buflen);
				dst = dstbuf + save_len;
			}

			dst += put_char_utf8(dst, v);
			node *tail = term_next(head);
			l = get_arg(q, tail, this_context);
		}

		*dst = '\0';
		int numeric = NUM_NONE;
		nbr_t v;

		if (isdigit(*dstbuf))
			parse_number(dstbuf, &v, &numeric);

		int ok;

		if (isdigit(*dstbuf) && (numeric == NUM_INT))
			ok = unify_int(q, term1, v);
		else if (isdigit(*dstbuf) && (numeric == NUM_BIGNUM)) {
			node *n = make_bignum(dstbuf);
			ok = unify_term(q, term1, n, q->c.curr_frame);
			term_heapcheck(n);
		}
		else if (numeric == NUM_REAL)
			ok = unify_float(q, term1, strtod(dstbuf, NULL));
		else
			ok = unify_atom(q, term1, strdup(dstbuf));

		free(dstbuf);
		return ok;
	}

	node *n;

	if (is_atom(term1) && !VAL_S(term1)) {
		n = make_const_atom("[]");
	}
	else if (is_atom(term1) || is_number(term1)) {
		size_t buflen = FUNCTOR_SIZE;

		if (is_atom(term1) && (LEN(term1) > FUNCTOR_SIZE))
			buflen = LEN(term1) + 1;

		char *dstbuf = malloc(buflen);

		if (is_atom(term1))
			strcpy(dstbuf, VAL_S(term1));
		else
			term_sprint(dstbuf, buflen, q->pl, q, term1, 0);

		const char *src = dstbuf;
		node *save_l = make_list();
		node *l = save_l;

		while (*src) {
			int ch = get_char_utf8(&src);
			node *tmp = make_int(ch);
			term_append(l, tmp);

			if (!*src)
				break;

			tmp = make_list();
			term_append(l, tmp);
			l = tmp;
		}

		term_append(l, make_const_atom("[]"));
		free(dstbuf);
		n = save_l;
	}
	else {
		QABORT(ABORT_INVALIDARGNOTATOMIC);
		return 0;
	}

	int ok = unify_term(q, term2, n, -1);
	term_heapcheck(n);
	return ok;
}
#endif

void bifs_load_iso(void)
{
	DEFINE_BIF("true", 0, bif_iso_true);
	DEFINE_BIF("fail", 0, bif_iso_fail);
	DEFINE_BIF("false", 0, bif_iso_fail);
	DEFINE_BIF("halt", 0, bif_iso_halt);
	DEFINE_BIF("halt", 1, bif_iso_halt_1);
	DEFINE_BIF("!", 0, bif_iso_cut);
	DEFINE_BIF("repeat", 0, bif_iso_repeat);
	DEFINE_BIF("is", 2, bif_iso_is);
	DEFINE_BIF("=", 2, bif_iso_unify);
	DEFINE_BIF("\\=", 2, bif_iso_notunify);
	DEFINE_BIF("]-[", 1, bif_iso_reverse);
	DEFINE_BIF("once", 1, bif_iso_once);
	DEFINE_BIF("call", 1 + 1, bif_iso_call);
	DEFINE_BIF("call", -1, bif_iso_calln);
	DEFINE_BIF("?-", 1, bif_iso_do);
	DEFINE_BIF("\\+", 1, bif_iso_not);
	// DEFINE_BIF("->", 2, bif_iso_ifthen);
	DEFINE_BIF(",", 2, bif_iso_and);
	DEFINE_BIF(";", 2, bif_iso_or);
	DEFINE_BIF("+", 2, bif_iso_add);
	DEFINE_BIF("-", 2, bif_iso_subtract);
	DEFINE_BIF("*", 2, bif_iso_multiply);
	DEFINE_BIF("/", 2, bif_iso_divide);
	DEFINE_BIF("//", 2, bif_iso_divint);
	DEFINE_BIF("rem", 2, bif_iso_rem);
	DEFINE_BIF("mod", 2, bif_iso_rem);
	DEFINE_BIF("**", 2, bif_iso_pow);
	DEFINE_BIF("^", 2, bif_iso_powi);
	DEFINE_BIF("<", 2, bif_iso_nlt);
	DEFINE_BIF("=<", 2, bif_iso_nle);
	DEFINE_BIF(">", 2, bif_iso_ngt);
	DEFINE_BIF(">=", 2, bif_iso_nge);
	DEFINE_BIF("=:=", 2, bif_iso_neq);
	DEFINE_BIF("=\\=", 2, bif_iso_nne);
	DEFINE_BIF("==", 2, bif_iso_seq);
	DEFINE_BIF("\\==", 2, bif_iso_sne);
	DEFINE_BIF("@<", 2, bif_iso_slt);
	DEFINE_BIF("@=<", 2, bif_iso_sle);
	DEFINE_BIF("@>", 2, bif_iso_sgt);
	DEFINE_BIF("@>=", 2, bif_iso_sge);
	DEFINE_BIF(">>", 2, bif_iso_shiftleft);
	DEFINE_BIF("<<", 2, bif_iso_shiftright);
	DEFINE_BIF("/\\", 2, bif_iso_bitand);
	DEFINE_BIF("\\/", 2, bif_iso_bitor);
	DEFINE_BIF("or", 2, bif_iso_xor);
	DEFINE_BIF("\\", 2, bif_iso_complement);
	DEFINE_BIF("abs", 1, bif_iso_abs);
	DEFINE_BIF("ceiling", 1, bif_iso_ceiling);
	DEFINE_BIF("sign", 1, bif_iso_sign);
	DEFINE_BIF("max", 2, bif_iso_max);
	DEFINE_BIF("min", 2, bif_iso_min);
	DEFINE_BIF("float_fractional_part", 1, bif_iso_float_fractional_part);
	DEFINE_BIF("float_integer_part", 1, bif_iso_float_integer_part);
	DEFINE_BIF("floor", 1, bif_iso_floor);
	DEFINE_BIF("exp", 1, bif_iso_exp);
	DEFINE_BIF("sqrt", 1, bif_iso_sqrt);
	DEFINE_BIF("log", 1, bif_iso_log);
	DEFINE_BIF("sin", 1, bif_iso_sin);
	DEFINE_BIF("asin", 1, bif_iso_asin);
	DEFINE_BIF("cos", 1, bif_iso_cos);
	DEFINE_BIF("acos", 1, bif_iso_acos);
	DEFINE_BIF("tan", 1, bif_iso_tan);
	DEFINE_BIF("atan", 1, bif_iso_atan);
	DEFINE_BIF("atan2", 1, bif_iso_atan_2);
	DEFINE_BIF("truncate", 1, bif_iso_truncate);
	DEFINE_BIF("round", 1, bif_iso_round);
	DEFINE_BIF("=..", 2, bif_iso_univ);
	DEFINE_BIF("atomic", 1, bif_iso_atomic);
	DEFINE_BIF("ground", 1, bif_iso_ground);
	DEFINE_BIF("var", 1, bif_iso_var);
	DEFINE_BIF("nonvar", 1, bif_iso_nonvar);
	DEFINE_BIF("atom", 1, bif_iso_atom);
	DEFINE_BIF("number", 1, bif_iso_number);
	DEFINE_BIF("integer", 1, bif_iso_integer);
	DEFINE_BIF("float", 1, bif_iso_float);
	DEFINE_BIF("compound", 1, bif_iso_compound);
	DEFINE_BIF("callable", 1, bif_iso_callable);
	DEFINE_BIF("stream_property_type", 2, bif_iso_stream_property_type);
	DEFINE_BIF("stream_property_mode", 2, bif_iso_stream_property_mode);
	DEFINE_BIF("stream_property_position", 2, bif_iso_stream_property_position);
	DEFINE_BIF("stream_property_file_name", 2, bif_iso_stream_property_file_name);
	DEFINE_BIF("copy_term", 2, bif_iso_copy_term);
	DEFINE_BIF("term_variables", 2, bif_iso_term_variables);
	DEFINE_BIF("functor", 3, bif_iso_functor);
	DEFINE_BIF("arg", 3, bif_iso_arg);
	DEFINE_BIF("clause", 2, bif_iso_clause);
	DEFINE_BIF("unify_with_occurs_check", 2, bif_iso_unify);
	DEFINE_BIF("atom_length", 2, bif_iso_atom_length);
	DEFINE_BIF("atom_concat", 3, bif_iso_atom_concat);
	DEFINE_BIF("sub_atom", 5, bif_iso_sub_atom);
	DEFINE_BIF("atom_chars", 2, bif_iso_atom_chars);
	DEFINE_BIF("atom_codes", 2, bif_iso_atom_codes);
	DEFINE_BIF("char_code", 2, bif_iso_char_code);
	DEFINE_BIF("number_codes", 2, bif_iso_number_codes);
	DEFINE_BIF("number_chars", 2, bif_iso_number_chars);
	DEFINE_BIF("open", 3, bif_iso_open_3);
	DEFINE_BIF("open", 4, bif_iso_open_4);
	DEFINE_BIF("close", 1, bif_iso_close);
	DEFINE_BIF("close", 2, bif_iso_close);
	DEFINE_BIF("write_canonical", 1, bif_iso_write_canonical);
	DEFINE_BIF("write_canonical", 2, bif_iso_write_canonical_2);
	DEFINE_BIF("write_term", 2, bif_iso_write_term);
	DEFINE_BIF("write_term", 3, bif_iso_write_term_3);
	DEFINE_BIF("writeq", 1, bif_iso_writeq);
	DEFINE_BIF("writeq", 2, bif_iso_writeq_2);
	DEFINE_BIF("write", 1, bif_iso_write);
	DEFINE_BIF("write", 2, bif_iso_write_2);
	DEFINE_BIF("nl", 0, bif_iso_nl);
	DEFINE_BIF("nl", 1, bif_iso_nl_1);
	DEFINE_BIF("read_term", 2, bif_iso_read_term_2);
	DEFINE_BIF("read_term", 3, bif_iso_read_term_3);
	DEFINE_BIF("read", 1, bif_iso_read_term_2);
	DEFINE_BIF("read", 2, bif_iso_read_term_3);
	DEFINE_BIF("put_char", 1, bif_iso_put_char);
	DEFINE_BIF("put_char", 2, bif_iso_put_char_2);
	DEFINE_BIF("put_byte", 1, bif_iso_put_byte);
	DEFINE_BIF("put_byte", 2, bif_iso_put_byte_2);
	DEFINE_BIF("put_code", 1, bif_iso_put_code);
	DEFINE_BIF("put_code", 2, bif_iso_put_code_2);
	DEFINE_BIF("flush_output", 0, bif_iso_flush_output);
	DEFINE_BIF("flush_output", 1, bif_iso_flush_output_1);
	DEFINE_BIF("get_code", 1, bif_iso_get_code);
	DEFINE_BIF("get_byte", 1, bif_iso_get_byte);
	DEFINE_BIF("get_char", 1, bif_iso_get_char);
	DEFINE_BIF("get_code", 2, bif_iso_get_code_2);
	DEFINE_BIF("get_byte", 2, bif_iso_get_byte_2);
	DEFINE_BIF("get_char", 2, bif_iso_get_char_2);
	DEFINE_BIF("peek_code", 1, bif_iso_peek_code);
	DEFINE_BIF("peek_byte", 1, bif_iso_peek_byte);
	DEFINE_BIF("peek_char", 1, bif_iso_peek_char);
	DEFINE_BIF("peek_code", 2, bif_iso_peek_code_2);
	DEFINE_BIF("peek_byte", 2, bif_iso_peek_byte_2);
	DEFINE_BIF("peek_char", 2, bif_iso_peek_char_2);
	DEFINE_BIF("at_end_of_stream", 0, bif_iso_at_end_of_stream);
	DEFINE_BIF("at_end_of_stream", 1, bif_iso_at_end_of_stream_1);
	DEFINE_BIF("set_stream_position", 2, bif_iso_set_stream_position);
	DEFINE_BIF("set_input", 1, bif_iso_set_input);
	DEFINE_BIF("set_output", 1, bif_iso_set_output);
	DEFINE_BIF("current_input", 1, bif_iso_current_input);
	DEFINE_BIF("current_output", 1, bif_iso_current_output);
	DEFINE_BIF("length", 2, bif_iso_length);
	DEFINE_BIF("sort", 2, bif_iso_sort);
	DEFINE_BIF("keysort", 2, bif_iso_keysort);
	DEFINE_BIF("asserta", 1, bif_iso_asserta);
	DEFINE_BIF("assertz", 1, bif_iso_assertz);
	DEFINE_BIF("retractall", 1, bif_iso_retractall);
	DEFINE_BIF("retract", 1, bif_iso_retract);
	DEFINE_BIF("abolish", 1, bif_iso_abolish);
	DEFINE_BIF("current_predicate", 1, bif_iso_current_predicate);
	DEFINE_BIF("current_op", 3, bif_iso_current_op);
	DEFINE_BIF("set_prolog_flag", 2, bif_iso_set_prolog_flag);
	DEFINE_BIF("current_prolog_flag", 2, bif_iso_current_prolog_flag);
	DEFINE_BIF("findall", 3, bif_iso_findall);
	DEFINE_BIF("bagof", 3 + 1, bif_iso_bagof);
	DEFINE_BIF("setof", 3 + 1, bif_iso_setof);
	DEFINE_BIF("compare", 3, bif_iso_compare);

	// DEFINE_BIF("include", 1, bif_iso_include);
	DEFINE_BIF("dynamic", 1, bif_iso_dynamic);
	DEFINE_BIF("op", 3, bif_iso_op);

// DEFINE_BIF("stream_property", 2, bif_iso_stream_property);

#ifndef ISO_ONLY
	DEFINE_BIF("unload_file", 1, bif_xtra_unload_file);
	DEFINE_BIF("using", 1, bif_xtra_using);
	DEFINE_BIF("use_module", 1, bif_xtra_use_module);
	DEFINE_BIF("import", 1, bif_xtra_use_module);
	DEFINE_BIF("retractw", 1, bif_xtra_retractw);
	DEFINE_BIF("clausew", 2, bif_xtra_clausew);
	DEFINE_BIF("clause", 3, bif_iso_clause);
	DEFINE_BIF("listing_canonical", 0, bif_xtra_listing_canonical);
	DEFINE_BIF("listing_canonical", 1, bif_xtra_listing_canonical);
	DEFINE_BIF("term_to_blob", 2, bif_xtra_term_to_blob);
	DEFINE_BIF("enter", 1, bif_xtra_enter);

	// These are used in the database log...

	DEFINE_BIF("a_", 1, bif_iso_asserta);
	DEFINE_BIF("z_", 1, bif_iso_assertz);
	DEFINE_BIF("r_", 1, bif_iso_retract);
	DEFINE_BIF("t_", 0, bif_iso_true);
#endif

	// These are not ISO-Prolog but are common...

	DEFINE_BIF("not", 1, bif_iso_not);
	DEFINE_BIF("div", 2, bif_iso_divint);
	DEFINE_BIF("assert", 1, bif_iso_assertz);

#ifndef ISO_ONLY
	DEFINE_BIF("consult", 1, bif_xtra_consult_1);
	DEFINE_BIF("deconsult", 1, bif_xtra_deconsult_1);
	DEFINE_BIF("reconsult", 1, bif_xtra_reconsult_1);
	DEFINE_BIF("listing", 0, bif_xtra_listing);
	DEFINE_BIF("listing", 1, bif_xtra_listing);
	DEFINE_BIF("abolish", 2, bif_xtra_abolish_2);
	DEFINE_BIF("writeln", 1, bif_xtra_writeln_1);
	DEFINE_BIF("writeln", 2, bif_xtra_writeln_2);
	DEFINE_BIF("is_list", 1, bif_xtra_is_list_1);
	DEFINE_BIF("is_struct", 1, bif_xtra_is_struct_1);
	DEFINE_BIF("is_tuple", 1, bif_xtra_is_tuple_1);
	DEFINE_BIF("is_stream", 1, bif_xtra_is_stream_1);
	DEFINE_BIF("term_to_atom", 2, bif_xtra_term_to_atom_2);
	DEFINE_BIF("findnsols", 1 + 4, bif_xtra_findnsols_4);
	DEFINE_BIF("maplist", 2, bif_xtra_maplist_2);
	DEFINE_BIF("maplist", 3, bif_xtra_maplist_3);
	DEFINE_BIF("asserta", 2, bif_xtra_asserta_2);
	DEFINE_BIF("assertz", 2, bif_xtra_assertz_2);
	DEFINE_BIF("erase", 1, bif_xtra_erase_1);
	DEFINE_BIF("random", 1, bif_xtra_random_1);
	DEFINE_BIF("term_hash", 2, bif_xtra_term_hash_2);
	DEFINE_BIF("read_term_from_atom", 3, bif_xtra_read_term_from_atom_3);
	DEFINE_BIF("atom_number", 2, bif_xtra_atom_number_2);
	DEFINE_BIF("trace", 0, bif_xtra_trace_0);
	DEFINE_BIF("trace", 1, bif_xtra_trace_1);
	DEFINE_BIF("getenv", 2, bif_xtra_getenv_2);
	DEFINE_BIF("setenv", 2, bif_xtra_setenv_2);
	DEFINE_BIF("unsetenv", 1, bif_xtra_unsetenv_1);
	DEFINE_BIF("exists_file", 1, bif_xtra_exists_file_1);
	DEFINE_BIF("delete_file", 1, bif_xtra_delete_file_1);
	DEFINE_BIF("rename_file", 2, bif_xtra_rename_file_2);
	DEFINE_BIF("make_directory", 1, bif_xtra_make_directory_1);
	DEFINE_BIF("memberchk", 2, bif_xtra_memberchk_2);
	DEFINE_BIF("name", 2, bif_xtra_name_2);

#if USE_SSL
	DEFINE_BIF("unbounded", 1, bif_xtra_unbounded_1);
#endif

	DEFINE_BIF("forall", 2, bif_xtra_forall_2);
	DEFINE_BIF("time", 1, bif_xtra_time_1);
	DEFINE_BIF("time", 2, bif_xtra_time_2);
	DEFINE_BIF("between", 3, bif_xtra_between_3);
	DEFINE_BIF("phrase", 1 + 2, bif_xtra_phrase);
	DEFINE_BIF("phrase", 1 + 3, bif_xtra_phrase);
	DEFINE_BIF("predicate_property", 2, bif_xtra_predicate_property_2);
#endif

// These are for Edinburgh-style file handling...

#ifndef ISO_ONLY
	DEFINE_BIF("see", 1, bif_xtra_see_1);
	DEFINE_BIF("seeing", 1, bif_xtra_seeing_1);
	DEFINE_BIF("seen", 0, bif_xtra_seen_0);
	DEFINE_BIF("tell", 1, bif_xtra_tell_1);
	DEFINE_BIF("append", 1, bif_xtra_append_1);
	DEFINE_BIF("telling", 1, bif_xtra_telling_1);
	DEFINE_BIF("told", 0, bif_xtra_told_0);
	DEFINE_BIF("tab", 1, bif_xtra_tab_1);
	DEFINE_BIF("tab", 2, bif_xtra_tab_2);
	DEFINE_BIF("get0", 1, bif_iso_get_code);
	DEFINE_BIF("get0", 2, bif_iso_get_code);
	DEFINE_BIF("get", 1, bif_xtra_get_1);
	DEFINE_BIF("get", 2, bif_xtra_get_2);
#endif

#ifndef ISO_ONLY
	DEFINE_BIF("linda:out", 1, bif_linda_out);
	DEFINE_BIF("linda:in", 1, bif_xtra_retractw);
	DEFINE_BIF("linda:inp", 1, bif_iso_retract);
	DEFINE_BIF("linda:rd", 1, bif_xtra_clausew);
	DEFINE_BIF("linda:rdp", 1, bif_iso_clause);
#endif
}
