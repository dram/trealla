#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <float.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#define snprintf _snprintf
#define fseeko _fseeki64
#define ftello _ftelli64
#else
#include <unistd.h>
#include <sys/time.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

#define END_OF_FILE "end_of_file"

funcs g_bifs[MAX_BIFS] = {{0}};
size_t g_bifs_idx = 0;

#ifdef _WIN32
static char *strndup (const char *s, size_t n)
{
	size_t len = strlen(s);
	if (n < len) len = n;
	char *dstbuf = (char*)malloc(len+1);
	dstbuf[len] = '\0';
	return (char*)memcpy(dstbuf, s, len);
}
#endif

void reset_arg(tpl_query *q, const node *term, unsigned frame)
{
	env *e = get_env(q, frame+term->slot);

	if (e->term)
	{
		term_heapcheck(e->term);
		e->term = NULL;
	}

	e->binding = 0;
}

static node *copy_nbr(tpl_query *q, node *from)
{
	node *n = new_node();
	n->flags |= from->flags;

	if (is_stream(from))
	{
		n->val_ptr = from->val_ptr;
		n->flags |= FLAG_CONST;
	}
	else if (is_float(from))
		n->val_f = from->val_f;
	else
		n->val_i = from->val_i;

	return n;
}

static node *copy_atom(tpl_query *q, node *from)
{
	node *n = new_node();
	n->flags |= from->flags;

	if (from->flags & FLAG_BLOB)
	{
		n->val_s = (char*)malloc(from->val_len+1);
		memcpy(n->val_s, from->val_s, from->val_len);
		n->val_len = from->val_len;
		n->val_s[n->val_len] = '\0';
		n->flags &= ~FLAG_CONST;
	}
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

static node *copy_var(tpl_query *q, node *from)
{
	node *n = new_node();
	n->flags |= from->flags|FLAG_CONST;

	if (from->flags & FLAG_CONST)
		n->val_s = from->val_s;
	else
		n->val_s = strdup(from->val_s);

	n->slot = from->slot;
	return n;
}

node *copy_term2(tpl_query *q, node *from, int clone, int depth)
{
	if (depth > (1024*1024))
		{ QABORT2(ABORT_MAXDEPTH,"COPY_TERM"); return 0; }

	if (is_number(from))
		return copy_nbr(q, from);

	if (is_atom(from))
		return copy_atom(q, from);

	if (is_var(from))
	{
		if (clone)
			return copy_var(q, from);

		env *e = get_env(q, q->latest_context+from->slot);
		node *tmp;

		if (!q->d)
			{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

		if (!sl_get(q->d, (char*)e, (void**)&tmp))
			sl_set(q->d, (char*)e, tmp=make_var(q));
		else
			tmp = copy_var(q, tmp);

		return tmp;
	}

	if (!is_compound(from))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	node *n = make_structure();
	n->flags |= from->flags;
	n->bifptr = from->bifptr;
	n->frame_size = from->frame_size;
	from = NLIST_FRONT(&from->val_l);
	int this_context = q->latest_context;

	while (from)
	{
		node *from2 = get_arg(q, from, this_context);
		node *tmp = copy_term2(q, from2, clone, depth+1);
		NLIST_PUSH_BACK(&n->val_l, tmp);
		from = NLIST_NEXT(from);
	}

	return n;
}

const funcs *get_bif(lexer *l, const char *functor)
{
	const funcs *fptr;
	const char *u = NULL;
	sl_start(&l->ns);

	do
	{
		char tmpbuf[(FUNCTOR_SIZE*2)+10];

		if (u != NULL)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", u, functor);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", functor);

		for (fptr = g_bifs; fptr->functor; fptr++)
		{
			if (!strcmp(fptr->functor, tmpbuf))
				return fptr;
		}
	}
	 while ((u = sl_next(&l->ns, NULL)) != NULL);

	return fptr;
}

const funcs *get_bifarity(lexer *l, const char *functor, int arity)
{
	const funcs *fptr = NULL;
	const char *u = NULL;
	sl_start(&l->ns);

	do
	{
		char tmpbuf[(FUNCTOR_SIZE*2)+10];

		if (u != NULL)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", u, functor);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", functor);

		for (fptr = g_bifs; fptr->functor; fptr++)
		{
			if ((fptr->arity != -1) && (fptr->arity != arity))
				continue;

			if (!strcmp(fptr->functor, tmpbuf))
				return fptr;
		}
	}
	 while ((u = sl_next(&l->ns, NULL)) != NULL);

	return fptr;
}

static int check_builtin(trealla *pl, const char *functarity)
{
	char tmpbuf[FUNCTOR_SIZE];
	tmpbuf[0] = '\0';
	int arity = 0;
	sscanf(functarity, "%[^/]/%d", tmpbuf, &arity);
	tmpbuf[sizeof(tmpbuf)-1] = '\0';
	const funcs *fptr;

	for (fptr = g_bifs; fptr->functor; fptr++)
	{
		if (!strcmp(fptr->functor, tmpbuf) &&
			((fptr->arity == arity) || (fptr->arity == -1)))
			break;
	}

	return fptr->functor != NULL;
}

static int check_dynamic(module *db, const char *functarity)
{
	char *key = (char*)functarity;
	rule *r = NULL;

	if (sl_get(&db->rules, key, (void**)&r))
		return r->dynamic;

	return 0;
}

int bif_iso_true(tpl_query *q)
{
	return 1;
}

int bif_iso_fail(tpl_query *q)
{
	return 0;
}

static int bif_iso_halt(tpl_query *q)
{
	QABORT(ABORT_HALT);
	q->halt = ABORT_HALT;
	return 1;
}

static int bif_iso_halt1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	QABORT(ABORT_HALT);
	q->halt = term1->val_i;
	return 1;
}

int bif_iso_cut(tpl_query *q)
{
	trust_me(q);
	return 1;
}

int bif_iso_cutfail(tpl_query *q)
{
	trust_me(q);
	return 0;
}

static int bif_iso_repeat(tpl_query *q)
{
	if (!q->retry) allocate_frame(q);
	try_me_nofollow(q);
	return 1;
}

int bif_iso_and(tpl_query *q)
{
	node *args = get_args(q);
	q->curr_term = args;
	return 1;
}

static int bif_iso_not(tpl_query *q)
{
	if (q->retry) return 1;
	node *args = get_args(q);
	allocate_frame(q);
	try_me(q);
	q->curr_term = args;
	return 1;
}

static int bif_iso_do(tpl_query *q)
{
	node *args = get_args(q);
	q->curr_term = args;
	trust_me(q);
	return 1;
}

static int bif_iso_then(tpl_query *q)
{
	if (q->retry) return 0;
	node *args = get_args(q);
	node *term1 = get_callable(term1);

	if (!(q->curr_term->flags & FLAG_NOFOLLOW))
	{
		allocate_frame(q);
		try_me(q);
	}

	q->curr_term = term1;
	return call(q);
}

static int bif_iso_or(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term1x = get_callable(term1x);
	node *term2 = get_callable(term2);

	if (!q->retry)
	{
		allocate_frame(q);
		try_me(q);
		q->curr_term = term1;
	}
	else
	{
		q->retry = 0;
		try_me_nochoice(q);
		q->curr_term = term2;
	}

	q->curr_match = NULL;
	return call(q);
}

static int bif_iso_once(tpl_query *q)
{
	if (q->retry) return 0;
	node *args = get_args(q);
	allocate_frame(q);
	try_me(q);
	q->curr_term = args;
	return 1;
}

static int bif_iso_call(tpl_query *q)
{
	if (q->retry) return 0;
	node *args = get_args(q);
	node *var = get_var(var);				// FLAG_HIDDEN
	node *term1 = get_callable(term1);
	allocate_frame(q);
	try_me(q);
	q->curr_term = term1;
	return call(q);
}

static int bif_iso_calln(tpl_query *q)
{
	if (q->retry) return 0;
	node *args = get_args(q);
	node *var = get_var(var);				// FLAG_HIDDEN
	node *term1 = get_atom(term1);
	node *param = get_next_arg(q, &args);

	node *tmp = make_structure();
	tmp->hdr.next = NULL;
	NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, term1));

	while (param)
	{
		NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, param));
		param = get_next_arg(q, &args);
	}

	const char *functor = term1->val_s;
	int arity = NLIST_COUNT(&tmp->val_l)-1;
	tmp->bifptr = get_bifarity(&q->pl->lex, functor, arity)->bifptr;

	if (!tmp->bifptr)
		tmp->match = xref_term(&q->pl->lex, term1, arity);
	else
		tmp->flags |= FLAG_BUILTIN;

	put_env(q, q->curr_frame+var->slot, tmp, -1);
	tmp->refcnt--;
	allocate_frame(q);
	try_me(q);
	q->curr_term = tmp;
	return call(q);
}

static int check_vars_term(tpl_query *q, node *n);

static int check_vars_compound(tpl_query *q, node *n)
{
	int cnt = 0;

	for (n = NLIST_FRONT(&n->val_l); n; n = NLIST_NEXT(n))
		cnt += check_vars_term(q, n);

	return cnt;
}

static int check_vars_term(tpl_query *q, node *n)
{
	n = get_arg(q, n, q->curr_frame);

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
	return check_vars_term(q, term1) ? 0 : 1;
}

static int bif_iso_var(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_var(term1) ? 1 : 0;
}

static int bif_iso_nonvar(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_var(term1) ? 0 : 1;
}

static int bif_iso_atomic(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_atomic(term1) ? 1 : 0;
}

static int bif_iso_atom(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_atom(term1) ? 1 : 0;
}

static int bif_iso_number(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_number(term1) ? 1 : 0;
}

static int bif_iso_compound(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_compound(term1) ? 1 : 0;
}

static int bif_iso_callable(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_callable(term1);
}

static int bif_iso_unify(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	int save_context = q->latest_context;
	node *term2 = get_term(term2);
	q->curr_context = save_context;
	q->is_det = 0;
	return unify_term(q, term1, term2, q->curr_frame);
}

static int bif_iso_notunify(tpl_query *q) { return !bif_iso_unify(q); }
static int compare_terms(tpl_query *q, node *term1, node *term2, int mode);

enum { CMP_LT=0, CMP_LE=1, CMP_EQ=2 };

static int compare_compounds(tpl_query *q, node *term1, node *term2, int mode)
{
	node *n1 = NLIST_FRONT(&term1->val_l), *n2 = NLIST_FRONT(&term2->val_l);

	while (n1 && n2)
	{
		int status = compare_terms(q, n1, n2, mode);

		if (status > 0)
			return 1;

		if ((mode == CMP_EQ) && (status != 0))
			return -1;

		if ((mode == CMP_LE) && (status < 0))
			return -1;

		if ((mode == CMP_LT) && (status < 0))
			return -1;

		n1 = NLIST_NEXT(n1);
		n2 = NLIST_NEXT(n2);
	}

	return 0;
}

static int compare_terms(tpl_query *q, node *term1, node *term2, int mode)
{
	node *n1 = get_arg(q, term1, q->curr_frame);
	node *n2 = get_arg(q, term2, q->curr_frame);

	if (is_integer(n1))
	{
		if (!is_integer(n2)) { QABORT(ABORT_INVALIDARGNOTINT); return 0; }
		if (n1->val_i < n2->val_i) return -1;
		if (n1->val_i == n2->val_i) return 0;
		return 1;
	}
	else if (is_float(n1))
	{
		if (!is_float(n2)) { QABORT(ABORT_INVALIDARGNOTFLOAT); return 0; }
		if (n1->val_f < n2->val_f) return -1;
		if (n1->val_f == n2->val_f) return 0;
		return 1;
	}
	else if (is_atom(n1))
	{
		if (!is_atom(n2)) { QABORT(ABORT_INVALIDARGNOTATOM); return 0; }
		return strcmp(n1->val_s, n2->val_s);
	}
	else if (is_var(n1))
	{
		if (!is_var(n2)) { QABORT(ABORT_INVALIDARGNOTVAR); return 0; }
		if (n1->slot < n2->slot) return -1;
		if (n1->slot == n2->slot) return 0;
		return 1;
	}
	else if (is_compound(n1))
	{
		if (!is_compound(n2)) { QABORT(ABORT_INVALIDARGNOTCOMPOUND); return 0; }
		return compare_compounds(q, n1, n2, mode);
	}

	return 0;
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
	size_t len = UTF8LEN(term1);

	if (is_var(term2))
	{
		put_int(q, q->curr_frame+term2->slot, len);
		return 1;
	}

	return len == (size_t)term2->val_i;
}

static int bif_iso_atom_concat(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	size_t len = LEN(term1) + LEN(term2);
	char *tmp = (char*)malloc(len+1);

	if (LEN(term1))
		memcpy(tmp, term1->val_ptr, LEN(term1));

	if (LEN(term2))
		memcpy(tmp+LEN(term1), term2->val_ptr, LEN(term2));

	tmp[len] = '\0';
	node *n;

#ifndef ISO_ONLY
	if (is_blob(term1) || is_blob(term2))
		n = make_blob(tmp, len);
	else
#endif
		n = make_atom(tmp, 1);

	put_env(q, q->curr_frame+term3->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_iso_curr_predicate(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return check_dynamic(q->curr_db, term1->val_s);
}

static int bif_iso_set_prolog_flag(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	const char *flag = term1->val_s;

	if (!strcmp(flag, "char_conversion")) q->pl->flag_char_conversion = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "debug")) q->pl->flag_debug = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "unknown")) q->pl->flag_unknown = !strcmp(term2->val_s,"error")?1:0;
	else if (!strcmp(flag, "double_quotes")) q->pl->flag_double_quotes = !strcmp(term2->val_s,"atom")?1:0;
	else if (!strcmp(flag, "character_escapes")) q->pl->flag_character_escapes = !strcmp(term2->val_s,"true")?1:0;
	else return 0;

	return 1;
}

static int bif_iso_curr_prolog_flag(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	const char *flag = term1->val_s;

	if (!strcmp(flag, "bounded")) put_const_atom(q, q->curr_frame+term2->slot, "false", 0);
	else if (!strcmp(flag, "max_integer")) put_int(q, q->curr_frame+term2->slot, INT_MAX);
	else if (!strcmp(flag, "min_integer"))	put_int(q, q->curr_frame+term2->slot, INT_MIN);
	else if (!strcmp(flag, "integer_rounding_function")) put_const_atom(q, q->curr_frame+term2->slot, "down", 0);
	else if (!strcmp(flag, "max_arity")) put_int(q, q->curr_frame+term2->slot, MAX_FRAME_SIZE-1);
	else if (!strcmp(flag, "char_conversion")) put_const_atom(q, q->curr_frame+term2->slot, q->pl->flag_char_conversion?"true":"false", 0);
	else if (!strcmp(flag, "debug")) put_const_atom(q, q->curr_frame+term2->slot, q->pl->flag_debug?"true":"false", 0);
	else if (!strcmp(flag, "unknown")) put_const_atom(q, q->curr_frame+term2->slot, q->pl->flag_unknown?"error":"none", 0);
	else if (!strcmp(flag, "double_quotes")) put_const_atom(q, q->curr_frame+term2->slot, q->pl->flag_double_quotes?"atom":"none", 0);
	else if (!strcmp(flag, "character_escapes")) put_const_atom(q, q->curr_frame+term2->slot, q->pl->flag_character_escapes?"true":"false", 0);
	else return 0;

	return 1;
}

static int bif_iso_predicate_property(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_var(term2);

	if (!is_atom(term1) || !is_atom(term2))
		return 0;

	const char *functarity = term1->val_s;
	const char *property = term2->val_s;

	if (!strcmp(property, "dynamic") && check_dynamic(q->curr_db, functarity))
		return 1;

	if (!strcmp(property, "static") && !check_dynamic(q->curr_db, functarity))
		return 1;

	if (!strcmp(property, "built_in") && check_builtin(q->pl, functarity))
		return 1;

	return 0;
}

static int bif_iso_open3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	const char *filename = term1->val_s;
	const char *mode = term2->val_s;
	const char *type = "text";
	char tmpbuf[40];
	strcpy(tmpbuf, !strcmp(mode, "append")?"a":
					!strcmp(mode, "update")?"r+":
					!strcmp(mode, "write")?"w+":
					"r");

	if (!strcmp(type, "binary"))
		strcat(tmpbuf, "b");

	FILE *fp = fopen(filename, tmpbuf);
	if (fp == NULL) return 0;
	stream *sp = CALLOC(stream);
	sp->fptr = fp;
	sp->filename = strdup(filename);
	sp->mode = strdup(mode);
	sp->type = strdup(type);
	node *n = make_stream(sp);
	n->flags |= FLAG_FILE;
	put_env(q, q->curr_frame+term3->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_iso_open4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	node *term4 = get_atom(term4);
	const char *filename = term1->val_s;
	const char *mode = term2->val_s;
	const char *type = term4->val_s;
	char tmpbuf[40];
	strcpy(tmpbuf, !strcmp(mode, "append")?"a":
					!strcmp(mode, "update")?"r+":
					!strcmp(mode, "write")?"w+":
					"r");

	if (!strcmp(type, "type(binary)"))
	{
		strcat(tmpbuf, "b");
		type = "binary";
	}
	else
		type = "text";

	FILE *fp = fopen(filename, tmpbuf);
	if (fp == NULL) return 0;
	stream *sp = CALLOC(stream);
	sp->fptr = fp;
	sp->filename = strdup(filename);
	sp->mode = strdup(mode);
	sp->type = strdup(type);
	node *n = make_stream(sp);
	n->flags |= FLAG_FILE;
	put_env(q, q->curr_frame+term3->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_iso_close(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	stream *sp = term1->val_str;

#ifndef ISO_ONLY
	if (is_socket(term1))
	{
		session_close((session*)sp->sptr);

		if (session_is_client((session*)sp->sptr))
			sp->sptr = NULL;
	}
	else
#endif
	if (sp->fptr)
		fclose(sp->fptr);

	return 1;
}

static int bif_iso_write_term3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	node *term3 = get_atom_or_list(term3);
	stream *sp = term1->val_str;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	int quoted = 0, nl = 0, fs = 0;
#ifndef ISO_ONLY
	int dots = 0;
	const char *save_dots = g_list_cons;
#endif

	if (is_atom(term2))
	{
		if (strcmp(term2->val_s, "[]"))
		{
			free(tmpbuf);
			return 0;
		}
	}
	else
	{
		char tmpbuf[1024];
		sprint_term(tmpbuf, sizeof(tmpbuf), q->pl, q, term2, 0);

		if (strstr(tmpbuf, "quoted(true)"))
			quoted = 1;

		if (strstr(tmpbuf, "fullstop(true)"))
			fs = 1;

		if (strstr(tmpbuf, "nl(true)"))
			nl = 1;

#ifndef ISO_ONLY
		if (strstr(tmpbuf, "dotlists(true)"))
		{
			g_list_cons = ".";
			dots = 1;
		}
#endif
	}

	size_t len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term2, quoted);

	if (q->halt)
	{
		free(tmpbuf);
		return 0;
	}

#ifndef ISO_ONLY
	if (dots)
		g_list_cons = save_dots;
#endif

	if (fs)
	{
		tmpbuf[len++] = '.';
		tmpbuf[len] = '\0';
	}

	if (nl)
	{
		tmpbuf[len++] = '\n';
		tmpbuf[len] = '\0';
	}

	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, sp->fptr);

	free(tmpbuf);
	return ok > 0;
}

static int bif_iso_write_term(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_atom_or_list(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	int quoted = 0, nl = 0, fs = 0;

	if (is_atom(term2))
	{
		if (strcmp(term2->val_s, "[]"))
		{
			free(tmpbuf);
			return 0;
		}
	}
	else
	{
		char tmpbuf[1024];
		sprint_term(tmpbuf, sizeof(tmpbuf), q->pl, q, term2, 0);

		if (strstr(tmpbuf, "quoted(true)"))
			quoted = 1;

		if (strstr(tmpbuf, "fullstop(true)"))
			fs = 1;

		if (strstr(tmpbuf, "nl(true)"))
			nl = 1;
	}

	size_t len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, quoted);

	if (q->halt)
	{
		free(tmpbuf);
		return 0;
	}

	if (fs)
	{
		tmpbuf[len++] = '.';
		tmpbuf[len] = '\0';
	}

	if (nl)
	{
		tmpbuf[len++] = '\n';
		tmpbuf[len] = '\0';
	}

	fwrite(tmpbuf, 1, len, stdout);
	free(tmpbuf);
	return 1;
}

static int bif_iso_write_canonical2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	size_t len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term2, 2);

	if (q->halt)
	{
		free(tmpbuf);
		return 0;
	}

	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, sp->fptr);

	free(tmpbuf);
	return ok > 0;
}

static int bif_iso_write_canonical(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	print_term(q->pl, q, term1, 2);

	if (q->halt)
		return 0;

	return 1;
}

static int bif_iso_writeq2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	size_t len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term2, 1);

	if (q->halt)
	{
		free(tmpbuf);
		return 0;
	}

	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, sp->fptr);

	free(tmpbuf);
	return ok > 0;
}

static int bif_iso_writeq(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	print_term(q->pl, q, term1, 1);

	if (q->halt)
		return 0;

	return 1;
}

static int bif_iso_write2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	char *tmpbuf;
	size_t len;

	if (is_atom(term2))
	{
		tmpbuf = term2->val_s;
		len = LEN(term2);
	}
	else
	{
		size_t max_len = PRINTBUF_SIZE;
		tmpbuf = (char*)malloc(max_len+1);
		char *dst = tmpbuf;
		len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term2, 0);

		if (q->halt)
		{
			free(tmpbuf);
			return 0;
		}
	}

	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, sp->fptr);

	if (!is_atom(term2))
		free(tmpbuf);

	return ok > 0;
}

static int bif_iso_write(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	char *tmpbuf;
	size_t len;

	if (is_atom(term1))
	{
		tmpbuf = term1->val_s;
		len = LEN(term1);
	}
	else
	{
		size_t max_len = PRINTBUF_SIZE;
		tmpbuf = (char*)malloc(max_len+1);
		char *dst = tmpbuf;
		len = sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);

		if (q->halt)
		{
			free(tmpbuf);
			return 0;
		}
	}

	fwrite(tmpbuf, 1, len, stdout);

	if (!is_atom(term1))
		free(tmpbuf);

	return 1;
}

static int bif_iso_nl1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	stream *sp = term1->val_str;
	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, "\n", 1);
	else
#endif
		ok = fwrite("\n", 1, 1, sp->fptr);

	return ok > 0;
}

static int bif_iso_nl(tpl_query *q)
{
	fwrite("\n", 1, 1, stdout);
	return 1;
}

static int bif_iso_read2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	char *line = NULL;

#ifndef ISO_ONLY
	if (is_socket(term1))
	{
		if (!session_readmsg((session*)sp->sptr, &line))
		{
			q->is_yielded = 1;
			return 0;
		}

		if (session_on_disconnect((session*)sp->sptr))
		{
			put_const_atom(q, q->curr_frame+term2->slot, END_OF_FILE, 0);
			return 1;
		}
	}
	else
#endif
	{
		if ((line = trealla_readline(sp->fptr)) == NULL)
		{
			put_const_atom(q, q->curr_frame+term2->slot, END_OF_FILE, 0);
			return 1;
		}
	}

	if (!line[0]) return 0;
	char *tmpbuf = (char*)malloc(strlen(line)+10);
	sprintf(tmpbuf, "?- %s", line);
	free(line);
	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, tmpbuf, NULL);
	free(tmpbuf);
	xref_rule(&l, l.r);
	node *term = NLIST_FRONT(&l.r->val_l);
	term = copy_term(q, NLIST_NEXT(term));
	term_heapcheck(l.r);
	lexer_done(&l);
	int ok = unify_term(q, term1, term, q->curr_frame);
	term_heapcheck(term);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_read(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	char *line;

	if ((line = trealla_readline(stdin)) == NULL)
	{
		put_const_atom(q, q->curr_frame+term1->slot, END_OF_FILE, 0);
		return 1;
	}

	line[strlen(line)-1] = '\0';
	if (!line[0]) return 0;
	char *tmpbuf = (char*)malloc(strlen(line)+10);
	sprintf(tmpbuf, "?- %s", line);
	free(line);
	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, tmpbuf, NULL);
	free(tmpbuf);
	xref_rule(&l, l.r);
	node *term = NLIST_FRONT(&l.r->val_l);
	term = copy_term(q, NLIST_NEXT(term));
	term_heapcheck(l.r);
	lexer_done(&l);
	int ok = unify_term(q, term1, term, q->curr_frame);
	term_heapcheck(term);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_read_term3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	node *term3 = get_atom_or_list(term3);
	stream *sp = term1->val_str;
	char *line = NULL;

#ifndef ISO_ONLY
	if (is_socket(term1))
	{
		if (!session_readmsg((session*)sp->sptr, &line))
		{
			q->is_yielded = 1;
			return 0;
		}

		if (session_on_disconnect((session*)sp->sptr))
		{
			put_const_atom(q, q->curr_frame+term2->slot, END_OF_FILE, 0);
			return 1;
		}
	}
	else
#endif
	{
		if ((line = trealla_readline(sp->fptr)) == NULL)
		{
			put_const_atom(q, q->curr_frame+term2->slot, END_OF_FILE, 0);
			return 1;
		}
	}

	if (!line[0]) return 0;
	char *tmpbuf = (char*)malloc(strlen(line)+10);
	sprintf(tmpbuf, "?- %s", line);
	free(line);
	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, tmpbuf, NULL);
	free(tmpbuf);
	xref_rule(&l, l.r);
	node *term = NLIST_FRONT(&l.r->val_l);
	term = copy_term(q, NLIST_NEXT(term));
	term_heapcheck(l.r);
	lexer_done(&l);
	int ok = unify_term(q, term1, term, q->curr_frame);
	term_heapcheck(term);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_read_term(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	node *term2 = get_atom_or_list(term2);
	char *line;

	if ((line = trealla_readline(stdin)) == NULL)
	{
		put_const_atom(q, q->curr_frame+term1->slot, END_OF_FILE, 0);
		return 1;
	}

	line[strlen(line)-1] = '\0';
	if (!line[0]) return 0;
	char *tmpbuf = (char*)malloc(strlen(line)+10);
	sprintf(tmpbuf, "?- %s", line);
	free(line);
	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, tmpbuf, NULL);
	free(tmpbuf);
	xref_rule(&l, l.r);
	node *term = NLIST_FRONT(&l.r->val_l);
	term = copy_term(q, NLIST_NEXT(term));
	term_heapcheck(l.r);
	lexer_done(&l);
	int ok = unify_term(q, term1, term, q->curr_frame);
	term_heapcheck(term);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_flush_output1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	stream *sp = term1->val_str;
	if (is_file(term1)) fflush(sp->fptr);
	return 1;
}

static int bif_iso_flush_output(tpl_query *q)
{
	fflush(stdout);
	return 1;
}

static int bif_iso_at_end_of_stream1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	stream *sp = term1->val_str;

#ifndef ISO_ONLY
	if (is_socket(term1))
		return session_on_disconnect((session*)sp->sptr);
	else
#endif
		return feof(sp->fptr) > 0;
}

static int bif_iso_at_end_of_stream(tpl_query *q)
{
	return feof(stdin) > 0;
}

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
	put_int(q, q->curr_frame+term2->slot, ftello(sp->fptr));
	return 1;
}

static int bif_iso_stream_property_file_name(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->curr_frame+term2->slot, strdup(sp->filename), 1);
	return 1;
}

static int bif_iso_stream_property_mode(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->curr_frame+term2->slot, strdup(sp->mode), 1);
	return 1;
}

static int bif_iso_stream_property_type(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	put_atom(q, q->curr_frame+term2->slot, strdup(sp->type), 1);
	return 1;
}

static int bif_iso_put_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	fwrite(term1->val_s, 1, 1, stdout);
	return 1;
}

static int bif_iso_put_char2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;
	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, term2->val_s, 1);
	else
#endif
		ok = fwrite(term2->val_s, 1, 1, sp->fptr);

	return ok > 0;
}

static int bif_iso_put_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	fwrite(term1->val_s, 1, 1, stdout);
	return 1;
}

static int bif_iso_put_byte2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;
	int ok;

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, term2->val_s, 1);
	else
#endif
		ok = fwrite(term2->val_s, 1, 1, sp->fptr);

	return ok > 0;
}

static int bif_iso_put_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	char tmpbuf[20];
	sprintf(tmpbuf, "%c", (int)term1->val_i);
	fwrite(tmpbuf, 1, 1, stdout);
	return 1;
}

static int bif_iso_put_code2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_int(term2);
	stream *sp = term1->val_str;
	int ok;

	char tmpbuf[20];
	sprintf(tmpbuf, "%c", (int)term1->val_i);

#ifndef ISO_ONLY
	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, 1);
	else
#endif
		ok = fwrite(tmpbuf, 1, 1, sp->fptr);

	return ok > 0;
}

static int bif_iso_get_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char ch = getc(stdin);
	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_get_code2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char ch = getc(sp->fptr);
	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_get_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char ch = getc(stdin);
	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_get_byte2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char ch = getc(sp->fptr);
	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_get_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char line[2];
	line[0] = getc(stdin);

	if (line[0] == EOF)
	{
		node *n = make_const_atom(END_OF_FILE, 0);
		int ok = unify_term(q, term1, n, q->curr_frame);
		term_heapcheck(n);
		return ok;
	}

	line[1] = '\0';
	node *n = make_atom(strdup(line), 1);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_get_char2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char line[2];
	line[0] = getc(sp->fptr);

	if (line[0] == EOF)
	{
		node *n = make_const_atom(END_OF_FILE, 0);
		int ok = unify_term(q, term1, n, q->curr_frame);
		term_heapcheck(n);
		if (ok) q->is_det = 1;
		return ok;
	}

	line[1] = '\0';
	node *n = make_atom(strdup(line), 0);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char ch = getc(stdin);

	if (ch != EOF)
		ungetc(ch, stdin);

	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_code2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char ch = getc(sp->fptr);

	if (ch != EOF)
		ungetc(ch, stdin);

	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_byte(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char ch = getc(stdin);

	if (ch != EOF)
		ungetc(ch, stdin);

	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_byte2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char ch = getc(sp->fptr);

	if (ch != EOF)
		ungetc(ch, stdin);

	node *n = make_quick_int(ch);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_char(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char line[2];
	line[0] = getc(stdin);

	if (line[0] == EOF)
	{
		node *n = make_const_atom(END_OF_FILE, 0);
		int ok = unify_term(q, term1, n, q->curr_frame);
		term_heapcheck(n);
		if (ok) q->is_det = 1;
		return ok;
	}

	ungetc(line[0], stdin);
	line[1] = '\0';
	node *n = make_atom(strdup(line), 0);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_peek_char2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char line[2];
	line[0] = getc(sp->fptr);

	if (line[0] == EOF)
	{
		node *n = make_const_atom(END_OF_FILE, 0);
		int ok = unify_term(q, term1, n, q->curr_frame);
		term_heapcheck(n);
		if (ok) q->is_det = 1;
		return ok;
	}

	ungetc(line[0], sp->fptr);
	line[1] = '\0';
	node *n = make_atom(strdup(line), 0);
	int ok = unify_term(q, term1, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_number_codes(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	if (is_list(term2))
	{
		nbr_t v = 0;
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_integer(n))
			{ QABORT(ABORT_INVALIDARGNOTINT); return 0; }

			char i = (char)n->val_i-'0';
			v *= 10;
			v += i;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node *tmp = make_quick_int(v);
		int ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
		if (ok) q->is_det = 1;
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	char tmpbuf[40];
	sprintf(tmpbuf, "%d", (int)term1->val_i);
	const char *src = tmpbuf;

	while (*src)
	{
		node *tmp = make_int(*src);
		NLIST_PUSH_BACK(&l->val_l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		NLIST_PUSH_BACK(&l->val_l, tmp);
		l = tmp;
	}

	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	int ok = unify_term(q, term2, save_l, q->curr_frame);
	term_heapcheck(save_l);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_number_chars(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);
	int save_context = q->latest_context;
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	if (is_list(term2))
	{
		nbr_t v = 0;
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_atom(n))
			{ QABORT(ABORT_INVALIDARGNOTATOM); return 0; }

			char i = n->val_s[0]-'0';

			if ((i < 0) || (i > 9))
			{ QABORT(ABORT_INVALIDARGNOTINT); return 0; }

			v *= 10;
			v += i;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		q->curr_context = save_context;
		node *tmp = make_quick_int(v);
		int ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
		if (ok) q->is_det = 1;
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	char tmpbuf[40];
	sprintf(tmpbuf, "%d", (int)term1->val_i);
	const char *src = tmpbuf;

	while (*src)
	{
		node *tmp = make_atom(strndup(src, 1), 1);
		NLIST_PUSH_BACK(&l->val_l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		NLIST_PUSH_BACK(&l->val_l, tmp);
		l = tmp;
	}

	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	int ok = unify_term(q, term2, save_l, q->curr_frame);
	term_heapcheck(save_l);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_atom_chars(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	if (is_list(term2))
	{
		char tmpbuf[FUNCTOR_SIZE];
		char *dst = tmpbuf;
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_atom(n))
			{ QABORT(ABORT_INVALIDARGNOTATOM); return 0; }

			char i = n->val_s[0];
			*dst++ = i;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		*dst = '\0';
		node *tmp = make_atom(strdup(tmpbuf), 1);
		int ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
		if (ok) q->is_det = 1;
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	const char *src = term1->val_s;

	while (*src)
	{
		node *tmp = make_atom(strndup(src, 1), 1);
		NLIST_PUSH_BACK(&l->val_l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		NLIST_PUSH_BACK(&l->val_l, tmp);
		l = tmp;
	}

	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	int ok = unify_term(q, term2, save_l, q->curr_frame);
	term_heapcheck(save_l);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_atom_codes(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_list_or_var(term2);

	if (is_var(term1) && is_var(term2))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	if (is_list(term2))
	{
		char tmpbuf[FUNCTOR_SIZE];
		char *dst = tmpbuf;
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_integer(n))
			{ QABORT(ABORT_INVALIDARGNOTINT); return 0; }

			int i = n->val_i-'0';

			if ((i < 0) || (i > 9))
			{ QABORT(ABORT_INVALIDARGNOTINT); return 0; }

			*dst++ = (char)n->val_i;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		*dst = '\0';
		node *tmp = make_atom(strdup(tmpbuf), 1);
		int ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
		if (ok) q->is_det = 1;
		return ok;
	}

	node *save_l = make_list();
	node *l = save_l;
	const char *src = term1->val_s;

	while (*src)
	{
		node *tmp = make_int(*src);
		NLIST_PUSH_BACK(&l->val_l, tmp);

		if (!*++src)
			break;

		tmp = make_list();
		NLIST_PUSH_BACK(&l->val_l, tmp);
		l = tmp;
	}

	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	int ok = unify_term(q, term2, save_l, q->curr_frame);
	term_heapcheck(save_l);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_char_code(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);
	node *term2 = get_int_or_var(term2);

	if (is_var(term1) && is_var(term2))
	{ QABORT(ABORT_INVALIDARGMISSING); return 0; }

	if (is_integer(term2))
	{
		char tmpbuf[2];
		tmpbuf[0] = (char)term2->val_i;
		tmpbuf[1] = '\0';
		node *tmp = make_atom(strdup(tmpbuf), 1);
		int ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
		return ok;
	}

	node *tmp = make_quick_int(term1->val_s[0]);
	int ok = unify_term(q, term2, tmp, q->curr_frame);
	term_heapcheck(tmp);
	if (ok) q->is_det = 1;
	return ok;
}

static int bif_iso_set_input(tpl_query *q)
{
	return 0;
}

static int bif_iso_set_output(tpl_query *q)
{
	return 0;
}

static int bif_iso_curr_input(tpl_query *q)
{
	return 0;
}

static int bif_iso_curr_output(tpl_query *q)
{
	return 0;
}

static int bif_iso_sub_atom(tpl_query *q)
{
	//node *args = get_args(q);
	//node *term1 = get_term(term1);
	//node *term2 = get_term(term2);
	//node *term3 = get_term(term3);
	//node *term4 = get_term(term4);
	//node *term5 = get_term(term5);
	return 0;
}

static void expand_frame(tpl_query *q, unsigned cnt)
{
	prepare_frame(q, cnt);
	q->env_point += cnt;
	choice *c = &q->choices[q->choice_point];
	c->frame_size += cnt;
	c->env_point += cnt;
}

static int collect_vars2(tpl_query *q, node *n, int depth)
{
	if (depth > MAX_UNIFY_DEPTH) { QABORT2(ABORT_MAXDEPTH,"COLLECT_VARS"); return 0; }
	n = get_arg(q, n, q->latest_context);
	int cnt = 0;

	if (is_compound(n))
	{
		for (n = NLIST_FRONT(&n->val_l); n; n = NLIST_NEXT(n))
			cnt += collect_vars2(q, n, depth+1);
	}
	else if (is_var(n))
	{
		env *e = get_env(q, q->latest_context+n->slot);

		if (!sl_get(q->d, (char*)e, NULL))
		{
			sl_set(q->d, (char*)e, n);
			cnt++;
		}
	}

	return cnt;
}

static int collect_vars(tpl_query *q, node *n)
{
	q->latest_context = q->curr_frame;
	return collect_vars2(q, n, 1);
}

static int bif_iso_copy_term(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);
	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	int cnt = collect_vars(q, term1);
	sl_clear(&vars, NULL);
	if (q->halt) return 0;
	if (cnt) expand_frame(q, cnt);
	node *term = copy_term(q, term1);
	sl_done(&vars, NULL);
	q->d = NULL;
	put_env(q, q->curr_frame+term2->slot, term, q->curr_frame);
	term->refcnt--;
	return 1;
}

static void rebase(lexer *l, node *term)
{
	for (node *n = NLIST_FRONT(&term->val_l); n; n = NLIST_NEXT(n))
	{
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
	l.db = q->curr_db;
	asserta_index(&l, n, 1, &persist);
	lexer_done(&l);
	return persist;
}

#ifndef ISO_ONLY
static int bif_sys_asserta(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		n->flags |= FLAG_RULE|FLAG_FACT;
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTA;

#ifndef ISO_ONLY
	if (q->curr_db->in_tran)
	{
		node *tmp = CALLOC(node);
		tmp->orig = n;
		NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
	}
	else
#endif
	{
		DBLOCK(q->curr_db);
		bif_asserta(q, n);
		DBUNLOCK(q->curr_db);
	}

	put_ptr(q, q->curr_frame+term2->slot, n);
	return 1;
}
#endif

static int bif_iso_asserta(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		n->flags |= FLAG_RULE|FLAG_FACT;
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTA;

#ifndef ISO_ONLY
	if (q->curr_db->in_tran)
	{
		node *tmp = CALLOC(node);
		tmp->orig = n;
		NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
	}
	else
#endif
	{
		DBLOCK(q->curr_db);
		bif_asserta(q, n);
		DBUNLOCK(q->curr_db);
	}

	return 1;
}

int bif_assertz(tpl_query *q, node *n)
{
	int persist;
	lexer l;
	lexer_init(&l, q->pl);
	l.db = q->curr_db;
	assertz_index(&l, n, 1, &persist);
	lexer_done(&l);
	return persist;
}

#ifndef ISO_ONLY
static int bif_sys_assertz(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		n->flags |= FLAG_RULE|FLAG_FACT;
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTZ;

#ifndef ISO_ONLY
	if (q->curr_db->in_tran)
	{
		node *tmp = CALLOC(node);
		tmp->orig = n;
		NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
	}
	else
#endif
	{
		DBLOCK(q->curr_db);
		bif_assertz(q, n);
		DBUNLOCK(q->curr_db);
	}

	put_ptr(q, q->curr_frame+term2->slot, n);
	return 1;
}
#endif

static int bif_iso_assertz(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		n->flags |= FLAG_RULE|FLAG_FACT;
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	lexer l;
	lexer_init(&l, q->pl);
	rebase(&l, n);
	n->frame_size = l.vars;
	lexer_done(&l);
	n->flags |= FLAG_DBS_ASSERTZ;

#ifndef ISO_ONLY
	if (q->curr_db->in_tran)
	{
		node *tmp = CALLOC(node);
		tmp->orig = n;
		NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
		return 1;
	}
#endif

	DBLOCK(q->curr_db);
	bif_assertz(q, n);
	DBUNLOCK(q->curr_db);
	return 1;
}

int bif_retract(tpl_query *q, node *n)
{
	int persist;
	lexer l;
	lexer_init(&l, q->pl);
	l.db = q->curr_db;
	retract_index(&l, n, &persist);
	lexer_done(&l);

#ifndef ISO_ONLY
	if (persist && !q->curr_db->loading)
	{
		size_t buflen = 1024*64;					// expandable
		char *dstbuf = (char*)malloc(buflen+1);
		dbs_save_node(q->curr_db, q->curr_db->fp, &dstbuf, &buflen, n);
		free(dstbuf);
	}
#endif

	if (is_fact(n))
		term_heapcheck(n);
	else
		n->flags |= FLAG_DELETED;

	return persist;
}

static int bif_iso_retract(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		n->flags |= FLAG_RULE|FLAG_FACT;
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	node *tmp = NLIST_FRONT(&n->val_l);
	node *head = tmp = NLIST_NEXT(tmp);
	int arity = 0;

	if (is_compound(head))
	{
		arity = NLIST_COUNT(&head->val_l)-1;
		tmp = NLIST_FRONT(&head->val_l);
	}

	functor = tmp->val_s;
	char tmpbuf[(FUNCTOR_SIZE*2)+10];

	if (!strchr(functor, ARITY_CHAR))
	{
		snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);

		if (!(tmp->flags & FLAG_CONST))
			free(tmp->val_s);

		tmp->val_s = strdup(tmpbuf);
	}
	else
		strcpy(tmpbuf, functor);

	functor = tmpbuf;
	rule *r = NULL;

	if (!q->curr_db->in_tran)
		DBLOCK(q->curr_db);

	if (!sl_get(&q->curr_db->rules, functor, (void**)&r))
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		return 0;
	}

	if (!r->dynamic)
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	if (!NLIST_COUNT(&r->clauses))
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		return 0;
	}

	if (!q->retry)
		allocate_frame(q);

	for (node *match = NLIST_FRONT(&r->clauses); match; match = NLIST_NEXT(match))
	{
		if (match->flags & FLAG_DELETED)
			continue;

		g_match_try++;

		if (!unify_term(q, n, match, q->curr_frame))
		{
			reallocate_frame(q);
			continue;
		}

		g_match_ok++;
		match->flags |= FLAG_DBS_RETRACT;
		const int is_last = !NLIST_NEXT(match);

#ifndef ISO_ONLY
		if (q->curr_db->in_tran)
		{
			node *tmp = CALLOC(node);
			tmp->orig = match;
			NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
		}
		else
#endif
			bif_retract(q, match);

		if (!is_last)
			try_me_nofollow(q);

		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		q->is_det = 1;
		return 1;
	}

	if (!q->curr_db->in_tran)
		DBUNLOCK(q->curr_db);

	term_heapcheck(n);
	return 0;
}

static int bif_iso_retractall(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	const char *functor;
	node *n;

	if (is_compound(term1))
		functor = NLIST_FRONT(&term1->val_l)->val_s;
	else
		functor = term1->val_s;

	if (strcmp(functor, ":-"))
	{
		n = make_structure();
		NLIST_PUSH_BACK(&n->val_l, make_const_atom(":-", 0));
		NLIST_PUSH_BACK(&n->val_l, clone_term(q, term1));
		NLIST_PUSH_BACK(&n->val_l, make_true());
	}
	else
		n = clone_term(q, term1);

	node *tmp = NLIST_FRONT(&n->val_l);
	node *head = tmp = NLIST_NEXT(tmp);
	int arity = 0;

	if (is_compound(head))
	{
		arity = NLIST_COUNT(&head->val_l)-1;
		tmp = NLIST_FRONT(&head->val_l);
	}

	functor = tmp->val_s;
	char tmpbuf[(FUNCTOR_SIZE*2)+10];

	if (!strchr(functor, ARITY_CHAR))
	{
		snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);

		if (!(tmp->flags & FLAG_CONST))
			free(tmp->val_s);

		tmp->val_s = strdup(tmpbuf);
	}
	else
		strcpy(tmpbuf, functor);

	functor = tmpbuf;
	rule *r = NULL;

	if (!q->curr_db->in_tran)
		DBLOCK(q->curr_db);

	if (!sl_get(&q->curr_db->rules, functor, (void**)&r))
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		return 0;
	}

	if (!r->dynamic)
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	if (!NLIST_COUNT(&r->clauses))
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		term_heapcheck(n);
		return 0;
	}

	allocate_frame(q);
	int any = 0;

	for (node *match = NLIST_FRONT(&r->clauses); match;)
	{
		if (match->flags & FLAG_DELETED)
			continue;

		prepare_frame(q, n->frame_size);
		g_match_try++;
		node *next = NLIST_NEXT(match);

		if (unify_term(q, n, match, q->curr_frame))
		{
			match->flags |= FLAG_DBS_RETRACT;

#ifndef ISO_ONLY
			if (q->curr_db->in_tran)
			{
				node *tmp = CALLOC(node);
				tmp->orig = match;
				NLIST_PUSH_BACK(&q->curr_db->tran_queue, tmp);
			}
			else
#endif
				bif_retract(q, match);

			g_match_ok++;
			any++;
		}

		reallocate_frame(q);
		match = next;
	}

	if (!q->curr_db->in_tran)
		DBUNLOCK(q->curr_db);

	deallocate_frame(q);
	term_heapcheck(n);
	return any > 0;
}

static int bif_iso_abolish(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_structure(term1);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, 1);
	printf("*** %s\n", tmpbuf);
	rule *r = NULL;

	if (!q->curr_db->in_tran)
		DBLOCK(q->curr_db);

	if (!sl_get(&q->curr_db->rules, tmpbuf, (void**)&r))
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		return 0;
	}

	if (!r->dynamic)
	{
		if (!q->curr_db->in_tran)
			DBUNLOCK(q->curr_db);

		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	for (node *match = NLIST_FRONT(&r->clauses); match; match = NLIST_NEXT(match))
	{
		if (is_fact(match))
			term_heapcheck(match);
		else
			match->flags |= FLAG_DELETED;
	}

	if (sl_del(&q->curr_db->rules, tmpbuf, (void**)&r))
		FREE(r);

	if (!q->curr_db->in_tran)
		DBUNLOCK(q->curr_db);

	free(tmpbuf);
	return 1;
}

#ifndef ISO_ONLY
static int bif_sys_erase(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_ptr(term1);
	node *n = (node*)term1->val_ptr;

	if (!q->curr_db->in_tran)
		DBLOCK(q->curr_db);

	bif_retract(q, n);

	if (!q->curr_db->in_tran)
		DBUNLOCK(q->curr_db);

	return 1;
}
#endif

static int nodecmp(const void *p1, const void *p2)
{
	node *term1 = *(node**)p1;
	node *term2 = *(node**)p2;

	if (is_integer(term1))
	{
		if (is_integer(term2))
		{
			if (term1->val_i < term2->val_i)
				return -1;

			if (term1->val_i > term2->val_i)
				return 1;

			return 0;
		}
		if (is_float(term2))
		{
			if (term1->val_i < term2->val_f)
				return -1;

			if (term1->val_i > term2->val_f)
				return 1;

			return 0;
		}
	}
	else if (is_float(term1))
	{
		if (is_integer(term2))
		{
			if (term1->val_f < term2->val_i)
				return -1;

			if (term1->val_f > term2->val_i)
				return 1;

			return 0;
		}
		if (is_float(term2))
		{
			if (term1->val_f < term2->val_f)
				return -1;

			if (term1->val_f > term2->val_f)
				return 1;

			return 0;
		}
	}
	else if (is_atom(term1) && is_atom(term2))
		return strcmp(term1->val_s, term2->val_s);

	return 0;
}

static int bif_iso_sort(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	int save_context = q->latest_context;
	node *term2 = get_atom_or_list_or_var(term2);
	q->latest_context = save_context;

	if (is_list(term1))
	{
		node *l = term1;
		size_t cnt = 0;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_atomic(n))
				return 0;

			cnt++;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node **base = (node**)malloc(sizeof(node*)*cnt);
		l = term1;
		q->latest_context = save_context;
		size_t idx = 0;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);
			base[idx++] = n;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		qsort(base, cnt, sizeof(node*), nodecmp);
		l = make_list();
		node *tmp = l;

		for (int i = 0; i < cnt; i++)
		{
			if (i < (cnt-1))
				if (!nodecmp(&base[i], &base[i+1]))
					continue;

			NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, base[i]));

			if (i == (cnt-1))
				break;

			node *tmp2;
			NLIST_PUSH_BACK(&tmp->val_l, tmp2=make_list());
			tmp = tmp2;
		}

		NLIST_PUSH_BACK(&tmp->val_l, make_const_atom("[]", 0));
		free(base);
		int ok = unify_term(q, term2, l, q->curr_frame);
		term_heapcheck(l);
		return ok;
	}

	return unify_term(q, term2, term1, q->curr_frame);
}

static int keycmp(const void *p1, const void *p2)
{
	node *term1 = *(node**)p1;
	node *term2 = *(node**)p2;
	node *n1 = NLIST_FRONT(&term1->val_l);
	node *n2 = NLIST_FRONT(&term2->val_l);
	return nodecmp(&n1->hdr.next, &n2->hdr.next);
}

static int bif_iso_keysort(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	int save_context = q->latest_context;
	node *term2 = get_atom_or_list_or_var(term2);
	q->latest_context = save_context;

	if (is_list(term1))
	{
		node *l = term1;
		size_t cnt = 0;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);

			if (!is_compound(n))
				return 0;

			if ((NLIST_COUNT(&n->val_l) != 3) ||
				!is_atom(NLIST_FRONT(&n->val_l)) ||
					strcmp(NLIST_FRONT(&n->val_l)->val_s, "-"))
				return 0;

			cnt++;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		node **base = (node**)malloc(sizeof(node*)*cnt);
		q->latest_context = save_context;
		l = term1;
		size_t idx = 0;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);
			base[idx++] = n;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		qsort(base, cnt, sizeof(node*), keycmp);
		l = make_list();
		node *tmp = l;

		for (size_t i = 0; i < cnt; i++)
		{
			NLIST_PUSH_BACK(&tmp->val_l, clone_term(q, base[i]));

			if (i == (cnt-1))
				break;

			node *tmp2;
			NLIST_PUSH_BACK(&tmp->val_l, tmp2=make_list());
			tmp = tmp2;
		}

		NLIST_PUSH_BACK(&tmp->val_l, make_const_atom("[]", 0));
		free(base);
		int ok = unify_term(q, term2, l, q->curr_frame);
		term_heapcheck(l);
		return ok;
	}

	return unify_term(q, term2, term1, q->curr_frame);
}

static int bif_iso_arg(tpl_query *q)
{
	node *args = get_args(q);
	node *orig_term1 = NLIST_NEXT(args);
	node *term1 = get_int_or_var(term1);
	node *term2 = get_compound(term2);
	unsigned save_context = q->latest_context;
	node *term3 = get_term(term3);

	if (is_integer(term1) && (term1->val_i <= 0))
		return 0;

	int idx;

	if (is_var(term1))
	{
		put_int(q, q->curr_frame+orig_term1->slot, idx=1);
		allocate_frame(q);
	}
	else if (q->retry)
	{
		reset_arg(q, orig_term1, q->curr_frame);
		put_int(q, q->curr_frame+orig_term1->slot, idx=term1->val_i+1);
	}
	else
		idx = term1->val_i;

	node *n = NLIST_FRONT(&term2->val_l);
	n = NLIST_NEXT(n);

	for (int i = 1; (i < idx) && n; i++)
		n = NLIST_NEXT(n);

	if (!n) return 0;

	if ((!q->retry && is_var(term1)) || q->retry)
		try_me_nofollow(q);

	node *term = get_arg(q, n, save_context);
	return unify_term(q, term, term3, q->curr_frame);
}

static int bif_iso_univ(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);

	if (is_var(term1) && is_var(term2))
	{
		node *s = make_structure();
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);
			NLIST_PUSH_BACK(&s->val_l, clone_term(q, n));
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		put_env(q, q->curr_frame+term1->slot, s, -1);
		s->refcnt--;
		return 1;
	}
	else if (is_structure(term1) && is_var(term2))
	{
		node *l = make_list();
		node *n = NLIST_FRONT(&term1->val_l);
		node *save_l = l;

		while (n)
		{
			NLIST_PUSH_BACK(&l->val_l, clone_term(q, n));
			if (!NLIST_NEXT(n)) break;
			node *tmp;
			NLIST_PUSH_BACK(&l->val_l, tmp=make_list());
			l = tmp;
			n = NLIST_NEXT(n);
		}

		NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
		put_env(q, q->curr_frame+term2->slot, save_l, -1);
		save_l->refcnt--;
		return 1;
	}
	else if (is_structure(term1) && is_list(term2))
	{
		node *l = make_list();
		node *n = NLIST_FRONT(&term1->val_l);
		node *save_l = l;

		while (n)
		{
			NLIST_PUSH_BACK(&l->val_l, clone_term(q, n));
			if (!NLIST_NEXT(n)) break;
			node *tmp;
			NLIST_PUSH_BACK(&l->val_l, tmp=make_list());
			l = tmp;
			n = NLIST_NEXT(n);
		}

		NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
		int ok = unify_term(q, save_l, term2, q->curr_frame);
		term_heapcheck(save_l);
		return ok;
	}
	else if (is_var(term1) && is_list(term2))
	{
		node *s = make_structure();
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			node *n = get_arg(q, head, q->latest_context);
			NLIST_PUSH_BACK(&s->val_l, copy_term(q, n));
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		if (NLIST_COUNT(&s->val_l) == 1)
			put_env(q, q->curr_frame+term1->slot, NLIST_FRONT(&s->val_l), q->curr_frame);
		else
			put_env(q, q->curr_frame+term1->slot, s, q->curr_frame);

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

	if (is_var(term1))
	{
		if (is_atom(term2) && is_integer(term3))
		{
			if (term3->val_i > 0)
			{
				expand_frame(q, term3->val_i);
				node *s = make_structure();
				s->refcnt--;
				NLIST_PUSH_BACK(&s->val_l, copy_atom(q, term2));

				for (int i = 0; i < term3->val_i; i++)
					NLIST_PUSH_BACK(&s->val_l, make_var(q));

				put_env(q, q->curr_frame+term1->slot, s, q->curr_frame);
			}
			else if (term3->val_i == 0)
				put_env(q, q->curr_frame+term1->slot, term2, -1);

			return 1;
		}

		return 0;
	}
	else if (is_atomic(term1))
	{
		nf = term1;
	}
	else if (is_compound(term1))
	{
		nf = NLIST_FRONT(&term1->val_l);
		arity = NLIST_COUNT(&term1->val_l)-1;
	}
	else
		return 0;

	node *na = make_quick_int(arity);
	int ok1 = unify_term(q, nf, term2, q->curr_frame);
	int ok2 = unify_term(q, na, term3, q->curr_frame);
	term_heapcheck(na);
	return ok1 && ok2;
}

static int bif_iso_length(tpl_query *q)
{
	node *args = get_args(q);
	node *orig_term1 = NLIST_NEXT(args);
	node *term1 = get_term(term1);
	int save_context = q->latest_context;
	node *orig_term2 = NLIST_NEXT(args);
	node *term2 = get_int_or_var(term2);
	if (!is_var(term1) && !is_atom(term1) && !is_list(term1)) { QABORT(ABORT_INVALIDARGNOTVARORLIST); return 0; }
	if (is_atom(term1) && strcmp(term1->val_s, "[]")) { QABORT(ABORT_INVALIDARGNOTVARORLIST); return 0; }

	if (is_atom(term1))
		return unify_term(q, term2, make_quick_int(0), q->curr_frame);

	if (is_list(term1) && !q->retry)
	{
		size_t cnt = 0;
		q->latest_context = save_context;
		node *l = term2;

		while (is_list(l))
		{
			node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
			//node *n = get_arg(q, head, q->latest_context);
			cnt++;
			node *tail = NLIST_NEXT(head);
			l = get_arg(q, tail, q->latest_context);
		}

		if (is_var(term2))
			put_int(q, q->curr_frame+term2->slot, cnt);
		else
			return term2->val_i == cnt;

		return 1;
	}

	if (is_list(term1))
		reset_arg(q, orig_term1, q->curr_frame);

	if (!is_var(orig_term1))
		return 0;

	if (is_var(term2))
	{
		put_int(q, q->curr_frame+orig_term2->slot, 0);
		allocate_frame(q);
		try_me_nofollow(q);

		if (is_anon(orig_term1))
			return 1;

		node *tmp = make_const_atom("[]", 0);
		put_env(q, q->curr_frame+orig_term1->slot, tmp, -1);
		tmp->refcnt--;
		q->is_det = 1;
		return 1;
	}

	int cnt = term2->val_i;

	if (q->retry)
	{
		reset_arg(q, orig_term2, q->curr_frame);
		put_int(q, q->curr_frame+orig_term2->slot, ++cnt);
		try_me_nofollow(q);
	}

	if (is_anon(orig_term1))
		return 1;

	if (cnt == 0)
	{
		node *tmp = make_const_atom("[]", 0);
		put_env(q, q->curr_frame+orig_term1->slot, tmp, -1);
		tmp->refcnt--;
		q->is_det = 1;
		return 1;
	}

	node *l = make_list();
	node *save_l = l;
	expand_frame(q, cnt);

	for (int i = 0; i < cnt; i++)
	{
		NLIST_PUSH_BACK(&l->val_l, make_var(q));
		if (i == (cnt-1)) break;
		node *tmp;
		NLIST_PUSH_BACK(&l->val_l, tmp=make_list());
		l = tmp;
	}

	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	put_env(q, q->curr_frame+orig_term1->slot, save_l, q->curr_frame);
	save_l->refcnt--;
	q->is_det = 1;
	return 1;
}

static int bif_iso_term_variables(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);

	if (is_anon(term2))
		return 1;

	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	q->d = &vars;
	int cnt = collect_vars(q, term1);
	if (q->halt) return 0;

	if (!cnt)
	{
		q->d = NULL;
		sl_done(&vars, NULL);
		node *tmp = make_const_atom("[]", 0);
		put_env(q, q->curr_frame+term2->slot, tmp, -1);
		tmp->refcnt--;
		return 1;
	}

	node *l = make_list();
	node *save_l = l;
	node *n;
	sl_start(&vars);

	while ((sl_next(&vars, (void**)&n)) != NULL)
	{
		NLIST_PUSH_BACK(&l->val_l, copy_term(q, n));
		if (!vars.iter) break;
		node *tmp;
		NLIST_PUSH_BACK(&l->val_l, tmp=make_list());
		l = tmp;
	}

	q->d = NULL;
	sl_done(&vars, NULL);
	NLIST_PUSH_BACK(&l->val_l, make_const_atom("[]", 0));
	put_env(q, q->curr_frame+term2->slot, save_l, -1);
	save_l->refcnt--;
	q->is_det = 1;
	return 1;
}

static int bif_iso_clause(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);
	const char *functor = NULL;
	node *head;

	if (is_anon(term2))
		return 1;

	if (!q->retry)
	{
		if (is_structure(term1))
			functor = NLIST_FRONT(&term1->val_l)->val_s;
		else if (is_atom(term1))
			functor = term1->val_s;

		rule *r = NULL;
		DBLOCK(q->curr_db);

		if (!sl_get(&q->curr_db->rules, functor, (void**)&r))
		{
			DBUNLOCK(q->curr_db);
			return 0;
		}

		node *n = NLIST_FRONT(&r->clauses);
		head = NLIST_NEXT(NLIST_FRONT(&n->val_l));
		q->curr_match = n;
		DBUNLOCK(q->curr_db);
		allocate_frame(q);

		if (!unify_term(q, term1, head, q->env_point))
			return 0;
	}
	else
	{
		q->curr_match = NLIST_NEXT(q->curr_match);
		if (!q->curr_match) return 0;
		head = NLIST_NEXT(NLIST_FRONT(&q->curr_match->val_l));

		if (!unify_term(q, term1, head, q->env_point))
			return 0;
	}

	node *body = NLIST_NEXT(head);

	if (!body)	// Fact?
	{
		body = make_true();
		body->refcnt--;
	}

	put_env(q, q->curr_frame+term2->slot, body, q->env_point);
	q->is_det = 1;
	try_me_nofollow(q);
	return 1;
}

static int bif_iso_findall(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_var(term3);
	tpl_query *who = query_create_subquery(q, 0);
	if (!who) { QABORT(ABORT_OUTOFMEMORY); return 0; }
	begin_query(who, term2);
	query_run(who);
	node *acc = make_const_atom("[]", 0);
	node *end = NULL;

	while (who->ok)
	{
		who->latest_context = 0;
		node *from = get_arg(who, term1, who->latest_context);
		node *res = clone_term(who, from);

		if (!end)
		{
			term_heapcheck(acc);
			acc = make_list();
			NLIST_PUSH_BACK(&acc->val_l, res);
			end = acc;
		}
		else
		{
			node *tmp;
			NLIST_PUSH_BACK(&end->val_l, tmp=make_list());
			NLIST_PUSH_BACK(&tmp->val_l, res);
			end = tmp;
		}

		query_continue(who);
	}

	if (end)
		NLIST_PUSH_BACK(&end->val_l, make_const_atom("[]", 0));

	int ok = unify_term(q, term3, acc, q->latest_context);
	term_heapcheck(acc);
	query_destroy(who);
	return ok;
}

static int bif_iso_bagof(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_var(var);				// FLAG_HIDDEN
	node *term1 = get_var(term1);
	node *term2 = get_structure(term2);
	node *term3 = get_var(term3);
	return 0;
}
static int bif_iso_setof(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_var(var);				// FLAG_HIDDEN
	node *term1 = get_var(term1);
	node *term2 = get_structure(term2);
	node *term3 = get_var(term3);
	return 0;
}

static void eval_nbr(tpl_query *q, node *nbr)
{
	if (is_integer(nbr))
	{
		q->nv.val_i = nbr->val_i;
		q->nv.type = NUM_INTEGER;
	}
	else if (is_float(nbr))
	{
		q->nv.val_f = nbr->val_f;
		q->nv.type = NUM_FLOAT;
	}
	else
		q->nv.type = NUM_NONE;
}

static void eval(tpl_query *q, node **args);

static void eval_term(tpl_query *q, node *n)
{
	if (!n->bifptr)		// Assume empty parens.
	{
		eval_term(q, NLIST_FRONT(&n->val_l));
		return;
	}

	node *save = q->curr_term;
	q->curr_term = n;

	if (is_builtin(n))
	{
		n->bifptr(q);
		g_s_resolves++;
		q->curr_term = save;
		return;
	}

	// UDFs...

	allocate_frame(q);
	try_me(q);
	q->curr_term = n;
	query_inline(q);
	trust_me(q);
	q->curr_term = save;
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

	if (is_var(term1))
	{
		if (q->nv.type == NUM_INTEGER)
			put_int(q, q->curr_frame+term1->slot, q->nv.val_i);
		else if (q->nv.type == NUM_FLOAT)
			put_float(q, q->curr_frame+term1->slot, q->nv.val_f);
		else
			{ QABORT(ABORT_TYPEERROR); return 0; }

		return 1;
	}

	if (is_integer(term1) && (q->nv.type == NUM_INTEGER))
		return term1->val_i == q->nv.val_i;
	else if (is_float(term1) && (q->nv.type == NUM_FLOAT))
		return term1->val_f == q->nv.val_f;

	{ QABORT(ABORT_TYPEERROR); return 0; }
}

static int bif_iso_integer(tpl_query *q)
{
	node *args = get_args(q);

	if (q->eval)
	{
		eval(q, &args);

		if (q->nv.type == NUM_FLOAT)
		{
			q->nv.val_i = (nbr_t)q->nv.val_f;
			q->nv.type = NUM_INTEGER;
		}
		else if (q->nv.type != NUM_INTEGER)
			{ QABORT(ABORT_TYPEERROR); return 0; }

		q->nv.type = NUM_INTEGER;
		return 1;
	}
	else
	{
		node *term1 = get_term(term1);
		return is_integer(term1) ? 1 : 0;
	}
}

static int bif_iso_float(tpl_query *q)
{
	node *args = get_args(q);

	if (q->eval)
	{
		eval(q, &args);

		if (q->nv.type == NUM_INTEGER)
		{
			q->nv.val_f = (flt_t)q->nv.val_i;
			q->nv.type = NUM_FLOAT;
		}
		else if (q->nv.type != NUM_FLOAT)
			{ QABORT(ABORT_TYPEERROR); return 0; }

		return 1;
	}
	else
	{
		node *term1 = get_term(term1);
		return is_float(term1) ? 1 : 0;
	}
}

int bif_iso_reverse(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_i = -q->nv.val_i;
	else if (q->nv.type == NUM_FLOAT)
		q->nv.val_f = -q->nv.val_f;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return 1;
}

static int bif_iso_add(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if (nv1.type == NUM_INTEGER)
	{
		if (nv2.type == NUM_INTEGER)
			q->nv.val_i = nv1.val_i + nv2.val_i;
		else if (nv2.type == NUM_FLOAT)
		{
			q->nv.val_f = nv1.val_i + nv2.val_f;
			nv1.type = NUM_FLOAT;
		}
	}
	else if (nv1.type == NUM_FLOAT)
	{
		if (nv2.type == NUM_FLOAT)
			q->nv.val_f = nv1.val_f + nv2.val_f;
		else if (nv2.type == NUM_INTEGER)
			q->nv.val_f = nv1.val_f + (flt_t)nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = nv1.type;
	return 1;
}

static int bif_iso_subtract(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if (nv1.type == NUM_INTEGER)
	{
		if (nv2.type == NUM_INTEGER)
			q->nv.val_i = nv1.val_i - nv2.val_i;
		else if (nv2.type == NUM_FLOAT)
		{
			q->nv.val_f = (flt_t)nv1.val_i - nv2.val_f;
			nv1.type = NUM_FLOAT;
		}
	}
	else if (nv1.type == NUM_FLOAT)
	{
		if (nv2.type == NUM_FLOAT)
			q->nv.val_f = nv1.val_f - nv2.val_f;
		else if (nv2.type == NUM_INTEGER)
			q->nv.val_f = nv1.val_f - (flt_t)nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = nv1.type;
	return 1;
}

static int bif_iso_multiply(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if (nv1.type == NUM_INTEGER)
	{
		if (nv2.type == NUM_INTEGER)
			q->nv.val_i = nv1.val_i * nv2.val_i;
		else if (nv2.type == NUM_FLOAT)
		{
			q->nv.val_f = (flt_t)nv1.val_i * nv2.val_f;
			nv1.type = NUM_FLOAT;
		}
	}
	else if (nv1.type == NUM_FLOAT)
	{
		if (nv2.type == NUM_FLOAT)
			q->nv.val_f = nv1.val_f * nv2.val_f;
		else if (nv2.type == NUM_INTEGER)
			q->nv.val_f = nv1.val_f * (flt_t)nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = nv1.type;
	return 1;
}

static int bif_iso_divide(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
	{
		if (nv2.val_i == 0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_f = (flt_t)nv1.val_i / (flt_t)nv2.val_i;
	}
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
	{
		if (nv2.val_f == 0.0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_f = (flt_t)nv1.val_i / nv2.val_f;
	}
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
	{
		if (nv2.val_f == 0.0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_f = nv1.val_f / nv2.val_f;
	}
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
	{
		if (nv2.val_i == 0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_f = nv1.val_f / (flt_t)nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_div(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
	{
		if (nv2.val_i == 0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_i = nv1.val_i / nv2.val_i;
	}
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
	{
		if (nv2.val_f == 0.0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_i = nv1.val_i / nv2.val_f;
	}
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
	{
		if (nv2.val_f == 0.0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_i = (nbr_t)nv1.val_f / nv2.val_f;
	}
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
	{
		if (nv2.val_i == 0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_i = (nbr_t)nv1.val_f / nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_nlt(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	int ok = 0;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_i < nv2.val_i;
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
		ok = (flt_t)nv1.val_i < nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
		ok = nv1.val_f < nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_f < (flt_t)nv2.val_i;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return ok;
}

static int bif_iso_nle(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	int ok = 0;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_i <= nv2.val_i;
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
		ok = (flt_t)nv1.val_i <= nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
		ok = nv1.val_f <= nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_f <= (flt_t)nv2.val_i;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return ok;
}

static int bif_iso_neq(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	int ok = 0;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_i == nv2.val_i;
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
		ok = (flt_t)nv1.val_i == nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
		ok = nv1.val_f == nv2.val_f;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
		ok = nv1.val_f == (flt_t)nv2.val_i;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return ok;
}

static int bif_iso_ngt(tpl_query *q) { return !bif_iso_nle(q); }
static int bif_iso_nge(tpl_query *q) { return !bif_iso_nlt(q); }
static int bif_iso_nne(tpl_query *q) { return !bif_iso_neq(q); }

static int bif_iso_rem(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
	{
		if (nv2.val_i == 0) { QABORT(ABORT_INVALIDARGDIVIDEBYZERO); return 0; }
		q->nv.val_i = nv1.val_i % nv2.val_i;
	}
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_shiftleft(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		q->nv.val_u = nv1.val_u << nv2.val_u;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_shiftright(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		q->nv.val_u = nv1.val_u >> nv2.val_u;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_bitand(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		q->nv.val_u = nv1.val_u & nv2.val_u;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_bitor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		q->nv.val_u = nv1.val_u | nv2.val_u;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_bitxor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		q->nv.val_u = nv1.val_u ^ nv2.val_u;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_complement(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	if (q->nv.type != NUM_INTEGER) { QABORT(ABORT_TYPEERROR); return 0; };
	q->nv.val_u = ~q->nv.val_u;
	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_sin(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = sin(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_asin(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = asin(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_cos(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = cos(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_acos(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = acos(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_tan(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = tan(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_atan(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = atan(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_atan2(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	if (nv1.type == NUM_INTEGER) nv1.val_f = (flt_t)nv1.val_i;
	if (nv2.type == NUM_INTEGER) nv2.val_f = (flt_t)nv2.val_i;
	q->nv.val_f = atan2(nv1.val_f,nv2.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_pow(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	if (nv1.type == NUM_INTEGER) nv1.val_f = (flt_t)nv1.val_i;
	if (nv2.type == NUM_INTEGER) nv2.val_f = (flt_t)nv2.val_i;
	q->nv.val_f = pow(nv1.val_f,nv2.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_powi(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;

	if ((nv1.type != NUM_INTEGER) || (nv2.type != NUM_INTEGER))
		return bif_iso_pow(q);

	if (nv2.val_i < 0)
		{ QABORT(ABORT_RESULTOVERFLOW); return 0; }

	q->nv.val_i = (nbr_t)pow((flt_t)nv1.val_i,(flt_t)nv2.val_i);
	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_exp(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = exp(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_sqrt(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = sqrt(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_log(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_f = (flt_t)q->nv.val_i;

	q->nv.val_f = log(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_abs(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_FLOAT)
		q->nv.val_f = q->nv.val_f < 0.0 ? -q->nv.val_f : q->nv.val_f;
	else if (q->nv.type == NUM_INTEGER)
		q->nv.val_i = q->nv.val_i < 0 ? -q->nv.val_i : q->nv.val_i;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return 1;
}

static int bif_iso_max(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	number *np = NULL;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		np = nv1.val_i > nv2.val_i ? &nv1 : &nv2;
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
		np = nv1.val_i > nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
		np = nv1.val_f > nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
		np = nv1.val_f > nv2.val_i ? &nv1 : &nv2;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv = *np;
	return 1;
}

static int bif_iso_min(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	number nv1 = q->nv;
	eval(q, &args);
	number nv2 = q->nv;
	number *np = NULL;

	if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_INTEGER))
		np = nv1.val_i < nv2.val_i ? &nv1 : &nv2;
	else if ((nv1.type == NUM_INTEGER) && (nv2.type == NUM_FLOAT))
		np = nv1.val_i < nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_FLOAT))
		np = nv1.val_f < nv2.val_f ? &nv1 : &nv2;
	else if ((nv1.type == NUM_FLOAT) && (nv2.type == NUM_INTEGER))
		np = nv1.val_f < nv2.val_i ? &nv1 : &nv2;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv = *np;
	return 1;
}

static int bif_iso_sign(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_INTEGER)
		q->nv.val_i = q->nv.val_i < 0 ? -1 : q->nv.val_i > 0 ? 1 : 0;
	else if (q->nv.type == NUM_FLOAT)
		q->nv.val_f = q->nv.val_f < 0.0 ? -1.0 : q->nv.val_f > 0.0 ? 1.0 : 0.0;
	else
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return 1;
}

static int bif_iso_float_fractional_part(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type != NUM_FLOAT)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.val_f -= (nbr_t)q->nv.val_f;
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_float_integer_part(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type != NUM_FLOAT)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.val_f = (nbr_t)q->nv.val_f;
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_floor(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type != NUM_FLOAT)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.val_i = floor(q->nv.val_f);
	q->nv.type = NUM_INTEGER;
	return 1;
}

static int bif_iso_round(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_FLOAT)
	{
		q->nv.type = NUM_INTEGER;
		q->nv.val_i = (nbr_t)(q->nv.val_f+0.5);
	}
	else if (q->nv.type != NUM_INTEGER)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return 1;
}

static int bif_iso_ceiling(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type != NUM_FLOAT)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	q->nv.val_f -= ceil(q->nv.val_f);
	q->nv.type = NUM_FLOAT;
	return 1;
}

static int bif_iso_truncate(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);

	if (q->nv.type == NUM_FLOAT)
	{
		q->nv.val_i = (nbr_t)q->nv.val_f;
		q->nv.type = NUM_INTEGER;
	}
	else if (q->nv.type != NUM_INTEGER)
		{ QABORT(ABORT_TYPEERROR); return 0; }

	return 1;
}

#ifndef ISO_ONLY
static int bif_sys_term_to_atom(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_atom_or_var(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0); // FIXME
	node *n = make_atom(strdup(tmpbuf), 1);
	free(tmpbuf);
	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_sys_term_to_blob(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_var(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	dst += sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0); // FIXME
	node *n = make_blob(strdup(tmpbuf), dst-tmpbuf);
	free(tmpbuf);
	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_sys_abolish(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int(term2);
	const char *functor = term1->val_s;
	int arity = (int)term2->val_i;
	char tmpbuf[KEY_SIZE];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
	rule *r = NULL;
	DBLOCK(q->curr_db);

	if (!sl_get(&q->curr_db->rules, tmpbuf, (void**)&r))
	{
		DBUNLOCK(q->curr_db);
		return 0;
	}

	if (!r->dynamic)
	{
		DBUNLOCK(q->curr_db);
		QABORT(ABORT_NOTDYNAMIC);
		return 0;
	}

	for (node *match = NLIST_FRONT(&r->clauses); match; match = NLIST_NEXT(match))
		match->flags |= FLAG_DELETED;

	if (sl_del(&q->curr_db->rules, tmpbuf, (void**)&r))
		FREE(r);

	DBUNLOCK(q->curr_db);
	return 1;
}

static int bif_sys_is_list(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);

	if (is_atom(term1))
	{
		if (!strcmp(term1->val_s, "[]"))
			return 1;
	}

	return is_list(term1);
}

static int bif_sys_is_tuple(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_tuple(term1);
}

static int bif_sys_is_struct(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_structure(term1);
}

static int bif_sys_is_stream(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_stream(term1);
}

static int bif_sys_consult(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return trealla_consult_file(q->pl, term1->val_s);
}

static int bif_sys_deconsult(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	return trealla_deconsult(q->pl, term1->val_s);
}

static int bif_sys_reconsult(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	trealla_deconsult(q->pl, term1->val_s);
	return trealla_consult_file(q->pl, term1->val_s);
}

static int bif_sys_between(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_atom_or_int(term2);
	node *orig_term3 = NLIST_NEXT(args);
	node *term3;

	if (is_atom(term2) && strcmp(term2->val_s, "inf") &&
		strcmp(term2->val_s, "infinite"))
		{ QABORT(ABORT_INVALIDARGNOTINT); return 0; }

	if (!q->retry)
	{
		term3 = get_var(term3);
		nbr_t v = term1->val_i;
		put_int(q, q->curr_frame+term3->slot, v);
		allocate_frame(q);
	}
	else
	{
		term3 = get_int(term3);
		nbr_t v = term3->val_i + 1;

		if (is_integer(term2))
		{
			if (v > term2->val_i)
				return 0;
		}

		reset_arg(q, orig_term3, q->curr_frame);
		put_int(q, q->curr_frame+orig_term3->slot, v);
	}

	try_me_nofollow(q);
	return 1;
}

static int bif_sys_findnsols(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_term(var);				// FLAG_HIDDEN
	node *term0 = get_int(term0);
	node *term1 = get_term(term1);
	node *term2 = get_callable(term2);
	node *orig_term3 = NLIST_NEXT(args);
	tpl_query *who;
	stream *sp;

	if (!q->retry)
	{
		who = query_create_subquery(q, 0);
		if (!who) { QABORT(ABORT_OUTOFMEMORY); return 0; }
		sp = CALLOC(stream);
		sp->subqptr = who;
		node *n = make_stream(sp);
		put_env(q, var->slot, n, -1);
		n->refcnt--;
		sp->subqgoal = clone_term(who, term2);
		begin_query(who, sp->subqgoal);
		query_run(who);
	}
	else
	{
		reset_arg(q, orig_term3, q->curr_frame);
		sp = var->val_str;
		who = sp->subqptr;
	}

	if (!q->retry)
		allocate_frame(q);

	try_me_nofollow(q);
	node *acc = make_const_atom("[]", 0);
	node *end = NULL;
	int i = 1;

	while (who->ok)
	{
		who->latest_context = 0;
		node *res = clone_term(who, term1);

		if (!end)
		{
			FREE(acc);
			acc = make_list();
			NLIST_PUSH_BACK(&acc->val_l, res);
			end = acc;
		}
		else
		{
			node *tmp;
			NLIST_PUSH_BACK(&end->val_l, tmp=make_list());
			NLIST_PUSH_BACK(&tmp->val_l, res);
			end = tmp;
		}

		query_continue(who);

		if (i++ == term0->val_i)
			break;
	}

	if (!who->ok)
		trust_me(q);

	NLIST_PUSH_BACK(&end->val_l, make_const_atom("[]", 0));
	return unify_term(q, orig_term3, acc, q->latest_context);
}

static int bif_sys_listing(tpl_query *q)
{
	const char *functor = NULL;

	if (is_compound(q->curr_term))
	{
		node *args = get_args(q);
		node *term1 = get_next_arg(q, &args);

		if (term1)
		{
			if (!is_atom(term1))
			{ QABORT(ABORT_INVALIDARGNOTATOM); return 0; }
		}

		functor = term1->val_s;
	}

	//printf("DEBUG: listing module '%s'\n", q->curr_db->name);

	module *db = q->curr_db;
	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void**)&r) != NULL)
	{
		for (node *n = NLIST_FRONT(&r->clauses); n; n = NLIST_NEXT(n))
		{
			if (n->flags & FLAG_HIDDEN)
				continue;

			if (functor)
			{
				node *head = NLIST_NEXT(NLIST_FRONT(&n->val_l));

				if (is_atom(head))
				{
					if (strncmp(head->val_s, functor, strlen(functor)))
						continue;
				}
				else if (is_compound(head))
				{
					if (strncmp(NLIST_FRONT(&head->val_l)->val_s, functor, strlen(functor)))
						continue;
				}
			}

			print_term(q->pl, q, n, 1);
			printf(".");
			printf("\n");
		}
	}

	return 1;
}

static int bif_sys_listing_canonical(tpl_query *q)
{
	const char *functor = NULL;

	if (is_compound(q->curr_term))
	{
		node *args = get_args(q);
		node *term1 = get_next_arg(q, &args);

		if (term1)
		{
			if (!is_atom(term1))
			{ QABORT(ABORT_INVALIDARGNOTATOM); return 0; }
		}

		functor = term1->val_s;
	}

	//printf("DEBUG: listing module '%s'\n", q->curr_db->name);

	module *db = q->curr_db;
	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void**)&r) != NULL)
	{
		for (node *n = NLIST_FRONT(&r->clauses); n; n = NLIST_NEXT(n))
		{
			if (n->flags & FLAG_HIDDEN)
				continue;

			if (functor)
			{
				node *head = NLIST_NEXT(NLIST_FRONT(&n->val_l));

				if (is_atom(head))
				{
					if (strncmp(head->val_s, functor, strlen(functor)))
						continue;
				}
				else if (is_compound(head))
				{
					if (strncmp(NLIST_FRONT(&head->val_l)->val_s, functor, strlen(functor)))
						continue;
				}
			}

			print_term(q->pl, q, n, 2);
			printf(".");
			printf("\n");
		}
	}

	return 1;
}

static int bif_sys_time1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	tpl_query *who = query_create_subquery(q, 0);
	if (!who) { QABORT(ABORT_OUTOFMEMORY); return 0; }
	begin_query(who, term1);
	int ok = query_run(who);
	printf("(%.3lf s) %s\n", query_elapsed(who), ok?"yes":"no");
	query_destroy(who);
	return ok;
}

static int bif_sys_time2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	node *term2 = get_var(term2);
	tpl_query *who = query_create_subquery(q, 0);
	if (!who) { QABORT(ABORT_OUTOFMEMORY); return 0; }
	begin_query(who, term1);
	int ok = query_run(who);
	put_float(q, q->curr_frame+term2->slot, query_elapsed(who));
	query_destroy(who);
	return ok;
}

int bif_dbs_enter(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);

	if (!sl_get(&q->pl->mods, term1->val_s, (void**)&q->curr_db))
		q->curr_db = &q->pl->db;

	return 1;
}
static int bif_sys_writeln(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	dst += sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
	*dst++ = '\n';
	*dst = '\0';
	fwrite(tmpbuf, 1, dst-tmpbuf, stdout);
	free(tmpbuf);
	return 1;
}

static int bif_sys_writeln2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file_or_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char*)malloc(max_len+1);
	char *dst = tmpbuf;
	dst += sprint2_term(&tmpbuf, &max_len, &dst, q->pl, q, term2, 0);
	*dst++ = '\n';
	*dst = '\0';
	int ok;

	if (is_socket(term1))
		ok = session_write((session*)sp->sptr, tmpbuf, dst-tmpbuf);
	else
		ok = fwrite(tmpbuf, 1, dst-tmpbuf, sp->fptr);

	free(tmpbuf);
	return ok > 0;
}

#endif

void bifs_load_iso(void)
{
	DEFINE_BIF("true", 0, bif_iso_true);
	DEFINE_BIF("fail", 0, bif_iso_fail);
	DEFINE_BIF("false", 0, bif_iso_fail);
	DEFINE_BIF("halt", 0, bif_iso_halt);
	DEFINE_BIF("halt", 1, bif_iso_halt1);
	DEFINE_BIF("!", 0, bif_iso_cut);
	DEFINE_BIF("repeat", 0, bif_iso_repeat);
	DEFINE_BIF("is", 2, bif_iso_is);
	DEFINE_BIF("=", 2, bif_iso_unify);
	DEFINE_BIF("\\=", 2, bif_iso_notunify);
	DEFINE_BIF("--", 1, bif_iso_reverse);
	DEFINE_BIF("once", 1+1, bif_iso_once);
	DEFINE_BIF("call", 1+1, bif_iso_call);
	DEFINE_BIF("call", -1, bif_iso_calln);
	DEFINE_BIF("?-", -1, bif_iso_do);
	DEFINE_BIF(":-", -1, 0);
	DEFINE_BIF("\\+", 1, bif_iso_not);
	DEFINE_BIF(",", 2, bif_iso_and);
	DEFINE_BIF(";", 2, bif_iso_or);
	DEFINE_BIF("->", 2, bif_iso_then);
	DEFINE_BIF("+", 2, bif_iso_add);
	DEFINE_BIF("-", 2, bif_iso_subtract);
	DEFINE_BIF("*", 2, bif_iso_multiply);
	DEFINE_BIF("/", 2, bif_iso_divide);
	DEFINE_BIF("//", 2, bif_iso_div);
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
	DEFINE_BIF("xor", 2, bif_iso_bitxor);
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
	DEFINE_BIF("atan2", 1, bif_iso_atan2);
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
	DEFINE_BIF("open", 3, bif_iso_open3);
	DEFINE_BIF("open", 4, bif_iso_open4);
	DEFINE_BIF("close", 1, bif_iso_close);
	DEFINE_BIF("close", 2, bif_iso_close);
	DEFINE_BIF("write", 1, bif_iso_write);
	DEFINE_BIF("write", 2, bif_iso_write2);
	DEFINE_BIF("write_canonical", 1, bif_iso_write_canonical);
	DEFINE_BIF("write_canonical", 2, bif_iso_write_canonical2);
	DEFINE_BIF("write_term", 2, bif_iso_write_term);
	DEFINE_BIF("write_term", 3, bif_iso_write_term3);
	DEFINE_BIF("writeq", 1, bif_iso_writeq);
	DEFINE_BIF("writeq", 2, bif_iso_writeq2);
	DEFINE_BIF("nl", 0, bif_iso_nl);
	DEFINE_BIF("nl", 1, bif_iso_nl1);
	DEFINE_BIF("read", 1, bif_iso_read);
	DEFINE_BIF("read", 2, bif_iso_read2);
	DEFINE_BIF("read_term", 2, bif_iso_read_term);
	DEFINE_BIF("read_term", 3, bif_iso_read_term3);
	DEFINE_BIF("put_char", 1, bif_iso_put_char);
	DEFINE_BIF("put_char", 2, bif_iso_put_char2);
	DEFINE_BIF("put_byte", 1, bif_iso_put_byte);
	DEFINE_BIF("put_byte", 2, bif_iso_put_byte2);
	DEFINE_BIF("put_code", 1, bif_iso_put_code);
	DEFINE_BIF("put_code", 2, bif_iso_put_code2);
	DEFINE_BIF("flush_output", 0, bif_iso_flush_output);
	DEFINE_BIF("flush_output", 1, bif_iso_flush_output1);
	DEFINE_BIF("get_code", 1, bif_iso_get_code);
	DEFINE_BIF("get_byte", 1, bif_iso_get_byte);
	DEFINE_BIF("get_char", 1, bif_iso_get_char);
	DEFINE_BIF("get_code", 2, bif_iso_get_code2);
	DEFINE_BIF("get_byte", 2, bif_iso_get_byte2);
	DEFINE_BIF("get_char", 2, bif_iso_get_char2);
	DEFINE_BIF("peek_code", 1, bif_iso_peek_code);
	DEFINE_BIF("peek_byte", 1, bif_iso_peek_byte);
	DEFINE_BIF("peek_char", 1, bif_iso_peek_char);
	DEFINE_BIF("peek_code", 2, bif_iso_peek_code2);
	DEFINE_BIF("peek_byte", 2, bif_iso_peek_byte2);
	DEFINE_BIF("peek_char", 2, bif_iso_peek_char2);
	DEFINE_BIF("at_end_of_stream", 0, bif_iso_at_end_of_stream);
	DEFINE_BIF("at_end_of_stream", 1, bif_iso_at_end_of_stream1);
	DEFINE_BIF("set_stream_position", 2, bif_iso_set_stream_position);
	DEFINE_BIF("set_input", 1, bif_iso_set_input);
	DEFINE_BIF("set_output", 1, bif_iso_set_output);
	DEFINE_BIF("curr_input", 1, bif_iso_curr_input);
	DEFINE_BIF("curr_output", 1, bif_iso_curr_output);
	DEFINE_BIF("length", 2, bif_iso_length);
	DEFINE_BIF("sort", 2, bif_iso_sort);
	DEFINE_BIF("keysort", 2, bif_iso_keysort);
	DEFINE_BIF("asserta", 1, bif_iso_asserta);
	DEFINE_BIF("assertz", 1, bif_iso_assertz);
	DEFINE_BIF("retract", 1, bif_iso_retract);
	DEFINE_BIF("retractall", 1, bif_iso_retractall);
	DEFINE_BIF("abolish", 1, bif_iso_abolish);
	DEFINE_BIF("curr_predicate", 1, bif_iso_curr_predicate);
	DEFINE_BIF("set_prolog_flag", 2, bif_iso_set_prolog_flag);
	DEFINE_BIF("curr_prolog_flag", 2, bif_iso_curr_prolog_flag);
	DEFINE_BIF("predicate_property", 2, bif_iso_predicate_property);
	DEFINE_BIF("findall", 3, bif_iso_findall);
	DEFINE_BIF("bagof", 3, bif_iso_bagof);
	DEFINE_BIF("setof", 3, bif_iso_setof);

	//DEFINE_BIF("stream_property", 2, bif_iso_stream_property);

	// These are not ISO-Prolog but are common to just
	// about every Prolog so they are included here...

#ifndef ISO_ONLY
	DEFINE_BIF("a_", 1, bif_iso_asserta);
	DEFINE_BIF("z_", 1, bif_iso_assertz);
	DEFINE_BIF("r_", 1, bif_iso_retract);

	DEFINE_BIF("div", 2, bif_iso_div);
	DEFINE_BIF("assert", 1, bif_iso_assertz);
	DEFINE_BIF("consult", 1, bif_sys_consult);
	DEFINE_BIF("deconsult", 1, bif_sys_deconsult);
	DEFINE_BIF("reconsult", 1, bif_sys_reconsult);
	DEFINE_BIF("listing", 0, bif_sys_listing);
	DEFINE_BIF("listing", 1, bif_sys_listing);
	DEFINE_BIF("listing_canonical", 0, bif_sys_listing_canonical);
	DEFINE_BIF("listing_canonical", 1, bif_sys_listing_canonical);
	DEFINE_BIF("abolish", 2, bif_sys_abolish);
	DEFINE_BIF("writeln", 1, bif_sys_writeln);
	DEFINE_BIF("writeln", 2, bif_sys_writeln2);
	DEFINE_BIF("time", 1, bif_sys_time1);
	DEFINE_BIF("time", 2, bif_sys_time2);
	DEFINE_BIF("between", 3, bif_sys_between);
	DEFINE_BIF("is_list", 1, bif_sys_is_list);
	DEFINE_BIF("is_struct", 1, bif_sys_is_struct);
	DEFINE_BIF("is_tuple", 1, bif_sys_is_tuple);
	DEFINE_BIF("is_stream", 1, bif_sys_is_stream);
	DEFINE_BIF("term_to_atom", 2, bif_sys_term_to_atom);
	DEFINE_BIF("term_to_blob", 2, bif_sys_term_to_blob);
	DEFINE_BIF("findnsols", 1+4, bif_sys_findnsols);
	DEFINE_BIF("asserta", 2, bif_sys_asserta);
	DEFINE_BIF("assertz", 2, bif_sys_assertz);
	DEFINE_BIF("erase", 1, bif_sys_erase);
	DEFINE_BIF("enter", 1, bif_dbs_enter);
#endif
}

