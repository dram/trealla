#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <locale.h>

#ifdef _WIN32
#include <io.h>
#define snprintf _snprintf
#define fseeko _fseeki64
#define ftello _ftelli64
#define isatty _isatty
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

#ifndef ISO_ONLY
static int bif_xtra_retractw(tpl_query *q) { return bif_retract2(q, 1); }

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

	rebase_clause(q, n);
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

		bif_asserta(q, n, NULL);

		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);
	}

	put_ptr(q, term2, term2_ctx, n);
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

	rebase_clause(q, n);
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

		bif_assertz(q, n, NULL);

		if (!q->in_tran)
			DBUNLOCK(q->c.curr_db);
	}

	put_ptr(q, term2, term2_ctx, n);
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

	int ok = bif_retract(q, n, n, NULL);

	if (did_lock)
		DBUNLOCK(q->c.curr_db);

	return ok;
}
#endif

static int bif_clause(tpl_query *q, int wait)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *term3 = get_next_arg(q, &args);
	unsigned term3_ctx = q->c.curr_frame;
	node *save_match = NULL;
	node *head = NULL;
	rule *r = NULL;
	int did_lock = 0, nochoice = 0;

	if ((!term3 || !is_ptr(term3)) && is_var(term1)) {
		QABORT(ABORT_INVALIDARGISVAR);
		return 0;
	}

	if (!q->retry) {
		const char *functor;
		int arity = 0;

		if (is_var(term1)) {
			node *ref = term3->val_ptr;
			rule *r = q->curr_rule;

			if (!r || !ref) {
				QABORT(ABORT_NOTEXISTREFERENCE);
				return 0;
			}

			if (!q->in_tran) {
				did_lock = 1;
				DBLOCK(q->c.curr_db);
			}

			int found = 0;

			for (node *n = NLIST_FRONT(&r->val_l); n; n = term_next(n)) {
				if (n == ref) {
					found = 1;
					break;
				}
			}

			if (did_lock)
				DBUNLOCK(q->c.curr_db);

			if (!found || is_deleted(ref)) {
				QABORT(ABORT_ISDELETED);
				return 0;
			}

			save_match = q->c.curr_match = ref;
			nochoice = 1;
		}
		else {
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
				save_match = q->c.curr_match = NULL;
			}
			else {
				save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);
			}

			if (did_lock)
				DBUNLOCK(q->c.curr_db);

			q->curr_rule = r;
		}
	}
	else {
		r = q->curr_rule;

		if (!q->c.curr_match) {
			save_match = q->c.curr_match = NLIST_FRONT(&r->val_l);
}
		else {
			r = q->curr_rule;
			save_match = q->c.curr_match;
			q->c.curr_match = term_next(q->c.curr_match);
		}
	}

	allocate_frame(q);

	while (q->c.curr_match) {
		if (is_hidden(q->c.curr_match) || is_deleted(q->c.curr_match)) {
			q->c.curr_match = term_next(q->c.curr_match);
			continue;
		}

		unsigned frame_size = q->c.curr_match->frame_size;
		prepare_frame(q, frame_size);
		head = term_firstarg(q->c.curr_match);
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
		q->latest_context = term1_ctx;

		// printf("***(%u) ", q->c.frame_size); TERM_PRINT(term1); q->latest_context = q->c.env_point;
		// printf(" <==> "); TERM_PRINT(head); printf(" (%u)\n", q->c.curr_match->frame_size);

		if (!unify(q, term1, term1_ctx, head, q->c.env_point)) {
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

	if (!q->c.curr_match && !wait) {
		return 0;
	}

	int is_eof = !q->c.curr_match;

	if (is_eof && wait) {
		q->c.curr_match = save_match;
#ifndef ISO_ONLY
		try_me_nofollow(q);

		if (!q->in_tran) {
			did_lock = 1;
			DBLOCK(q->c.curr_db);
		}

		sl_set(&r->procs, (const char *)q, NULL);

		if (did_lock) {
			DBUNLOCK(q->c.curr_db);
		}

		PIDLOCK(q->pl);

		if (q->tmo_msecs > 0) {
			q->tmo_when_msecs = gettimeofday_usec() / 1000;
			q->tmo_when_msecs += q->tmo_msecs;
			q->is_idle = 1;
			sl_set(&q->pl->idle, (const char *)q, NULL);
		}

		return process_yield_locked(q);
#endif
	}

	if (!nochoice) {
		try_me_nofollow(q);
		q->c.env_point += q->c.curr_match->frame_size;	// FIXME: make nice
	}

	if (term3 && is_var(term3))
		put_ptr(q, term3, term3_ctx, q->c.curr_match);

	node *body = term_next(head);
	return unify(q, term2, term2_ctx, body, q->c.env_point);
}

#ifndef ISO_ONLY
static int bif_xtra_clausew(tpl_query *q) { return bif_clause(q, 1); }
#endif

#ifndef ISO_ONLY
static int bif_xtra_split_string_4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	node *term4 = get_var(term4);
	const char *src = VAL_S(term1);

	const char *src_pad = VAL_S(term3);
	int pads[256];

	for (int i = 0; *src_pad && (i < 255); i++) {
		int ch = get_char_utf8(&src_pad);
		pads[i] = ch;
		pads[1+1] = 0;
	}

	const char *save_src = src;
	int ch = get_char_utf8(&src);
	int skip = 0;

	while (*src) {
		for (int i = 0; pads[i] && (i < 255); i++) {
			if (ch == pads[i]) {
				save_src = src;
				ch = get_char_utf8(&src);
				skip = 1;
				break;
			}
		}

		if (!skip)
			break;
	}

	src = save_src;

	if (!*src) {
		node *tmp = make_const_atom("[]");
		put_env(q, term4, term4_ctx, tmp, q->c.curr_frame);
		term_heapcheck(tmp);
		return 1;
	}

	node *l = make_list();
	node *save_l = l;
	char *dstbuf = (char *)malloc(LEN_S(term1) + 1);
	const char *src_sep = VAL_S(term2);
	int seps[256];

	for (int i = 0; *src_sep && (i < 255); i++) {
		int ch = get_char_utf8(&src_sep);
		seps[i] = ch;
		seps[1+1] = 0;
	}

	while (*src) {
		char *dst = dstbuf;
		int ch = get_char_utf8(&src), found = 0;


		while (!found) {
			for (int i = 0; seps[i] && !found && (i < 255); i++) {
				if (ch == seps[i]) {
					found = 1;
					break;
				}
			}

			if (found)
				break;

			dst += put_char_utf8(dst, ch);

			if (!*src)
				break;

			ch = get_char_utf8(&src);
		}

		*dst = '\0';
		node *tmp = make_atom(strdup(dstbuf));
		dstbuf[0] = '\0';
		term_append(l, tmp);

		const char *save_src = src;
		ch = get_char_utf8(&src);
		int skip = 0;

		while (*src) {
			for (int i = 0; pads[i] && (i < 255); i++) {
				if (ch == pads[i]) {
					save_src = src;
					ch = get_char_utf8(&src);
					skip = 1;
					break;
				}
			}

			if (!skip)
				break;
		}

		src = save_src;

		if (!*src)
			break;

		l = term_append(l, make_list());
	}

	if (!*dstbuf) {
		l = term_append(l, make_list());
		node *tmp = make_const_atom("");
		term_append(l, tmp);
	}

	free(dstbuf);
	term_append(l, make_const_atom("[]"));
	put_env(q, term4, term4_ctx, save_l, q->c.curr_frame);
	term_heapcheck(save_l);
	return 1;
}

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
		maxn = VAL_INT(term2);

	if (!q->retry) {
		term3 = get_term(term3);

		if (is_var(term3)) {
			nbr_t v = VAL_INT(term1);
			put_int(q, term3, term3_ctx, v);
		}
		else {
			nbr_t v = VAL_INT(term3);
			return (v >= VAL_INT(term1)) && (v <= VAL_INT(term2));
		}
	}
	else {
		term3 = get_next_arg(q, &args);
		nbr_t v = term3->val_i + 1;

		if (v > maxn)
			return 0;

		reset_arg(q, orig_term3, q->c.curr_frame);
		put_int(q, orig_term3, q->c.curr_frame, v);
	}

	allocate_frame(q);
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
	node *term2 = get_term(term2);
	node *tmp = NULL;

	if (is_atom(term1)) {
		tmp = make_compound();
		term_append(tmp, clone_term(q, term1));
	}
	else
		tmp = clone_term(q, term1);

	term_append(tmp, clone_term(q, term2));
	node *param = get_next_arg(q, &args);
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

	put_env(q, var, var_ctx, tmp, q->c.curr_frame);
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
	put_float(q, term2, term2_ctx, query_elapsed(subq));
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
		ok = unify_atom(q, term2, term1_ctx, strdup(tmpbuf));
		free(tmpbuf);
	}
	else
		ok = unify(q, term1, term1_ctx, term2, -1);

	return ok;
}

static int bif_xtra_term_to_blob_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_nonvar(term1);
	node *term2 = get_var(term2);
	int ok;

	if (is_atom(term1)) {
		node *n = make_blob(strdup(VAL_S(term1)), strlen(VAL_S(term1)));
		ok = unify(q, term2, term2_ctx, n, -1);
		term_heapcheck(n);
	}
	else if (!is_blob(term1)) {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
		node *n = make_blob(strdup(tmpbuf), dst - tmpbuf);
		ok = unify(q, term2, term2_ctx, n, -1);
		term_heapcheck(n);
		free(tmpbuf);
	}
	else
		ok = unify(q, term1, term1_ctx, term2, -1);

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

		term_append(s, clone_term(q, subst(q, l2, this_context)));
		begin_query(subq, s);
		int ok = query_run(subq);
		query_destroy(subq);
		term_heapcheck(s);

		if (!ok)
			return 0;

		l2 = subst(q, term_next(l2), this_context);
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

		term_append(s, clone_term(q, subst(q, l2, this_context)));
		term_append(s, clone_term(q, subst(q, l3, this_context)));
		begin_query(subq, s);
		int ok = query_run(subq);
		query_destroy(subq);
		term_heapcheck(s);

		if (!ok)
			return 0;

		l2 = subst(q, term_next(l2), this_context);
		l3 = subst(q, term_next(l3), this_context);
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
	unsigned term4_ctx = q->latest_context;
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
		put_env(q, var, var_ctx, n, -1);
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
	return unify(q, term4, term4_ctx, acc, term2_ctx);
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
			if (is_hidden(n) && !functor)
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

			term_print(q->pl, NULL, n, 2);
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
			if (is_hidden(n) && !functor)
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

			term_print(q->pl, NULL, n, 3);
			printf(".\n");
		}
	}

	return 1;
}

static int bif_xtra_rdiv(tpl_query *q)
{
	node *args = get_args(q);
	eval(q, &args);
	node nv1 = q->nv;
	eval(q, &args);
	node nv2 = q->nv;

	if (is_rational(&nv1) || is_rational(&nv2)) {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}
	else if (is_number(&nv1) && is_number(&nv2)) {
		if (VAL_INT(&nv2) == 0) {
			QABORT(ABORT_INVALIDARGDIVIDEBYZERO);
			return 0;
		}

		q->nv.val_num = VAL_INT(&nv1);
		q->nv.val_den = VAL_INT(&nv2);
	}
	else {
		QABORT(ABORT_TYPEERROR);
		return 0;
	}

	q->nv.flags = TYPE_RATIONAL;
	return 1;
}

static int bif_xtra_rational_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_int(term2);
	q->nv.flags = TYPE_RATIONAL;
	q->nv.val_num = VAL_INT(term1);
	q->nv.val_den = VAL_INT(term2);
	reduce(&q->nv);
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

static int bif_xtra_fixed_4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_int(term2);
	node *term3 = get_int(term3);
	node *term4 = get_var(term4);
	int digs = VAL_INT(term2);
	int prec = VAL_INT(term3);

	if ((digs < 0) || (digs > 512)) {
		QABORT(ABORT_INVALIDARGOUTOFRANGE);
		return 0;
	}

	if (digs > prec) {
		QABORT(ABORT_INVALIDARGOUTOFRANGE);
		return 0;
	}

	struct lconv *lc = localeconv();
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
	size_t len = dst - tmpbuf;
	char *dstbuf = (char *)malloc(len + digs + 10);
	const char *src = tmpbuf;
	dst = dstbuf;
	int pad = 0, offset = prec - digs;

	len -= offset;

	if (len < digs) {
		pad = 1;
		*dst++ = *lc->decimal_point;

		int i = len;

		while (i++ < digs)
			*dst++ = '0';
	}

	while (*src && (len-- > digs))
		*dst++ = *src++;

	len += offset;

	if (!pad && *src) {
		*dst++ = *lc->decimal_point;
	}

	while (*src && (len-- >= offset))
		*dst++ = *src++;

	*dst = '\0';
	free(tmpbuf);
	put_atom(q, term4, term4_ctx, dstbuf);
	return 1;
}

static int bif_xtra_random_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	double v = (double)rand_r(&q->seed) / (double)RAND_MAX;
	put_float(q, term1, term1_ctx, v);
	return 1;
}

static uint32_t jenkins_one_at_a_time_hash(const char *key)
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
		put_int(q, term2, term2_ctx, jenkins_one_at_a_time_hash(VAL_S(term1)));
	}
	else {
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
		put_int(q, term2, term2_ctx, jenkins_one_at_a_time_hash(tmpbuf));
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

	int ok = unify(q, term2, term2_ctx, n, -1);
	term_heapcheck(n);
	return ok;
}

static int bif_xtra_read_term_from_atom_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	node *term3 = get_atom_or_list(term3);
	char *src = VAL_S(term1);
	char *line = (char *)malloc(strlen(src) + 10);
	sprintf(line, "%s.", src);
	int ok = read_term(q, line, term2, term2_ctx, term3, NULL);
	free(line);
	return ok;
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
	node *term1 = get_atom_or_stream(term1);
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
		ok = fwrite(tmpbuf, 1, dst - tmpbuf, get_output_stream(term1));

	free(tmpbuf);
	return ok > 0;
}

static int bif_xtra_unload_file(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	dir_unload_file(q->lex, term1);
	return 1;
}
#endif

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
		q->trace = VAL_INT(term1);

	return unify_int(q, term1, q->latest_context, q->trace);
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
		return unify_const_atom(q, term2, q->latest_context, "built_in");

	if (check_dynamic(q->c.curr_db, functarity))
		return unify_const_atom(q, term2, q->latest_context, "dynamic");

	if (check_static(q->c.curr_db, functarity))
		return unify_const_atom(q, term2, q->latest_context, "static");

	return 0;
}

static int bif_xtra_getenv_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_var(term2);
	const char *value = getenv(VAL_S(term1));

	if (!value)
		return 0;

	return unify_atom(q, term2, q->latest_context, strdup(value));
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
			node *n = subst(q, head, this_context);

			if (!is_integer(n) && !is_bignum(n)) {
				QABORT(ABORT_INVALIDARGNOTINT);
				return 0;
			}

			nbr_t v = VAL_INT(n);

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
			l = subst(q, tail, this_context);
		}

		*dst = '\0';
		int numeric = NUM_NONE;
		nbr_t v;

		if (isdigit(*dstbuf))
			parse_number(dstbuf, &v, &numeric);

		int ok;

		if (isdigit(*dstbuf) && (numeric == NUM_INT))
			ok = unify_int(q, term1, term1_ctx, v);
		else if (isdigit(*dstbuf) && (numeric == NUM_BIGNUM)) {
			node *n = make_bignum(dstbuf);
			ok = unify(q, term1, term1_ctx, n, -1);
			term_heapcheck(n);
		}
		else if (numeric == NUM_REAL)
			ok = unify_float(q, term1, term1_ctx, strtod(dstbuf, NULL));
		else
			ok = unify_atom(q, term1, term1_ctx, strdup(dstbuf));

		free(dstbuf);
		return ok;
	}

	node *n;

	if (is_atom(term1) && !*VAL_S(term1)) {
		n = make_const_atom("[]");
	}
	else if (is_atom(term1) || is_number(term1)) {
		size_t buflen = FUNCTOR_SIZE;

		if (is_atom(term1) && (LEN_S(term1) > FUNCTOR_SIZE))
			buflen = LEN_S(term1) + 1;

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

			l = term_append(l, make_list());
		}

		term_append(l, make_const_atom("[]"));
		free(dstbuf);
		n = save_l;
	}
	else {
		QABORT(ABORT_INVALIDARGNOTATOMIC);
		return 0;
	}

	int ok = unify(q, term2, term2_ctx, n, -1);
	term_heapcheck(n);
	return ok;
}

static int bif_xtra_atomic_concat_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atomic(term1);
	node *term2 = get_atomic(term2);
	node *term3 = get_var(term3);
	size_t dstlen = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(dstlen + 1);
	char *dst = tmpbuf;
	dst += term_sprint2(&tmpbuf, &dstlen, &dst, q->pl, q, term1, 0);
	dst += term_sprint2(&tmpbuf, &dstlen, &dst, q->pl, q, term2, 0);
	node *n;

	if (is_blob(term1) || is_blob(term2))
		n = make_blob(tmpbuf, dst-tmpbuf);
	else
		n = make_atom(tmpbuf);

	put_env(q, term3, term3_ctx, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_xtra_blob_concat_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	size_t len = LEN_S(term1) + LEN_S(term2);
	char *tmp = (char *)malloc(len + 1);

	if (LEN_S(term1) > 0)
		memcpy(tmp, VAL_S(term1), LEN_S(term1));

	if (LEN_S(term2) > 0)
		memcpy(tmp + LEN_S(term1), VAL_S(term2), LEN_S(term2));

	tmp[len] = '\0';
	node *n;

#ifndef ISO_ONLY
	if (is_blob(term1) || is_blob(term2))
		n = make_blob(tmp, len);
	else
#endif
		n = make_atom(tmp);

	int ok = unify(q, term3, term3_ctx, n, -1);
	term_heapcheck(n);
	return ok;
}

static int bif_xtra_atomic_list_concat_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	node *term2 = get_var(term2);
	q->latest_context = term1_ctx;
	node *l = term1;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	int any_blobs = 0;

	if (is_atom(term1) && strcmp(VAL_S(term1), "[]")) {
		QABORT(ABORT_INVALIDARGNOTLIST);
		return 0;
	}

	while (is_list(l)) {
		node *head = term_firstarg(l);
		unsigned this_context = q->latest_context;
		node *n = subst(q, head, this_context);
		dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, n, 0);
		node *tail = term_next(head);
		l = subst(q, tail, this_context);
	}

	*dst = '\0';
	node *n;

	if (any_blobs)
		n = make_blob(tmpbuf, dst-tmpbuf);
	else
		n = make_atom(tmpbuf);

	put_env(q, term2, term2_ctx, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_xtra_atomic_list_concat_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	q->latest_context = term1_ctx;
	node *l = term1;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	int any_blobs = 0;

	if (is_atom(term1) && strcmp(VAL_S(term1), "[]")) {
		QABORT(ABORT_INVALIDARGNOTLIST);
		return 0;
	}

	while (is_list(l)) {
		node *head = term_firstarg(l);
		unsigned this_context = q->latest_context;
		node *n = subst(q, head, this_context);
		dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, NULL, n, 0);
		node *tail = term_next(head);
		l = subst(q, tail, this_context);

		if (is_list(l))
			dst += sprintf(dst, "%s", VAL_S(term2));
	}

	*dst = '\0';
	node *n;

	if (any_blobs)
		n = make_blob(tmpbuf, dst-tmpbuf);
	else
		n = make_atom(tmpbuf);

	put_env(q, term3, term3_ctx, n, -1);
	term_heapcheck(n);
	return 1;
}
#endif

void bifs_load_xtra(void)
{
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
	DEFINE_BIF("term_to_blob", 2, bif_xtra_term_to_blob_2);
	DEFINE_BIF("enter", 1, bif_xtra_enter);

	// These are used in the database log...

	DEFINE_BIF("a_", 1, bif_iso_asserta);
	DEFINE_BIF("z_", 1, bif_iso_assertz);
	DEFINE_BIF("r_", 1, bif_iso_retract);
	DEFINE_BIF("t_", 0, bif_iso_true);

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
	DEFINE_BIF("name", 2, bif_xtra_name_2);
	DEFINE_BIF("atomic_concat", 3, bif_xtra_atomic_concat_3);
	DEFINE_BIF("blob_concat", 3, bif_xtra_blob_concat_3);
	DEFINE_BIF("atomic_list_concat", 2, bif_xtra_atomic_list_concat_2);
	DEFINE_BIF("atomic_list_concat", 3, bif_xtra_atomic_list_concat_3);
	DEFINE_BIF("fixed", 4, bif_xtra_fixed_4);
	DEFINE_BIF("rational", 2, bif_xtra_rational_2);
	DEFINE_BIF("rdiv", 2, bif_xtra_rdiv);

#if USE_SSL
	DEFINE_BIF("unbounded", 1, bif_xtra_unbounded_1);
#endif

	DEFINE_BIF("time", 1, bif_xtra_time_1);
	DEFINE_BIF("time", 2, bif_xtra_time_2);
	DEFINE_BIF("between", 3, bif_xtra_between_3);
	DEFINE_BIF("phrase", 1 + 2, bif_xtra_phrase);
	DEFINE_BIF("phrase", 1 + 3, bif_xtra_phrase);
	DEFINE_BIF("predicate_property", 2, bif_xtra_predicate_property_2);
	DEFINE_BIF("garbage_collect", 0, bif_iso_true);
	DEFINE_BIF("split_string", 4, bif_xtra_split_string_4);

	DEFINE_BIF("linda:out", 1, bif_linda_out);
	DEFINE_BIF("linda:in", 1, bif_xtra_retractw);
	DEFINE_BIF("linda:inp", 1, bif_iso_retract);
	DEFINE_BIF("linda:rd", 1, bif_xtra_clausew);
	DEFINE_BIF("linda:rdp", 1, bif_iso_clause);
#endif
}
