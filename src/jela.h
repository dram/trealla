#ifndef JELA_H
#define JELA_H

extern void begin_query(tpl_query *q, node *term);
extern void allocate_frame(tpl_query *q);
extern void reallocate_frame(tpl_query *q);
extern void prepare_frame(tpl_query *q, unsigned frame_size);
extern int call(tpl_query *q);
extern void run_me(tpl_query *q);
extern int match(tpl_query *q);
extern void try_me2(tpl_query *q, int nofollow, int nochoice, int noretry);
extern int retry_me(tpl_query *q);
extern void trust_me(tpl_query *q);
extern int query_inline(tpl_query *q);
extern void query_reset(tpl_query *q);
extern int unify(tpl_query *q, node *term1, unsigned context1, node *term2, unsigned context2);

#define unify_term(q, term1, term2, frame) unify(q, term1, q->curr_context, term2, frame)
#define try_me(q) try_me2(q, 0, 0, 0)
#define try_me_nofollow(q) try_me2(q, 1, 0, 0)
#define try_me_nochoice(q) try_me2(q, 0, 1, 0)
#define try_me_noretry(q) try_me2(q, 0, 0, 1)

inline static int unify_int(tpl_query *q, node *term, nbr_t v)
{
	node *n = make_quick_int(v);
	int ok = unify_term(q, term, n, q->c.curr_frame);
	term_heapcheck(n);
	return ok;
}

inline static int unify_float(tpl_query *q, node *term, flt_t v)
{
	node *n = make_float(v);
	int ok = unify_term(q, term, n, q->c.curr_frame);
	term_heapcheck(n);
	return ok;
}

inline static int unify_atom(tpl_query *q, node *term, char *v)
{
	node *n = make_atom(v);
	int ok = unify_term(q, term, n, q->c.curr_frame);
	term_heapcheck(n);
	return ok;
}

inline static int unify_const_atom(tpl_query *q, node *term, const char *v)
{
	node *n = make_const_atom(v);
	int ok = unify_term(q, term, n, q->c.curr_frame);
	term_heapcheck(n);
	return ok;
}

#endif
