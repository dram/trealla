#ifndef BIFS_H
#define BIFS_H

#ifdef DEBUG
#include <assert.h>
#endif

#include "internal.h"
#include "utf8.h"

#define is_compound(n) (((n)->flags & TYPE_COMPOUND) ? 1 : 0)
#define is_list(n) (((n)->flags & FLAG_LIST) ? 1 : 0)
#define is_structure(n) (is_compound(n) && !is_list(n) ? 1 : 0)
#define is_callable(n) (is_structure(n) || is_atom(n) ? 1 : 0)
#define is_var(n) (((n)->flags & TYPE_VAR) ? 1 : 0)
#define is_float(n) (((n)->flags & TYPE_FLOAT) ? 1 : 0)
#define is_integer(n) (((n)->flags & TYPE_INTEGER) ? 1 : 0)
#define is_bignum(n) (((n)->flags & TYPE_BIGNUM) ? 1 : 0)
#define is_number(n) (is_integer(n) || is_float(n) || is_bignum(n) ? 1 : 0)
#define is_atom(n) (((n)->flags & TYPE_ATOM) ? 1 : 0)
#define is_atomic(n) (is_atom(n) || is_number(n) ? 1 : 0)

#define is_quoted(n) (((n)->flags & FLAG_QUOTED) ? 1 : 0)
#define is_anon(n) (((n)->flags & FLAG_ANON) ? 1 : 0)
#define is_tuple(n) (((n)->flags & FLAG_TUPLE) ? 1 : 0)
#define is_builtin(n) (((n)->flags & FLAG_BUILTIN) ? 1 : 0)
#define is_file(n) (((n)->flags & FLAG_FILE) ? 1 : 0)
#define is_stream(n) (((n)->flags & FLAG_STREAM) ? 1 : 0)
#define is_clause(n) (((n)->flags & FLAG_CLAUSE) ? 1 : 0)
#define is_fact(n) (((n)->flags & FLAG_FACT) ? 1 : 0)
#define is_heap(n) (((n)->flags & FLAG_HEAP) ? 1 : 0)
#define is_ptr(n) (((n)->flags & FLAG_PTR) ? 1 : 0)
#define is_deleted(n) (((n)->flags & FLAG_DELETED) ? 1 : 0)
#define is_dynamic(n) (((n)->flags & FLAG_DYNAMIC) ? 1 : 0)
#define is_promoted(n) (((n)->flags & FLAG_PROMOTED) ? 1 : 0)
#define is_hidden(n) (((n)->flags & FLAG_HIDDEN) ? 1 : 0)
#define is_rule(n) (((n)->flags & FLAG_RULE) ? 1 : 0)
#define is_noargs(n) (((n)->flags & FLAG_NOARGS) ? 1 : 0)
#define is_tailrecursive(n) (((n)->flags & FLAG_TAILRECURSIVE) ? 1 : 0)
#define is_passthru(n) (((n)->flags & FLAG_PASSTHRU) ? 1 : 0)

#ifndef ISO_ONLY
#define is_blob(n) (((n)->flags & FLAG_BLOB) ? 1 : 0)
#define is_pid(n) (((n)->flags & FLAG_PID) ? 1 : 0)
#define is_socket(n) (((n)->flags & FLAG_SOCKET) ? 1 : 0)
#define is_storage(n) (((n)->flags & FLAG_DBS_STORAGE) ? 1 : 0)
#endif

#define get_arity(q) term_arity(q->c.curr_term)
#define get_args(q)                                                                                                            \
	term_first(q->c.curr_term);                                                                                                \
	if (!term_count(q->c.curr_term)) {                                                                                         \
		QABORT(ABORT_INVALIDARGMISSING);                                                                                       \
		return 0;                                                                                                              \
	}
#define get_term(t)                                                                                                            \
	get_next_arg(q, &args);                                                                                                    \
	if (t == NULL) {                                                                                                           \
		QABORT(ABORT_INVALIDARGMISSING);                                                                                       \
		return 0;                                                                                                              \
	}

#define get_atomic(t)                                                                                                          \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atomic(t)) {                                                                                                       \
		QABORT(ABORT_INVALIDARGNOTATOMIC);                                                                                     \
		return 0;                                                                                                              \
	}
#define get_atom(t)                                                                                                            \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t)) {                                                                                                         \
		QABORT(ABORT_INVALIDARGNOTATOM);                                                                                       \
		return 0;                                                                                                              \
	}
#define get_atom_or_int(t)                                                                                                     \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_integer(t) && !is_bignum(t)) {                                                                      \
		QABORT(ABORT_INVALIDARGNOTATOMORINT);                                                                                  \
		return 0;                                                                                                              \
	}
#define get_atom_or_list(t)                                                                                                    \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_list(t)) {                                                                                          \
		QABORT(ABORT_INVALIDARGNOTATOMORLIST);                                                                                 \
		return 0;                                                                                                              \
	}
#define get_atom_or_list_or_var(t)                                                                                             \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_list(t) && !is_var(t)) {                                                                            \
		QABORT(ABORT_INVALIDARGNOTATOMORLIST);                                                                                 \
		return 0;                                                                                                              \
	}
#define get_atom_or_var(t)                                                                                                     \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_var(t)) {                                                                                           \
		QABORT(ABORT_INVALIDARGNOTATOMORVAR);                                                                                  \
		return 0;                                                                                                              \
	}
#define get_nbr(t)                                                                                                             \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_number(t)) {                                                                                                       \
		QABORT(ABORT_INVALIDARGNOTNBR);                                                                                        \
		return 0;                                                                                                              \
	}
#define get_nbr_or_var(t)                                                                                                      \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_number(t) && !is_var(t)) {                                                                                         \
		QABORT(ABORT_INVALIDARGNOTNBRORVAR);                                                                                   \
		return 0;                                                                                                              \
	}
#define get_int(t)                                                                                                             \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_integer(t) && !is_bignum(t)) {                                                                                     \
		QABORT(ABORT_INVALIDARGNOTINT);                                                                                        \
		return 0;                                                                                                              \
	}
#define get_int_or_var(t)                                                                                                      \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_integer(t) && !is_bignum(t) && !is_var(t)) {                                                                       \
		QABORT(ABORT_INVALIDARGNOTINTORVAR);                                                                                   \
		return 0;                                                                                                              \
	}
#define get_ptr(t)                                                                                                             \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_ptr(t)) {                                                                                                          \
		QABORT(ABORT_INVALIDARGNOTPTR);                                                                                        \
		return 0;                                                                                                              \
	}
#define get_var(t)                                                                                                             \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_var(t)) {                                                                                                          \
		QABORT(ABORT_INVALIDARGNOTVAR);                                                                                        \
		return 0;                                                                                                              \
	}
#define get_nonvar(t)                                                                                                          \
	get_next_arg(q, &args);                                                                                                    \
	if (is_var(t)) {                                                                                                           \
		QABORT(ABORT_INVALIDARGISVAR);                                                                                         \
		return 0;                                                                                                              \
	}
#define get_compound(t)                                                                                                        \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_compound(t)) {                                                                                                     \
		QABORT(ABORT_INVALIDARGNOTCOMPOUND);                                                                                   \
		return 0;                                                                                                              \
	}
#define get_callable(t)                                                                                                        \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_callable(t)) {                                                                                                     \
		QABORT(ABORT_INVALIDARGNOTCALLABLE);                                                                                   \
		return 0;                                                                                                              \
	}
#define get_list_or_callable(t)                                                                                                \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_callable(t) && !is_list(t)) {                                                                                      \
		QABORT(ABORT_INVALIDARGNOTCALLABLE);                                                                                   \
		return 0;                                                                                                              \
	}
#define get_structure(t)                                                                                                       \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_structure(t)) {                                                                                                    \
		QABORT(ABORT_INVALIDARGNOTSTRUCTURE);                                                                                  \
		return 0;                                                                                                              \
	}
#define get_list(t)                                                                                                            \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_list(t)) {                                                                                                         \
		QABORT(ABORT_INVALIDARGNOTLIST);                                                                                       \
		return 0;                                                                                                              \
	}
#define get_tuple(t)                                                                                                           \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_tuple(t)) {                                                                                                        \
		QABORT(ABORT_INVALIDARGNOTTUPLE);                                                                                      \
		return 0;                                                                                                              \
	}
#define get_list_or_var(t)                                                                                                     \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_list(t) && !is_var(t)) {                                                                                           \
		QABORT(ABORT_INVALIDARGNOTLISTORVAR);                                                                                  \
		return 0;                                                                                                              \
	}
#define get_atom_or_file(t)                                                                                                    \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_file(t)) {                                                                                          \
		QABORT(ABORT_INVALIDARGNOTFILE);                                                                                       \
		return 0;                                                                                                              \
	}
#define get_atom_or_stream(t)                                                                                                  \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_atom(t) && !is_stream(t)) {                                                                                        \
		QABORT(ABORT_INVALIDARGNOTSTREAM);                                                                                     \
		return 0;                                                                                                              \
	}
#define get_stream(t)                                                                                                          \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_stream(t)) {                                                                                                       \
		QABORT(ABORT_INVALIDARGNOTSTREAM);                                                                                     \
		return 0;                                                                                                              \
	}
#define get_file(t)                                                                                                            \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_file(t)) {                                                                                                         \
		QABORT(ABORT_INVALIDARGNOTFILE);                                                                                       \
		return 0;                                                                                                              \
	}

#ifndef ISO_ONLY
#define get_socket(t)                                                                                                          \
	get_next_arg(q, &args);                                                                                                    \
	if (!is_socket(t)) {                                                                                                       \
		QABORT(ABORT_INVALIDARGNOTSOCKET);                                                                                     \
		return 0;                                                                                                              \
	}
#else
#endif

#define VAL_S(n) ((n)->flags & FLAG_SMALL ? (n)->val_ch : (n)->val_s)

#ifndef ISO_ONLY
#define UTF8LEN(n) (is_blob(n) ? (n)->val_len : strlen_utf8(VAL_S(n)))
#define LEN(n) (is_blob(n) ? (n)->val_len : strlen(VAL_S(n)))
#else
#define UTF8LEN(n) strlen_utf8(VAL_S(n))
#define LEN(n) strlen(VAL_S(n))
#endif

#ifndef ISO_ONLY
#define process_yield_unlocked(q) process_yield(q, 0)
#define process_yield_locked(q) process_yield(q, 1)
#endif

#define CA_PEMFILE "ca.pem"
#define KEY_PEMFILE "key.pem"
#define CERT_PEMFILE "cert.pem"

typedef struct {
	char *functor;
	int (*bifptr)(tpl_query *);
	int arity;
} funcs;

#define DEFINE_BIF(f, a, p)                                                                                                    \
	if (g_bifs_idx < MAX_BIFS) {                                                                                               \
		g_bifs[g_bifs_idx].functor = (char *)f;                                                                                \
		g_bifs[g_bifs_idx].bifptr = p;                                                                                         \
		g_bifs[g_bifs_idx].arity = a;                                                                                          \
		g_bifs_idx++;                                                                                                          \
	}

enum { HELLO, BYE, CMD, HTTP2 };

extern funcs g_bifs[];
extern size_t g_bifs_idx;
extern const funcs *get_bifarity(lexer *l, const char *functor, int arity);
extern const funcs *get_bif(lexer *l, const char *functor);
extern void bifs_load_iso(void);
extern char *url_decode(const char *src, char *dstbuf);
extern const char *make_key(trealla *pl, char *dstbuf, node *term);
extern void reset_arg(tpl_query *q, const node *term, unsigned frame);

#ifndef ISO_ONLY
extern int configure_server(tpl_query *q, handler *h, node *term, int (*f)(session *, void *data), int *has_uncle);
extern int process_start_handler(void *data);
extern int process_restart_handler(void *data);
extern int process_yield(tpl_query *q, int locked);
extern node *make_socket(stream *v);
#endif

extern node *make_blob(void *s, size_t len);
extern node *make_int(nbr_t v);
extern node *make_ptr(void *v);
extern node *make_float(flt_t v);
extern node *make_stream(stream *v);
extern node *make_atom(char *s);
extern node *make_const_atom(const char *s);
extern node *make_compound(void);
extern node *make_list(void);
extern node *make_var(tpl_query *q);
extern node *make_tuple(void);
extern node *make_quick_int(nbr_t v);

#if USE_SSL
extern node *make_bignum(const char *s);
#endif

inline static void put_env(tpl_query *q, unsigned point, node *term, signed frame)
{
	env *e = &q->envs[point];
	point -= e->binding;
	e -= e->binding;

	if ((point < q->c.curr_frame) || (point >= (q->c.curr_frame + q->c.frame_size)))
		q->trails[q->c.trail_point + q->c.trail_size++] = point;

	e->binding = frame;
	e->term = term;
	term->refcnt++;
}

inline static void put_ptr(tpl_query *q, unsigned point, void *v)
{
	node *n = make_ptr(v);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static void put_int(tpl_query *q, unsigned point, nbr_t v)
{
	node *n = make_quick_int(v);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static void put_float(tpl_query *q, unsigned point, flt_t v)
{
	node *n = make_float(v);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static void put_atom(tpl_query *q, unsigned point, char *s)
{
	node *n = make_atom(s);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static void put_const_atom(tpl_query *q, unsigned point, const char *s)
{
	node *n = make_const_atom(s);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static env *get_env(tpl_query *q, unsigned point)
{
	env *e = &q->envs[point];
	return e->term ? e : e - e->binding;
}

inline static node *get_arg(tpl_query *q, node *term, unsigned frame)
{
	if (!is_var(term)) {
		q->latest_context = frame;
		return term;
	}

	const env *e = get_env(q, frame + term->slot);

	if (e->term != NULL) {
		q->latest_context = e->context;
		return e->term;
	}

	q->latest_context = frame;
	return term;
}

inline static node *get_next_arg(tpl_query *q, node **term_ptr)
{
	if ((*term_ptr = NLIST_NEXT(*term_ptr)) != NULL)
		return get_arg(q, *term_ptr, q->c.curr_frame);

	return NULL;
}

#ifndef ISO_ONLY
#define PIDLOCK(pl) lock_lock(pl->pid_guard)
#define PIDUNLOCK(pl) lock_unlock(pl->pid_guard)
#define SYSLOCK(pl) lock_lock(pl->pid_guard)
#define SYSUNLOCK(pl) lock_unlock(pl->pid_guard)
#define DBLOCK(db) lock_lock((db)->guard)
#define DBUNLOCK(db) lock_unlock((db)->guard)
#else
#define SYSLOCK(pl)
#define SYSUNLOCK(pl)
#define DBLOCK(db)
#define DBUNLOCK(db)
#endif

#define OP_INFIX(spec) (spec && (!strcmp(spec, "yfx") || !strcmp(spec, "xfy") || !strcmp(spec, "xfx")))
#define OP_PREFIX(spec) (spec && (!strcmp(spec, "fx") || !strcmp(spec, "fy")))
#define OP_POSTFIX(spec) (spec && (!strcmp(spec, "xf") || !strcmp(spec, "yf")))
#define OP_VALID(spec) (OP_INFIX(spec) || OP_PREFIX(spec) || OP_POSTFIX(spec))

#define is_op(db, f) (get_op((db), (f), 0)->fun != NULL)
#define is_infix(db, f) OP_INFIX(get_op((db), (f), 0)->spec)
#define is_prefix(db, f) OP_PREFIX(get_op((db), (f), 0)->spec)
#define is_postfix(db, f) OP_POSTFIX(get_op((db), (f), 0)->spec)

#define clone_term(q, n) copy_term2(q, n, 1, 0) // make exact copy
#define copy_term(q, n) copy_term2(q, n, 0, 0)  // make fresh vars

#define FUNCTOR_LEN 1000               // in UTF-8 characters
#define FUNCTOR_SIZE (FUNCTOR_LEN * 4) // in encoded bytes
#define KEY_SIZE (FUNCTOR_SIZE + 100)
#define PRINTBUF_SIZE (1024 * 64)
#define MAX_UNIFY_DEPTH 1000
#define QABORT(code)                                                                                                           \
	q->halt_code = 1, q->halt = code;                                                                                          \
	q->halt_s = strdup(#code);                                                                                                 \
	q->line_nbr = __LINE__;
#define QABORT2(code, str)                                                                                                     \
	q->halt_code = 1, q->halt = code;                                                                                          \
	q->halt_s = strdup(#code " - " str);                                                                                       \
	q->line_nbr = __LINE__;
#define FUDGE_FACTOR 1
#define PI 3.141592653589793238462643383279502884L

// These are functions to simplify compiler error messages...

inline static node *term_first(node *s) { return NLIST_FRONT(&s->val_l); }
inline static node *term_next(node *n) { return NLIST_NEXT(n); }
inline static node *term_remove(node *s, node *n) { return NLIST_REMOVE(&s->val_l, n); }
inline static void term_insert_before(node *s, node *n1, node *n2) { NLIST_INSERT_BEFORE(&s->val_l, n1, n2); }
inline static void term_insert_after(node *s, node *n1, node *n2) { NLIST_INSERT_AFTER(&s->val_l, n1, n2); }
inline static void term_prepend(node *s, node *n) { NLIST_PUSH_FRONT(&s->val_l, n); }
inline static void term_append(node *s, node *n) { NLIST_PUSH_BACK(&s->val_l, n); }
inline static void term_concat(node *s1, node *s2) { NLIST_CONCAT(&s1->val_l, &s2->val_l); }
inline static int term_count(node *s) { return (int)NLIST_COUNT(&s->val_l); }
inline static int term_arity(node *s) { return (int)(NLIST_COUNT(&s->val_l) > 0 ? (NLIST_COUNT(&s->val_l)) -1 : 0); }

#ifdef DEBUG
inline static const char *term_functor(node *s) { assert(NLIST_FRONT(&s->val_l)); return VAL_S(NLIST_FRONT(&s->val_l)); }
inline static node *term_firstarg(node *s) { assert(term_first(s)); return term_next(term_first(s)); }
#else
inline static const char *term_functor(node *s) { return VAL_S(NLIST_FRONT(&s->val_l)); }
inline static node *term_firstarg(node *s) { return term_next(term_first(s)); }
#endif

inline static void term_heapcheck(node *n)
{
	extern void term_heapclean(node * n);

	if (n == NULL)
		return;

	if (!(n->flags & FLAG_HEAP))
		return;

	if (--n->refcnt != 0)
		return;

	term_heapclean(n);
}

extern void term_destroy(node *n);
extern void trace(tpl_query *q, int fail, int leave);
extern node *copy_term2(tpl_query *q, node *from, int clone, int depth);
extern uint64_t gettimeofday_usec(void);

extern void attach_vars(lexer *l, node *var);
extern int xref_clause(lexer *l, node *r);
extern rule *xref_term(lexer *l, node *term, int arity);
extern int xref_body(lexer *l, node *term, const char *head_functor, int head_arity, int is_last);

extern node *term_make(void);
extern node *make_const_atom(const char *s);
extern node *make_and(void);
extern node *make_true(void);

extern const char *parse_number(const char *s, nbr_t *v, int *numeric);
extern size_t sprint_uint(char *dst, size_t size, unbr_t n, int base);
extern size_t sprint_int(char *dst, size_t size, nbr_t n);
extern const op *get_op_2(module *db, const char *functor, const char *spec);
extern const op *get_op(module *db, const char *functor, int hint_prefix);
extern char *deescape(char *dst, const char *src, char quote);
extern char *dict(module *db, const char *key);
extern void db_init(module *self, trealla *pl, const char *name, const char *filename);
extern int needs_quoting(const char *s);

#ifndef ISO_ONLY
extern void dbs_save_node(module *db, FILE *fp, char **dstbuf, size_t *buflen, node *n, int in_tran);
extern nbr_t dbs_get_fpos(module *db);
extern int dbs_done(module *db);
#endif

#ifndef ISO_ONLY
extern int dir_using(lexer *l, node *n);
extern int dir_use_module(lexer *l, node *n);
extern int dir_unload_file(lexer *l, node *n);
#endif

extern int dir_dynamic(lexer *l, node *n);
extern int dir_include(lexer *l, node *n);
extern int dir_op_3(lexer *l, int pri, const char *spec, const char *name);

extern size_t term_sprint(char *dst, size_t size, trealla *pl, tpl_query *q, node *n, int listing);
extern void term_print(trealla *pl, tpl_query *q, node *n, int listing);
extern size_t term_sprint2(char **dstbuf, size_t *bufsize, char **dst, trealla *pl, tpl_query *q, node *n, int listing);

extern void process_error(tpl_query *q);
extern int process_enqueue(trealla *pl, tpl_query *q, tpl_query *who, node *term, int noerror);

extern int asserta_index(lexer *l, node *n, int manual, int *persist, int in_tran);
extern int assertz_index(lexer *l, node *n, int manual, int *persist, int in_tran);
extern int retract_index(lexer *l, node *n, node *n2, int *persist, int in_tran);
extern tpl_query *query_create_subquery(tpl_query *self);

extern int bif_asserta(tpl_query *q, node *n);
extern int bif_assertz(tpl_query *q, node *n);
extern int bif_retract(tpl_query *q, node *n, node *n2);

#ifndef ISO_ONLY
extern void bifs_load_sys(void);
extern void bifs_load_net(void);
extern void bifs_load_proc(void);
extern void bifs_load_kvs(void);
extern void bifs_load_dbs(void);
extern void bifs_load_http(void);
extern void bifs_load_ws(void);
extern void bifs_load_stomp(void);
#endif

extern int bif_iso_true(tpl_query *q);
extern int bif_iso_reverse(tpl_query *q);
extern int bif_iso_and(tpl_query *q);
extern int bif_iso_or(tpl_query *q);
extern int bif_iso_cut(tpl_query *q);
extern int bif_iso_fail(tpl_query *q);
extern int bif_xtra_cutfail(tpl_query *q);
extern int bif_xtra_enter(tpl_query *q);
extern int bif_iso_retract(tpl_query *q);
extern int bif_iso_asserta(tpl_query *q);
extern int bif_iso_assertz(tpl_query *q);

enum { NUM_NONE, NUM_REAL, NUM_BIGNUM, NUM_INT, NUM_BINARY, NUM_OCTAL, NUM_HEX };

inline static nbr_t get_word(node *n)
{
	if (is_integer(n))
		return n->val_i;
#if USE_SSL
	else if (is_bignum(n))
		return BN_get_word(n->val_bn);
#endif
	else
		return 0;
}

extern node *dbs_read_entry(module *db, nbr_t fpos);

#endif
