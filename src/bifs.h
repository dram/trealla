#ifndef BIFS_H
#define BIFS_H

#include <assert.h>

#include "utf8.h"

#define get_arity(q) (NLIST_COUNT(&q->curr_term->val_l)-1)
#define get_args(q) NLIST_FRONT(&q->curr_term->val_l)

#define get_atomic(t) get_next_arg(q, &args); if (!is_atomic(t)) { QABORT(ABORT_INVALIDARGNOTATOMIC); return 0; }
#define get_atom(t) get_next_arg(q, &args); if (!is_atom(t)) { QABORT(ABORT_INVALIDARGNOTATOM); return 0; }
#define get_atom_or_int(t) get_next_arg(q, &args); if (!is_atom(t)&&!is_integer(t)) { QABORT(ABORT_INVALIDARGNOTATOMORINT); return 0; }
#define get_atom_or_list(t) get_next_arg(q, &args); if (!is_atom(t)&&!is_list(t)) { QABORT(ABORT_INVALIDARGNOTATOMORLIST); return 0; }
#define get_atom_or_list_or_var(t) get_next_arg(q, &args); if (!is_atom(t)&&!is_list(t)&&!is_var(t)) { QABORT(ABORT_INVALIDARGNOTATOMORLIST); return 0; }
#define get_atom_or_var(t) get_next_arg(q, &args); if (!is_atom(t)&&!is_var(t)) { QABORT(ABORT_INVALIDARGNOTATOMORVAR); return 0; }
#define get_int_or_var(t) get_next_arg(q, &args); if (!is_integer(t)&&!is_var(t)) { QABORT(ABORT_INVALIDARGNOTATOMORVAR); return 0; }
#define get_nbr(t) get_next_arg(q, &args); if (!is_number(t)) { QABORT(ABORT_INVALIDARGNOTNBR); return 0; }
#define get_nbr_or_var(t) get_next_arg(q, &args); if (!is_number(t)&&!is_var(t)) { QABORT(ABORT_INVALIDARGNOTNBRORVAR); return 0; }
#define get_next_float(t) get_next_arg(q, &args); if (!is_float(t)) { QABORT(ABORT_INVALIDARGNOTFLOAT); return 0; }
#define get_int(t) get_next_arg(q, &args); if (!is_integer(t)) { QABORT(ABORT_INVALIDARGNOTINT); return 0; }
#define get_ptr(t) get_next_arg(q, &args); if (!is_ptr(t)) { QABORT(ABORT_INVALIDARGNOTPTR); return 0; }
#define get_var(t) get_next_arg(q, &args); if (!is_var(t)) { QABORT(ABORT_INVALIDARGNOTVAR); return 0; }
#define get_nonvar(t) get_next_arg(q, &args); if (is_var(t)) { QABORT(ABORT_INVALIDARGISVAR); return 0; }
#define get_compound(t) get_next_arg(q, &args); if (!is_compound(t)) { QABORT(ABORT_INVALIDARGNOTCOMPOUND); return 0; }
#define get_callable(t) get_next_arg(q, &args); if (!is_callable(t)) { QABORT(ABORT_INVALIDARGNOTCALLABLE); return 0; }
#define get_structure(t) get_next_arg(q, &args); if (!is_structure(t)) { QABORT(ABORT_INVALIDARGNOTSTRUCTURE); return 0; }
#define get_list(t) get_next_arg(q, &args); if (!is_list(t)) { QABORT(ABORT_INVALIDARGNOTLIST); return 0; }
#define get_list_or_var(t) get_next_arg(q, &args); if (!is_list(t)&&!is_var(t)) { QABORT(ABORT_INVALIDARGNOTLISTORVAR); return 0; }
#define get_stream(t) get_next_arg(q, &args); if (!is_stream(t)) { QABORT(ABORT_INVALIDARGNOTSTREAM); return 0; }
#define get_file(t) get_next_arg(q, &args); if (!is_file(t)) { QABORT(ABORT_INVALIDARGNOTFILE); return 0; }
#define get_term(t) get_next_arg(q, &args); if (t == NULL) { QABORT(ABORT_INVALIDARGMISSING); return 0; }

#ifndef ISO_ONLY
#define get_socket(t) get_next_arg(q, &args); if (!is_socket(t)) { QABORT(ABORT_INVALIDARGNOTSOCKET); return 0; }
#define get_file_or_socket(t) get_next_arg(q, &args); if (!is_file(t)&&!is_socket(t)) { QABORT(ABORT_INVALIDARGNOTFILE); return 0; }
#else
#define get_file_or_socket(t) get_next_arg(q, &args); if (!is_file(t)) { QABORT(ABORT_INVALIDARGNOTFILE); return 0; }
#endif

#ifndef ISO_ONLY
#define UTF8LEN(n) (is_blob(n) ? (n)->val_len : strlen_utf8((n)->val_s))
#define LEN(n) (is_blob(n) ? (n)->val_len : strlen((n)->val_s))
#else
#define UTF8LEN(n) strlen_utf8((n)->val_s)
#define LEN(n) strlen((n)->val_s)
#endif

#define process_yield_unlocked(q) process_yield(q, 0)
#define process_yield_locked(q) process_yield(q, 1)

#define CA_PEMFILE "ca.pem"
#define KEY_PEMFILE "key.pem"
#define CERT_PEMFILE "cert.pem"

typedef struct
{
	char *functor;
	int arity;
	int (*bifptr)(tpl_query*);
}
 funcs;

#define DEFINE_BIF(f,a,p) \
	if (g_bifs_idx == MAX_BIFS) abort(); \
	g_bifs[g_bifs_idx].functor = (char*)f; \
	g_bifs[g_bifs_idx].arity = a; \
	g_bifs[g_bifs_idx].bifptr = p; \
	g_bifs_idx++; \
	assert(g_bifs_idx < MAX_BIFS);

enum { HELLO,BYE,CMD,HTTP2 };

extern funcs g_bifs[];
extern size_t g_bifs_idx;
extern const funcs *get_bifarity(lexer *l, const char *functor, int arity);
extern const funcs *get_bif(lexer *l, const char *functor);
extern void bifs_load_iso(void);
extern char *url_decode(const char *src, char *dstbuf);
extern const char *make_key(trealla *pl, char *dstbuf, node *term);
extern void reset_arg(tpl_query *q, const node *term, unsigned frame);

#ifndef ISO_ONLY
extern int http_get10(session *s, const char *path, int keep, int *status);
extern int configure_server(tpl_query *q, handler *h, node *term, int (*f)(session*, void *data), int *has_uncle);
extern int process_start_handler(void *data);
extern int process_restart_handler(void *data);
extern int process_yield(tpl_query *q, int locked);
extern node *make_socket(stream *v);
#endif

extern node *make_blob(void *s, size_t len);
extern node *make_int(nbr_t v);
extern node *make_ptr(void* v);
extern node *make_float(flt_t v);
extern node *make_stream(stream *v);
extern node *make_atom(char *s, int quoted);
extern node *make_const_atom(const char *s, int quoted);
extern node *make_structure(void);
extern node *make_list(void);
extern node *make_var(tpl_query *q);
extern node *make_tuple(void);
extern node *make_quick_int(nbr_t v);

inline static void put_ptr(tpl_query *q, unsigned point, void* v)
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

inline static void put_atom(tpl_query *q, unsigned point, char *s, int quoted)
{
	node *n = make_atom(s, quoted);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static void put_const_atom(tpl_query *q, unsigned point, const char *s, int quoted)
{
	node *n = make_const_atom(s, quoted);
	put_env(q, point, n, -1);
	n->refcnt--;
}

inline static node *get_arg(tpl_query *q, node *term, unsigned frame)
{
	q->latest_context = frame;

	if (!is_var(term))
		return term;

	const env *e = get_env(q, frame+term->slot);

	if (!e->term)
		return term;

	if (e->context != -1)
		q->latest_context = e->context;

	return e->term;
}

inline static node *get_next_arg(tpl_query *q, node **term)
{
	static node dummy = {{0}};

	if ((*term = NLIST_NEXT(*term)) != NULL)
		return get_arg(q, *term, q->curr_frame);
	else
		return &dummy;
}

extern int bif_asserta(tpl_query *q, node *r);
extern int bif_assertz(tpl_query *q, node *r);
extern int bif_retract(tpl_query *q, node *r);

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
extern int bif_iso_cut(tpl_query *q);
extern int bif_iso_cutfail(tpl_query *q);
extern int bif_iso_fail(tpl_query *q);
extern int bif_dbs_enter(tpl_query *q);
extern node *dbs_read_entry(module* db, nbr_t fpos);

#endif

