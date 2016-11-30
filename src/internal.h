#ifndef INTERNAL_H
#define INTERNAL_H

#include <stdio.h>
#include <stdint.h>

#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
#include <stdatomic.h>
#endif

#include "skipbuck.h"
#include "skiplist.h"
#include "list.h"

#ifndef ISO_ONLY
#include "network.h"
#endif

#if defined(__TINYC__) || defined(_WIN32) || defined(DONT_WANT_128)
typedef int64_t nbr_t;
typedef uint64_t unbr_t;
#else
typedef __int128_t nbr_t;
typedef __uint128_t unbr_t;
#endif

typedef double flt_t;

extern
#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
_Atomic
#else
volatile
#endif
uint64_t g_allocs;

extern uint64_t
	g_allocates, g_deallocates, g_reallocates, g_choicepoints,
	g_heap_used, g_backtracks, g_executes, g_reexecutes, g_match_ok,
	g_match_try, g_cuts, g_enqueues, g_rescheds, g_busy, g_u_resolves,
	g_s_resolves;

typedef uint64_t mask_t;
#define MAX_FRAME_SIZE (sizeof(mask_t)*8)	// 8*8=64
#define MAX_UOPS 100
#define ARITY_CHAR '/'

#define TYPE_INTEGER			(1ULL << 0)
#define TYPE_FLOAT				(1ULL << 1)
#define TYPE_ATOM				(1ULL << 2)
#define TYPE_VAR				(1ULL << 3)
#define TYPE_COMPOUND			(1ULL << 4)

#define FLAG_HEAP				(1ULL << 10)
#define FLAG_ATTACHED			(1ULL << 11)
#define FLAG_LASTCALL			(1ULL << 12)
#define FLAG_CUT				(1ULL << 13)
#define FLAG_FACT				(1ULL << 14)
#define FLAG_BUILTIN			(1ULL << 15)
#define FLAG_CONSING			(1ULL << 16)
#define FLAG_TUPLE				(1ULL << 17)
#define FLAG_QUOTED				(1ULL << 18)
#define FLAG_CONST				(1ULL << 19)
#define FLAG_BINARY				(1ULL << 20)
#define FLAG_OCTAL				(1ULL << 21)
#define FLAG_HEX				(1ULL << 22)
#define FLAG_PI					(1ULL << 23)
#define FLAG_STREAM				(1ULL << 24)
#define FLAG_HIDDEN				(1ULL << 25)
#define FLAG_ANON				(1ULL << 26)
#define FLAG_FILE				(1ULL << 28)
#define FLAG_SOCKET				(1ULL << 29)
#define FLAG_BLOB				(1ULL << 30)
#define FLAG_PID				(1ULL << 31)
#define FLAG_RULE				(1ULL << 32)
#define FLAG_SKIPPED			(1ULL << 33)
#define FLAG_NOFOLLOW			(1ULL << 34)
#define FLAG_DOUBLE_QUOTE		(1ULL << 35)
#define FLAG_DELETED			(1ULL << 36)
#define FLAG_DIRTY				(1ULL << 37)
#define FLAG_BARE				(1ULL << 38)
#define FLAG_LIST				(1ULL << 39)
#define FLAG_PTR				(1ULL << 40)

// DBS flags...

#define FLAG_DBS_STORAGE		(1ULL << 50)
#define FLAG_DBS_ASSERTA		(1ULL << 51)
#define FLAG_DBS_ASSERTZ		(1ULL << 52)
#define FLAG_DBS_RETRACT		(1ULL << 53)

#define NLIST_INIT(l) list_init(l)
#define NLIST_COUNT(l) list_count(l)
#define NLIST_PREV(n) ((node*)(list_prev(&(n)->hdr)-offsetof(node,hdr)))
#define NLIST_NEXT(n) ((node*)(list_next(&(n)->hdr)-offsetof(node,hdr)))
#define NLIST_FRONT(l) ((node*)(list_front(l)-offsetof(node,hdr)))
#define NLIST_BACK(l) (((node*)list_back(l)-offsetof(node,hdr)))
#define NLIST_REMOVE(l,n) list_remove(l,&(n)->hdr)
#define NLIST_PUSH_FRONT(l,n) list_push_front(l,&(n)->hdr)
#define NLIST_PUSH_BACK(l,n) list_push_back(l,&(n)->hdr)
#define NLIST_POP_FRONT(l) ((node*)(list_pop_front(l)-offsetof(node,hdr)))
#define NLIST_POP_BACK(l) ((node*)(list_pop_back(l)-offsetof(node,hdr)))
#define NLIST_INSERT_BEFORE(l,n,v) list_insert_before(l,&(n)->hdr,&(v)->hdr)
#define NLIST_INSERT_AFTER(l,n,v) list_insert_after(l,&(n)->hdr,&(v)->hdr)
#define NLIST_CONCAT(l1,l2) list_concat(l1,l2)
#define NLIST_ITER(l,f,d) list_iter(l,(int (*)(lnode*,void*))f,d)
#define NLIST_CLEAR(l) list_clear(l)

typedef struct node_ node;

typedef struct
{
	const char *modname;
	skiplist idx;
	list clauses;
	uint8_t dynamic, manual, persist, storage;
}
 rule;

typedef struct
{
	char *filename, *mode, *type;
	FILE *fptr;
	tpl_query *subqptr;
	node *subqgoal;

#ifndef ISO_ONLY
	void *sptr;
	tpl_query *qptr;
#endif
}
 stream;

// By making all nodes the same size makes the job of the
// allocator much easier and greatly simplifies the code.

struct node_
{
	lnode hdr;

	union
	{
		struct
		{
			union
			{
				stream *val_str;		// stream ptr
				void *val_ptr;			// other ptr
				char *val_s;			// atom or var
				flt_t val_f;			// float
				nbr_t val_i;			// integer signed
				unbr_t val_u;			// integer unsigned
			};

			uint32_t val_len;			// BLOB length
		};

		list val_l;						// compound
	};

	union
	{
		int (*bifptr)(tpl_query*);		// built-in function
		rule *match;					// index
		node *orig;
#ifndef ISO_ONLY
		char *kvs_key;					// proc kv dict
		tpl_query *pid;					// sender
#endif
	};

	uint64_t flags;

#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
	_Atomic
#endif
	uint32_t refcnt;					// FLAG_HEAP

	union
	{
		uint16_t slot;					// TYPE_VAR
		uint16_t frame_size;			// TYPE_RULE
	};
};

typedef struct
{
	const char *op, *spec;
	unsigned priority;
}
 ops;

typedef struct
{
	trealla *pl;
	char *name;
	skiplist rules, dict, exports;
	ops uops[MAX_UOPS];
	int uops_cnt;
	uint8_t in_tran;

#ifndef ISO_ONLY
	list tran_queue;
	FILE *fp;
	lock *guard;
	uint8_t loading;
	nbr_t last_fpos;
#endif
}
 module;

typedef struct
{
	node *term;								// ref-counted if on heap

	union
	{
		int32_t context;					// >=0 for compounds (-1 = none)
		uint32_t binding;					// <0 offset back to bound Var
	};
}
 env;

typedef struct
{
	slnode *idx_iter;						// First-arg iterator
	module *curr_db;
	node *curr_term, *curr_match;
	mask_t mask, mask2;
	uint32_t env_point, previous, curr_frame;
	uint8_t cut, nofollow, frame_size;
}
 choice;

typedef enum { NUM_NONE=0, NUM_INTEGER, NUM_FLOAT, NUM_PTR } numtype;

typedef struct
{
	union { nbr_t val_i; unbr_t val_u; flt_t val_f; void *val_ptr; };
	numtype type;
}
 number;

typedef struct
{
	list clauses;
	skiplist symtab, ns, defines, funs;
	const char *name;
	node *r, *term;
	FILE *fp;
	trealla *pl;
	module *db;
	char *init, *tok;
	int depth, anons, vars, numeric;
	uint8_t fact, consult;
	uint8_t quoted, error;
}
 lexer;

#define DEF_CHOICES_BYTES 512
#define DEF_ENVS_BYTES 1024

struct tpl_query_
{
	choice choice_stack[DEF_CHOICES_BYTES/sizeof(choice)];
	env env_stack[DEF_ENVS_BYTES/sizeof(env)];
	choice *choices;
	env *envs;
	node *curr_term, *curr_match;
	slnode *idx_iter;						// First-arg iterator
	trealla *pl;
	tpl_query *parent;
	char *halt_s;
	lexer *lex;
	module *curr_db;

	union
	{
		skiplist *d;						// used as a temp
		number nv;
	};

	uint64_t started, elapsed, tmo_when_msecs;
	uint32_t choices_used, choices_possible, choice_point;
	uint32_t envs_used, envs_possible, env_point;
	uint32_t curr_context, latest_context, line_nbr;
	uint32_t curr_frame, curr_choice, print_depth;
	int32_t tmo_msecs;
	uint16_t frame_size, depth, max_depth, fail_arg;
	uint8_t retry, halt, ok, def_choice, def_env;
	uint8_t is_det, timed_out, trace, noopt;
	uint8_t is_running, is_yielded, eval;

#ifndef ISO_ONLY
	list queue;								// process queue
	tpl_query *curr_pid;
	skiplist *kvs;
	char *name;
	uint8_t linked, is_forked, is_proc, is_dead, is_busy, is_idle;

#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
	_Atomic
#endif
	uint32_t refcnt;
#endif
};

#define MAX_BIFS 1000

struct trealla_
{
	skiplist mods;
	module db;
	lexer lex;
	const char *keywords[MAX_BIFS+20];
	volatile int abort, abort_wait;
	uint8_t trace, noopt, tty;
	uint8_t flag_char_conversion, flag_debug, flag_character_escapes;
	uint8_t flag_unknown, flag_double_quotes;

#ifndef ISO_ONLY
	handler *h;
	tpool *tp;
	lock *pid_guard, *dbs_guard;
	skiplist idle, names;
#endif
};

struct library
{
	const char *name;
	const uint8_t *code;
	const uint8_t *len;
};

extern struct library libs[];

#define is_compound(n) (((n)->flags & TYPE_COMPOUND) ? 1 : 0)
#define is_list(n) (((n)->flags & FLAG_LIST) ? 1 : 0)
#define is_structure(n) (is_compound(n) && !is_list(n))
#define is_callable(n) (is_structure(n) || is_atom(n))
#define is_var(n) (((n)->flags & TYPE_VAR) ? 1 : 0)
#define is_float(n) (((n)->flags & TYPE_FLOAT) ? 1 : 0)
#define is_integer(n) (((n)->flags & TYPE_INTEGER) ? 1 : 0)
#define is_number(n) (is_integer(n) || is_float(n))
#define is_atom(n) (((n)->flags & TYPE_ATOM) ? 1 : 0)
#define is_atomic(n) (is_atom(n) || is_number(n))

#define is_anon(n) (((n)->flags & FLAG_ANON) ? 1 : 0)
#define is_tuple(n) (((n)->flags & FLAG_TUPLE) ? 1 : 0)
#define is_builtin(n) (((n)->flags & FLAG_BUILTIN) ? 1 : 0)
#define is_file(n) (((n)->flags & FLAG_FILE) ? 1 : 0)
#define is_stream(n) (((n)->flags & FLAG_STREAM) ? 1 : 0)
#define is_rule(n) (((n)->flags & FLAG_RULE) ? 1 : 0)
#define is_fact(n) (((n)->flags & FLAG_FACT) ? 1 : 0)
#define is_heap(n) (((n)->flags & FLAG_HEAP) ? 1 : 0)
#define is_ptr(n) (((n)->flags & FLAG_PTR) ? 1 : 0)

#ifndef ISO_ONLY
#define is_blob(n) (((n)->flags & FLAG_BLOB) ? 1 : 0)
#define is_pid(n) (((n)->flags & FLAG_PID) ? 1 : 0)
#define is_socket(n) (((n)->flags & FLAG_SOCKET) ? 1 : 0)
#endif

#define FUNCTOR_LEN 1024
#define FUNCTOR_SIZE (FUNCTOR_LEN*4)	// allow for utf8 encoding
#define KEY_SIZE (FUNCTOR_SIZE+100)
#define PRINTBUF_SIZE (1024*64)
#define MAX_UNIFY_DEPTH 999
#define QABORT(code) q->halt = code; q->halt_s = strdup(#code); q->line_nbr = __LINE__;
#define QABORT2(code,str) q->halt = code; q->halt_s = strdup(#code " - " str); q->line_nbr = __LINE__;
#define CALLOC(T) (T*)calloc(1, sizeof(T)); g_allocs++
#define MALLOC(T) (T*)malloc(sizeof(T)); g_allocs++
#define FREE(ptr) free(ptr); g_allocs--
#define PI ((double)3.1415926535897932384)

inline static node *new_node(void)
{
	node* n = MALLOC(node);
	n->flags = FLAG_HEAP;
	n->match = NULL;
	n->refcnt = 1;
	g_heap_used++;
	return n;
}

#define clone_term(q,n) copy_term2(q,n,1,0)		// make exact copy
#define copy_term(q,n) copy_term2(q,n,0,0)		// make fresh vars

#ifndef ISO_ONLY
#define PIDLOCK(pl) lock_lock(pl->pid_guard)
#define PIDUNLOCK(pl) lock_unlock(pl->pid_guard)
#define DBSLOCK(pl) lock_lock(pl->pid_guard)
#define DBSUNLOCK(pl) lock_unlock(pl->pid_guard)
#endif

#ifndef ISO_ONLY
#define DBLOCK(db) lock_lock((db)->guard)
#define DBUNLOCK(db) lock_unlock((db)->guard)
#else
#define DBLOCK(db)
#define DBUNLOCK(db)
#endif

#define OP_INFIX(spec) (spec && (!strcmp(spec,"yfx") || !strcmp(spec,"xfy") || !strcmp(spec,"xfx")))
#define OP_PREFIX(spec) (spec && (!strcmp(spec,"fx") || !strcmp(spec,"fy")))
#define OP_POSTFIX(spec) (spec && (!strcmp(spec,"xf") || !strcmp(spec,"yf")))
#define OP_VALID(spec) (OP_INFIX(spec) || OP_PREFIX(spec) || OP_POSTFIX(spec))

#define is_op(db,f) (get_op((db),(f),0)->op != NULL)
#define is_infix(db,f) OP_INFIX(get_op((db),(f),0)->spec)
#define is_prefix(db,f) OP_PREFIX(get_op((db),(f),0)->spec)
#define is_postfix(db,f) OP_POSTFIX(get_op((db),(f),0)->spec)

inline static void put_env(const tpl_query *q, unsigned point, node *term, int frame)
{
	env *e = &q->envs[point];
	if (e->term) return;				// A safety check, set once
	e -= e->binding;
	e->context = frame?frame:-1;
	e->term = term;
	term->refcnt++;						// Only needed for heap terms
}

inline static env *get_env(const tpl_query *q, unsigned point)
{
	env *e = &q->envs[point];
	return e->term ? e : e-e->binding;
}

extern void trace(tpl_query *q, int fail, int leave);
extern node *copy_term2(tpl_query *q, node *from, int clone, int depth);
extern uint64_t gettimeofday_usec(void);
extern void term_heapcheck(node *n);
extern void term_destroy(node *n);
extern char *trealla_readline(FILE *fp);
extern char *trealla_readstring(FILE *fp);

#ifndef ISO_ONLY
extern void dbs_save_node(module *db, FILE* fp, char **dstbuf, size_t *buflen, node *n);
extern nbr_t dbs_get_fpos(module *db);
#endif

extern int grow_env_stack(tpl_query *q);
extern size_t strlen_utf8(const char *s);
extern void process_error(tpl_query *q);
extern void asserta_index(lexer *l, node *r, int manual, int *persist);
extern void assertz_index(lexer *l, node *r, int manual, int *persist);
extern void retract_index(lexer *l, node *r, int *persist);
extern tpl_query *query_create_subquery(tpl_query *self, int process);
extern void attach_vars(lexer *l, node *var);
extern int xref_rule(lexer *l, node *r);
extern node *make_const_atom(const char *s, int quoted);
extern node *make_and(void);
extern node *make_cut(void);
extern node *make_cutfail(void);
extern node *make_true(void);
extern node *make_fail(void);
extern rule *xref_term(lexer *l, node *term, int arity);
extern const char *parse_number(char ch, const char *s, nbr_t *v, int *numeric);
extern nbr_t dec_to_int(const char *src);
extern size_t sprint_int(char *dst, size_t size, nbr_t n, int base);
extern const ops *get_op(module *db, const char *functor, int hint_prefix);
extern char *deescape(char *dst, const char *src, char quote);

extern size_t sprint_term(char *dst, size_t size, trealla *pl, tpl_query *q, node *n, int listing);
extern void print_term(trealla *pl, tpl_query *q, node *n, int listing);
extern size_t sprint2_term(char **dstbuf, size_t *bufsize, char **dst, trealla *pl, tpl_query *q, node *n, int listing);

extern const char *g_list_cons;
extern void lexer_init(lexer *l, trealla *pl);
extern const char *lexer_parse(lexer *l, node *term, const char *src, char **line);
extern int lexer_consult(lexer *self, const char *filename);
extern void lexer_done(lexer *l);

#endif
