#ifndef INTERNAL_H
#define INTERNAL_H

#include <stdio.h>
#include <stdint.h>

#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
#include <stdatomic.h>
#define atomic _Atomic
#else
#define atomic volatile
#endif

#include "skiplist.h"
#include "skipbuck.h"
#include "list.h"

#ifndef ISO_ONLY
#include "network.h"
#endif

#if defined(__TINYC__) || defined(_WIN32) || !defined(USE_128)
typedef int64_t nbr_t;
typedef uint64_t unbr_t;
#else
typedef __int128_t nbr_t;
typedef __uint128_t unbr_t;
#endif

typedef double flt_t;

#ifdef DEBUG
extern uint64_t g_choicepoints, g_heap_used, g_backtracks, g_executes,
	g_reexecutes, g_cuts, g_enqueues, g_rescheds, g_u_resolves, g_s_resolves;
#endif

extern uint64_t g_busy;

typedef nbr_t mask_t;
#define MAX_FRAME_SIZE (sizeof(mask_t)*8)
#define MAX_UOPS 100

#define TYPE_INTEGER			(1ULL << 0)
#define TYPE_FLOAT				(1ULL << 1)
#define TYPE_ATOM				(1ULL << 2)
#define TYPE_VAR				(1ULL << 3)
#define TYPE_COMPOUND			(1ULL << 4)

#define FLAG_HEAP				(1ULL << 10)
#define FLAG_ATTACHED			(1ULL << 11)
#define FLAG_TAILRECURSIVE		(1ULL << 12)
#define FLAG_ISCUT				(1ULL << 13)
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
#define FLAG_CLAUSE				(1ULL << 32)
#define FLAG_SKIPPED			(1ULL << 33)
#define FLAG_NOFOLLOW			(1ULL << 34)
#define FLAG_DOUBLE_QUOTE		(1ULL << 35)
#define FLAG_DELETED			(1ULL << 36)
#define FLAG_NOARGS				(1ULL << 37)
#define FLAG_LIST				(1ULL << 38)
#define FLAG_PTR				(1ULL << 39)
#define FLAG_PROMOTED			(1ULL << 40)
#define FLAG_DYNAMIC			(1ULL << 41)
#define FLAG_PASSTHRU			(1ULL << 42)
#define FLAG_NOOP				(1ULL << 43)
#define FLAG_SMALL				(1ULL << 44)

#define FLAG_DBS_STORAGE		(1ULL << 50)
#define FLAG_DBS_ASSERTA		(1ULL << 51)
#define FLAG_DBS_ASSERTZ		(1ULL << 52)
#define FLAG_DBS_RETRACT		(1ULL << 53)
#define FLAG_DBS_RETRACTALL		(1ULL << 54)

#define NLIST_INIT(l) list_init(l)
#define NLIST_COUNT(l) list_count(l)
#define NLIST_PREV(n) ((node*)(list_prev(&(n)->hdr)-offsetof(node,hdr)))
#define NLIST_NEXT(n) ((node*)(list_next(&(n)->hdr)-offsetof(node,hdr)))
#define NLIST_FRONT(l) ((node*)(list_front(l)-offsetof(node,hdr)))
#define NLIST_BACK(l) (((node*)list_back(l)-offsetof(node,hdr)))
#define NLIST_REMOVE(l,n) (((node*)list_remove(l,&(n)->hdr)-offsetof(node,hdr)))
#define NLIST_PUSH_FRONT(l,n) list_push_front(l,&(n)->hdr)
#define NLIST_PUSH_BACK(l,n) list_push_back(l,&(n)->hdr)
#define NLIST_POP_FRONT(l) ((node*)(list_pop_front(l)-offsetof(node,hdr)))
#define NLIST_POP_BACK(l) ((node*)(list_pop_back(l)-offsetof(node,hdr)))
#define NLIST_INSERT_BEFORE(l,n,v) list_insert_before(l,&(n)->hdr,&(v)->hdr)
#define NLIST_INSERT_AFTER(l,n,v) list_insert_after(l,&(n)->hdr,&(v)->hdr)
#define NLIST_CONCAT(l1,l2) list_concat(l1,l2)
#define NLIST_CLEAR(l) list_clear(l)

typedef struct node_ node;
typedef struct rule_ rule;
typedef struct stream_ stream;

// By having all nodes the same size makes the job of
// the allocator easier and simplifies the code.

struct node_
{
	lnode hdr;

	union {
		struct {
			char *val_s;			// atom or var
			size_t val_len;			// BLOB length
		};

		struct {
			node *n1, *n2;
		};

		char val_ch[sizeof(list)];

		stream *val_str;			// stream ptr
		void *val_ptr;				// other ptr
		flt_t val_f;				// float
		nbr_t val_i;				// integer signed
		unbr_t val_u;				// integer unsigned
		list val_l;					// compound
	};

	union {
		int (*bifptr)(tpl_query*);	// built-in function
		rule *match;				// or rule index

#ifndef ISO_ONLY
		tpl_query *pid;				// sender
#endif
	};

	uint64_t flags;
	atomic uint32_t refcnt;
	uint8_t slot, frame_size;
	uint16_t cpos;					// Used in parsing
};

typedef struct
{
	const char *fun, *spec;
	unsigned priority;
}
 op;

typedef struct
{
	trealla *pl;
	char *name, *filename, *dbname;
	skiplist rules, dict, exports;
	op uops[MAX_UOPS];
	int uops_cnt, loaded, loading, merging;

#ifndef ISO_ONLY
	list tran_queue;
	FILE *fp;
	lock *guard;
	nbr_t last_fpos;
#endif
}
 module;

struct rule_
{
	module *db;
	skipbuck *idx;
	list val_l;
	const char *functor;

	struct {
		unsigned dynamic:1;
		unsigned manual:1;
		unsigned hidden:1;

#ifndef ISO_ONLY
		unsigned notify:1;
		unsigned numeric:1;
		unsigned persist:1;
		unsigned storage:1;
#endif
	};

#ifndef ISO_ONLY
	skiplist procs;
#endif
};

struct stream_
{
	char *filename, *mode, *type;
	FILE *fptr;
	node *subqgoal;
	tpl_query *subqptr;
	skiplist *kvs;
	void *sptr;
};

struct lexer_
{
	list val_l;
	skiplist symtab, ns;

#ifndef ISO_ONLY
	skiplist defines, funs;
#endif

	char *name;
	node *r, *term, *dcg_last;
	FILE *fp;
	trealla *pl;
	module *db;
	char *init, *tok, quote, last;
	uint32_t cpos;
	int depth, numeric, line_nbr, was_atom, was_paren, is_paren, error, finalized;
	int fact, dcg, dcg_passthru, consult, quoted, was_atomic, neg, vars, was_op, is_op;
};

// If 'term' set then  'context' is an actual env point (or -1).
// If it is NULL then 'binding' is an offset.
// A choice point will increment 'choices' in the first slot.

typedef struct
{
	node *term;

	union {
		uint32_t context;
		int32_t binding;
	};

	uint16_t choices;
}
 env;

typedef struct
{
	sbiter *idx_iter;
	module *curr_db;
	node *curr_term, *curr_match;
	mask_t mask1, mask2;
	uint32_t env_point, trail_point, prev_choice, curr_frame;
	uint8_t frame_size, trail_size;
	uint8_t cut, nofollow;
}
 choice;

typedef uint32_t trail;

#define DEF_TRAILS_BYTES 256
#define DEF_CHOICES_BYTES 256
#define DEF_ENVS_BYTES 512

struct tpl_query_
{
	choice choice_stack[DEF_CHOICES_BYTES/sizeof(choice)];
	env env_stack[DEF_ENVS_BYTES/sizeof(env)];
	trail trail_stack[DEF_TRAILS_BYTES/sizeof(trail)];
	choice *choices;
	env *envs;
	trail *trails;
	node *curr_term, *curr_match;
	trealla *pl;
	tpl_query *parent, *subq;
	lexer *lex;
	module *curr_db;
	char *halt_s;
	node nv;

	union {
		sbiter *idx_iter;					// First-arg iterator
		skiplist *d;						// used as a temp
		uint32_t line_nbr;					// used during parsing
	};

	mask_t mask1, mask2, pins;
	uint64_t started, elapsed, tmo_when_msecs;
	uint32_t choices_used, choices_possible, choice_point;
	uint32_t envs_used, envs_possible, env_point;
	uint32_t trails_used, trails_possible, trail_point;
	uint32_t curr_context, latest_context;
	uint32_t curr_frame, prev_choice, print_depth;
	FILE *curr_stdin, *curr_stdout;
	uint16_t unify_depth;
	uint8_t frame_size, trail_size, fail_arg;
	uint8_t halt_code, halt, is_running;
	uint8_t is_yielded, retry, ok, def_choice, def_env, def_trail;
	uint8_t is_det, timed_out, trace, optimize;
	uint8_t eval, did_getc, in_tran, ignore_ops;

#ifndef ISO_ONLY
	list queue;								// process queue
	tpl_query *curr_pid;
	rule *curr_rule;
	skiplist *kvs;
	char *name;
	int tmo_msecs;
	uint8_t linked, is_forked, is_proc, is_dead, is_busy, is_idle;
	atomic uint32_t refcnt;
#endif
};

#define MAX_BIFS 1000

struct trealla_
{
	skiplist mods;
	module db;
	const char *keywords[MAX_BIFS+20];
	volatile int abort, end_wait, halt_code, halt;
	uint8_t trace, optimize, tty, quiet;
	uint8_t flag_char_conversion, flag_debug, flag_character_escapes;
	uint8_t flag_unknown, flag_double_quotes;

#ifndef ISO_ONLY
	handler *h;
	tpool *tp;
	lock *pid_guard, *dbs_guard;
	skiplist idle, names;
#endif
};

typedef struct
{
	const char *name;
	const uint8_t *start;
	const uint8_t *end;
}
 library;

extern library g_libs[];
extern atomic int64_t g_allocs;

extern void lexer_init(lexer *l, trealla *pl);
extern const char *lexer_parse(lexer *l, node *term, const char *src, char **line);
extern int lexer_consult_file(lexer *self, const char *filename);
extern int lexer_consult_fp(lexer *self, FILE *fp);
extern void lexer_done(lexer *l);

#ifndef ISO_ONLY
extern int http_get10(session *s, const char *path, int keep, int *status);
extern int http_get11(session *s, const char *path, int keep, int *status);
extern tpl_query *query_create_proc(tpl_query *self);
#endif

enum
{
	ABORT_NONE,
	ABORT_ABORT,
	ABORT_HALT,
	ABORT_ARGTOOBIG,
	ABORT_MAXENVS,
	ABORT_MAXCHOICES,
	ABORT_MAXTRAILS,
	ABORT_INVALIDARGS,
	ABORT_OUTOFMEMORY,
	ABORT_TYPEERROR,
	ABORT_MAXDEPTH,
	ABORT_UNDEFINED,
	ABORT_NOTDYNAMIC,
	ABORT_NOTEXISTFUNCTOR,
	ABORT_NOTEXISTPROCESS,
	ABORT_NOTEXISTREMOTE,
	ABORT_NOTEXISTFILE,
	ABORT_NOFILEACCESS,
	ABORT_NOTCHILD,
	ABORT_NOTCALLABLE,
	ABORT_RESULTOVERFLOW,
	ABORT_CANTCONNECT,
	ABORT_SERVERCANTBIND,
	ABORT_SERVERCANTJOIN,
	ABORT_STREAMCLOSED,
	ABORT_PIDCLOSED,
	ABORT_NOCURRENTPID,
	ABORT_DUPLICATENAME,
	ABORT_INVALIDARGMISSING,
	ABORT_INVALIDARGMAXARITY,
	ABORT_INVALIDARGNOTLESSZERO,
	ABORT_INVALIDARGISVAR,
	ABORT_INVALIDARGNOTVAR,
	ABORT_INVALIDARGNOTNBR,
	ABORT_INVALIDARGNOTATOM,
	ABORT_INVALIDARGNOTLIST,
	ABORT_INVALIDARGNOTSTRUCT,
	ABORT_INVALIDARGNOTTUPLE,
	ABORT_INVALIDOPUNKNOWN,
	ABORT_INVALIDOP,
	ABORT_INVALIDARGNOTCOMPOUND,
	ABORT_INVALIDARGNOTNBRORLIST,
	ABORT_INVALIDARGNOTATOMORINT,
	ABORT_INVALIDARGNOTATOMORCOMPOUND,
	ABORT_INVALIDARGNOTATOMORLIST,
	ABORT_INVALIDARGNOTATOMORSTRUCT,
	ABORT_INVALIDARGNOTNBRORSTRUCT,
	ABORT_INVALIDARGNOTATOMIC,
	ABORT_INVALIDARGNOTNBRORVAR,
	ABORT_INVALIDARGNOTINTORVAR,
	ABORT_INVALIDARGNOTLISTORVAR,
	ABORT_INVALIDARGNOTATOMORVAR,
	ABORT_INVALIDARGNOTCALLABLE,
	ABORT_INVALIDARGNOTSTRUCTURE,
	ABORT_INVALIDARGNOTVARORLIST,
	ABORT_INVALIDARGNOTVARORNBR,
	ABORT_INVALIDARGNOTVARORATOM,
	ABORT_INVALIDARGNOTVARORCALLABLE,
	ABORT_INVALIDARGNOTINT,
	ABORT_INVALIDARGNOTPTR,
	ABORT_INVALIDARGNOTFLOAT,
	ABORT_INVALIDARGDIVIDEBYZERO,
	ABORT_INVALIDARGNOTGROUNDED,
	ABORT_INVALIDARGNOTSTREAM,
	ABORT_INVALIDARGNOTFILE,
	ABORT_INVALIDARGNOTSOCKET,
	ABORT_INVALIDARGNOTWEBSOCKET,
	ABORT_INVALIDARGNOTBLOB,
	ABORT_INVALIDARGNOTPID,
	ABORT_END
};

#endif
