#include <stdlib.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <winsock2.h>
#include <io.h>
#define snprintf _snprintf
#define isatty _isatty
#else
#include <unistd.h>
#include <sys/time.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"
#include "history.h"

#ifndef ISO_ONLY
#include "base64.h"
#include "uuid.h"
#include "jsonq.h"
#include "thread.h"
#endif

#ifndef ISO_ONLY
extern int g_dbs_merge;
#endif

int g_trealla_memlimit_mb = 256;
volatile int g_abort = 0;
const char *g_trealla_version = "0.1alpha";
const char *g_list_cons = ".";

#define FUDGE_FACTOR 1

#if (__STDC_VERSION__ >= 201112L) && !defined(ISO_ONLY)
_Atomic
#else
volatile
#endif
uint64_t g_allocs = 0;

uint64_t g_enqueues = 0, g_rescheds = 0, g_busy = 0,
	g_allocates = 0, g_deallocates = 0, g_reallocates = 0,
	g_choicepoints = 0, g_heap_used = 0, g_backtracks = 0,
	g_executes = 0, g_reexecutes = 0, g_cuts = 0, g_match_ok = 0,
	g_match_try = 0, g_u_resolves = 0, g_s_resolves = 0;

static size_t g_instances = 0;
const char *g_escapes = "\a\f\b\t\v\r\n\0";
const char *g_anti_escapes = "afbtvrn0";

static ops g_ops[] =
 {
	{":-", "xfx", 1200},
	{":-", "fx", 1200},
	{"?-", "fx", 1200},
	{";", "xfy", 1100},
	{"|", "xfy", 1100},
	{"->", "xfy", 1050},
	{"*->", "xfy", 1050},
	{",", "xfy", 1000},
#ifndef ISO_ONLY
	//{":=", "xfx", 990},
#endif
#ifndef ISO_ONLY
	{"receive", "fy", 900},
	{"undo", "fy", 900},
	{"enter", "fy", 900},
#endif
	{"\\+", "fy", 900},
	{"is", "xfx", 700},
	{"=", "xfx", 700},
	{"\\=", "xfx", 700},
	{"==", "xfx", 700},
	{"\\==", "xfx", 700},
	{"=:=", "xfx", 700},
	{"=\\=", "xfx", 700},
	{"<", "xfx", 700},
	{"=<", "xfx", 700},
	{">", "xfx", 700},
	{">=", "xfx", 700},
	{"@<", "xfx", 700},
	{"@=<", "xfx", 700},
	{"@>", "xfx", 700},
	{"@>=", "xfx", 700},
	{"=..", "xfx", 700},
	{":", "xfy", 600},
	{"+", "yfx", 500},
	{"-", "yfx", 500},
#ifndef ISO_ONLY
	//{"?", "yfx", 500},
#endif
	{"*", "yfx", 400},
	{"/", "yfx", 400},
	{"//", "yfx", 400},
	{"div", "yfx", 400},
	{"\\", "yfx", 400},
	{"\\/", "yfx", 400},
	{"/\\", "yfx", 400},
	{"xor", "yfx", 400},
	{"rem", "yfx", 400},
	{"mod", "yfx", 400},
	{"<<", "yfx", 400},
	{">>", "yfx", 400},
	{"**", "xfx", 200},
	{"^", "xfx", 200},
	{"--", "fy", 200},
	{"\\", "fy", 200},
#ifndef ISO_ONLY
	//{".", "yfx", 100},
	//{"$", "fx", 1},
#endif

	{0}
 };

const ops *get_op(module *db, const char *functor, int hint_prefix)
{
	const ops *optr;
	int i;

	for (i = 0, optr = db->uops; (i < db->uops_cnt) && optr->op; i++, optr++)
	{
		if (hint_prefix && !OP_PREFIX(optr->spec))
			continue;

		if (!strcmp(optr->op, functor))
			return optr;
	}

	for (optr = g_ops; optr->op; optr++)
	{
		if (hint_prefix && !OP_PREFIX(optr->spec))
			continue;

		if (!strcmp(optr->op, functor))
			return optr;
	}

	return optr;
}

static void stream_close(stream *sp)
{
	if (sp->filename) free(sp->filename);
	if (sp->mode) free(sp->mode);
	if (sp->type) free(sp->type);
	if (sp->fptr) fclose(sp->fptr);
	if (sp->subqptr) query_destroy(sp->subqptr);
	if (sp->subqgoal) term_heapcheck(sp->subqgoal);
#ifndef ISO_ONLY
	if (sp->sptr) session_close((session*)sp->sptr);
#endif
}

void term_destroy(node *n)
{
	while (n)
	{
		node *save = n;
		n = NLIST_NEXT(n);
		term_heapcheck(save);
	}
}

void term_heapcheck(node *n)
{
	if (!(n->flags & FLAG_HEAP))
		return;

	if (--n->refcnt > 0)
		return;

	if (is_compound(n))
	{
		node *n2 = NLIST_FRONT(&n->val_l);

		while (n2)
		{
			node *save = n2;
			n2 = NLIST_NEXT(n2);
			term_heapcheck(save);
		}
	}
	else if (!(n->flags & FLAG_CONST))
	{
		if (is_atom(n))
			free(n->val_s);
		else if (is_var(n))
			free(n->val_s);
		else if (is_stream(n))
		{
			stream_close(n->val_str);
			FREE(n->val_str);
		}
	}

	FREE(n);
}

node *make_ptr(void* v)
{
	node *n = new_node();
	n->flags |= TYPE_INTEGER|FLAG_HEX|FLAG_PTR;
	n->val_ptr = v;
	return n;
}

node *make_int(nbr_t v)
{
	node *n = new_node();
	n->flags |= TYPE_INTEGER;
	n->val_i = v;
	return n;
}

#define S_NUMBERS (1000*1000)

node *make_quick_int(nbr_t v)
{
	static node s_ints[S_NUMBERS+1] = {{{0}}};
	node *n;

	if ((v >= 0) && (v <= S_NUMBERS))
	{
		n = &s_ints[(size_t)v];
		n->flags = TYPE_INTEGER;
		n->val_i = v;
	}
	else
		n = make_int(v);

	return n;
}

node *make_float(flt_t v)
{
	node *n = new_node();
	n->flags |= TYPE_FLOAT;
	n->val_f = v;
	return n;
}

node *make_stream(stream *v)
{
	node *n = new_node();
	n->flags |= TYPE_INTEGER|FLAG_HEX|FLAG_STREAM;
	n->val_str = v;
	return n;
}

node *make_atom(char *s, int quoted)
{
	node *n = new_node();
	n->flags |= TYPE_ATOM;
	if (quoted) n->flags |= FLAG_QUOTED;
	n->val_s = s;
	return n;
}

node *make_const_atom(const char *s, int quoted)
{
	node *n = make_atom((char*)s, quoted);
	n->flags |= FLAG_CONST;
	return n;
}

node *make_structure(void)
{
	node *n = new_node();
	n->flags |= TYPE_COMPOUND;
	n->frame_size = 0;
	n->bifptr = NULL;
	NLIST_INIT(&n->val_l);
	return n;
}

node *make_list(void)
{
	node *n = make_structure();
	n->flags |= FLAG_LIST;
	NLIST_PUSH_BACK(&n->val_l, make_const_atom(g_list_cons, 1));
	return n;
}

node *make_blob(void *s, size_t len)
{
	node *n = new_node();
	n->flags |= TYPE_ATOM|FLAG_QUOTED|FLAG_BLOB;
	n->val_s = (char*)s;
	n->val_len = len;
	return n;
}

#ifndef ISO_ONLY
node *make_socket(stream *v)
{
	node *n = make_stream(v);
	n->flags |= FLAG_SOCKET;
	return n;
}

#endif

node *make_tuple(void)
{
	node *n = make_structure();
	n->flags |= FLAG_TUPLE;
	NLIST_PUSH_BACK(&n->val_l, make_const_atom("{}", 0));
	return n;
}

node *make_var(tpl_query *q)
{
	node *n = new_node();
	n->flags |= TYPE_VAR|FLAG_ANON|FLAG_CONST;
	n->val_s = (char*)"_";
	n->slot = q->frame_size++;
	return n;
}

node *make_true(void)
{
	node *n = make_const_atom("true", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_iso_true;
	return n;
}

node *make_fail(void)
{
	node *n = make_const_atom("fail", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_iso_fail;
	return n;
}

node *make_and(void)
{
	node *n = make_const_atom(",", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_iso_and;
	return n;
}

node *make_cut(void)
{
	node *n = make_const_atom("!", 0);
	n->flags |= FLAG_BUILTIN|FLAG_CUT;
	n->bifptr = bif_iso_cut;
	return n;
}

node *make_cutfail(void)
{
	node *n = make_const_atom("!fail", 0);
	n->flags |= FLAG_BUILTIN|FLAG_CUT;
	n->bifptr = bif_iso_cutfail;
	return n;
}

const char *make_key(trealla *pl, char *dstbuf, node *term)
{
	if (is_atom(term))
		return term->val_s;

	if (is_compound(term))
		snprintf(dstbuf, KEY_SIZE, "%s/%d", NLIST_FRONT(&term->val_l)->val_s, (int)NLIST_COUNT(&term->val_l)-1);
	else
		sprint_term(dstbuf, KEY_SIZE, pl, NULL, term, 0);

	return dstbuf;
}

static char *dict(module *db, const char *key)
{
	char *value = NULL;

	if (sl_get(&db->dict, key, (void**)&value))
		return value;

	value = strdup(key);
	sl_set(&db->dict, value, value);
	return value;
}

#ifndef ISO_ONLY
static int get_ns(lexer *l, const char *name)
{
	int ok = sl_get(&l->ns, name, NULL);
	if (!ok) ok = sl_get(&l->pl->mods, name, NULL);
	return ok;
}

static void add_function(lexer *l, node *n)
{
	node *n2 = NLIST_NEXT(NLIST_FRONT(&n->val_l));
	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", n2->val_s, ARITY_CHAR, (int)(NLIST_NEXT(n2)->val_i));
	sl_set(&l->funs, strdup(tmpbuf), NULL);
}

static int get_function(lexer *l, node *n)
{
	if (!sl_count(&l->funs))
		return 0;

	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", NLIST_FRONT(&n->val_l)->val_s, ARITY_CHAR, (int)(NLIST_COUNT(&n->val_l)-1));
	int ok = sl_get(&l->funs, tmpbuf, NULL);
	return ok;
}

static void add_define(lexer *l, const char *name, const char *value)
{
	void *tmp_value;

	if (sl_get(&l->defines, name, &tmp_value))
		;

	sl_set(&l->defines, strdup(name), strdup(value));
}

static const char *get_define(lexer *l, const char *name)
{
	void *tmp_value = NULL;

	if (sl_get(&l->defines, name, &tmp_value))
		return (char*)tmp_value;

	return NULL;
}
#endif

// Should be called with DBLOCK in effect

static void assert_index(lexer *l, node *n, int manual, int *persist, int append_mode)
{
	module *db = l->db;
	node *tmp = NLIST_FRONT(&n->val_l);
	node *head = tmp = NLIST_NEXT(tmp), *idx = NULL;
	int arity = 0;

	if (is_compound(head))
	{
		arity = NLIST_COUNT(&head->val_l)-1;
		tmp = NLIST_FRONT(&head->val_l);
		idx = NLIST_NEXT(tmp);
	}

	const char *functor = tmp->val_s;
	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
	char *save = NULL;

	if (!(tmp->flags & FLAG_CONST))
		save = tmp->val_s;

	tmp->flags |= FLAG_CONST;
	tmp->val_s = dict(db, tmp->val_s);

	if (save != NULL)
		free(save);

	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void**)&r))
	{
		r = CALLOC(rule);
		r->modname = db->name;
		r->dynamic = r->manual = manual;
		sl_init(&r->idx, 1, &strcmp, NULL);
		sl_set(&db->rules, strdup(tmpbuf), r);
	}

#ifndef ISO_ONLY
	if (r->persist && !db->loading)
	{
		size_t buflen = 1024*64;					// expandable
		char *dstbuf = (char*)malloc(buflen+1);
		dbs_save_node(db, db->fp, &dstbuf, &buflen, n);
		free(dstbuf);
	}
#endif

	*persist = r->persist;

#ifndef ISO_ONLY
	if (r->storage)
	{
		node *head = NLIST_NEXT(NLIST_FRONT(&n->val_l));
		node *tmp_fa = NLIST_NEXT(NLIST_FRONT(&head->val_l));
		node *tmp_rest = NLIST_NEXT(tmp_fa);
		NLIST_REMOVE(&head->val_l, tmp_rest);
		term_heapcheck(tmp_rest);
		node *tmp = make_int(dbs_get_fpos(db));
		tmp->flags |= FLAG_PTR;
		n->flags |= FLAG_DBS_STORAGE;
		NLIST_PUSH_BACK(&head->val_l, tmp);
		//print_term(l->pl, NULL, n, 1); printf("\n");
	}
#endif

	if (append_mode)
		NLIST_PUSH_BACK(&r->clauses, n);
	else
		NLIST_PUSH_FRONT(&r->clauses, n);

	if (idx)
	{
		char tmpbuf[KEY_SIZE];
		const char *key = make_key(l->pl, tmpbuf, idx);

		if (append_mode)
			sl_app(&r->idx, strdup(key), n);
		else
			sl_set(&r->idx, strdup(key), n);
	}
}

void asserta_index(lexer *l, node *n, int manual, int *persist)
{
	assert_index(l, n, manual, persist, 0);
}

void assertz_index(lexer *l, node *n, int manual, int *persist)
{
	assert_index(l, n, manual, persist, 1);
}

// Should be called with DBLOCK in effect

void retract_index(lexer *l, node *n, int *persist)
{
	module *db = l->db;
	node *tmp = NLIST_FRONT(&n->val_l);
	node *head = tmp = NLIST_NEXT(tmp), *idx = NULL;
	int arity = 0;

	if (is_compound(head))
	{
		arity = NLIST_COUNT(&head->val_l)-1;
		tmp = NLIST_FRONT(&head->val_l);
		idx = NLIST_NEXT(tmp);
	}

	const char *functor = tmp->val_s;
	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void**)&r))
	{
		*persist = 0;
		return;
	}

	if (idx)
	{
		char tmpbuf[KEY_SIZE];
		const char *key = make_key(l->pl, tmpbuf, idx);
		sl_del(&r->idx, key, NULL);
	}

	*persist = r->persist;
	NLIST_REMOVE(&r->clauses, n);
}

char *trealla_readline(FILE *fp)
{
	if ((fp == stdin) && isatty(0))
		return history_readline_eol("|: ", '.');

	size_t maxlen = 0, blocksize = 1024;
	char *line = NULL;

	while (!feof(fp))
	{
		maxlen += blocksize;
		char *newline;

		if ((newline = (char*)realloc(line, maxlen+1)) == NULL)
			break;

		line = newline;
		char *block = (line + maxlen) - blocksize;

		LOOP:

		if (fgets(block, blocksize+1, fp) == NULL)
		{
			if (block == line)
			{
				free(line);
				line = NULL;
			}

			break;
		}

		size_t len = strlen(block);

		if ((strchr(block, '\n') != NULL) && isatty(0))
		{
			if ((block[len-1] == '\n') &&
				(block[len-2] == '.'))
				block[--len] = '\0';

			break;
		}

		if ((strchr(block, '\n') != NULL) && !isatty(0))
		{
			while (isspace(block[len-1]))
				block[--len] = '\0';

			if (block[len-1] == '.')
				break;

			block = block + len;
			goto LOOP;
		}

		blocksize *= 2;
	}

	return line;
}

char *trealla_readstring(FILE *fp)
{
	if ((fp == stdin) && isatty(0))
		return history_readline_eol("|: ", '\0');

	size_t maxlen = 0, blocksize = 1024;
	char *line = NULL;

	while (!feof(fp))
	{
		maxlen += blocksize;
		char *newline;

		if ((newline = (char*)realloc(line, maxlen+1)) == NULL)
			break;

		line = newline;
		char *block = (line + maxlen) - blocksize;

		if (fgets(block, blocksize+1, fp) == NULL)
		{
			if (block == line)
			{
				free(line);
				line = NULL;
			}

			break;
		}

		if (strchr(block, '\n') != NULL)	// FIXME
		{
			size_t len = strlen(block);

			if (block[len-1] == '\n')
				block[--len] = '\0';

			break;
		}

		blocksize *= 2;
	}

	return line;
}

uint64_t gettimeofday_usec(void)
#ifdef _WIN32
{
	static const uint64_t epoch = 116444736000000000ULL;
	FILETIME file_time;
	SYSTEMTIME system_time;
	ULARGE_INTEGER u;
	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	u.LowPart = file_time.dwLowDateTime;
	u.HighPart = file_time.dwHighDateTime;
	return (u.QuadPart-epoch)/10 + (1000ULL*system_time.wMilliseconds);
}
#else
{
	struct timeval tp;
	gettimeofday(&tp, NULL);
	return ((uint64_t)tp.tv_sec*1000*1000)+tp.tv_usec;
}
#endif

void attach_vars(lexer *l, node *var)
{
	void *v;

	if (sl_get(&l->symtab, var->val_s, &v))
	{
		var->slot = (uint16_t)(size_t)v;
		return;
	}

	var->slot = l->vars++;
	sl_set(&l->symtab, var->val_s, (void*)(size_t)var->slot);
}

static node *flatten(node *term, node *n)
{
	if (NLIST_COUNT(&term->val_l) != 1)
		return n;

	NLIST_REMOVE(&term->val_l, n);
	NLIST_CONCAT(&term->val_l, &n->val_l);
	term->flags |= FLAG_ATTACHED;

	if (is_builtin(n))
	{
		term->flags |= FLAG_BUILTIN;
		term->bifptr = n->bifptr;
	}

	term_heapcheck(n);
	return NULL;
}

static node *attach_op_infix(lexer *l, node *term, node *n, const char *functor)
{
	node *tmp = make_structure();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n))
	{
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	node *n_prev = NLIST_PREV(n);
	if (!n_prev) return NULL;
	node *n_next = NLIST_NEXT(n);
	if (!n_next) return NULL;
	NLIST_INSERT_BEFORE(&term->val_l, n, tmp);
	NLIST_REMOVE(&term->val_l, n);
	NLIST_REMOVE(&term->val_l, n_prev);
	NLIST_PUSH_BACK(&tmp->val_l, n);
	NLIST_PUSH_BACK(&tmp->val_l, n_prev);

	if (!strcmp(functor, "->"))
	{
		node *n2 = make_cut();
		n2->flags |= FLAG_HIDDEN;
		NLIST_PUSH_BACK(&tmp->val_l, n2);
	}
	else if (!strcmp(functor, ";"))		// Part1
	{
		node *n2 = make_cut();
		n2->flags |= FLAG_HIDDEN|FLAG_NOFOLLOW;
		NLIST_PUSH_BACK(&tmp->val_l, n2);
	}

	const funcs *fptr = get_bif(l, functor);

	if (fptr->arity == -1)
	{
		while (n_next)
		{
			node *save = n_next;
			n_next = NLIST_NEXT(save);
			NLIST_REMOVE(&term->val_l, save);
			NLIST_PUSH_BACK(&tmp->val_l, save);
		}
	}
	else
	{
		NLIST_REMOVE(&term->val_l, n_next);
		NLIST_PUSH_BACK(&tmp->val_l, n_next);
	}

	if (!strcmp(functor, ";"))			// Part2
	{
		node *n2 = make_cut();
		n2->flags |= FLAG_HIDDEN|FLAG_NOFOLLOW;
		NLIST_PUSH_BACK(&tmp->val_l, n2);
	}

	if (strcmp(functor, ":-"))
		tmp = flatten(term, tmp);

	return tmp;
}

static node *attach_op_prefix_n(lexer *l, node *term, node *n)
{
	node *tmp = make_structure();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n))
	{
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	node *n_next = NLIST_NEXT(n);
	if (!n_next) return NULL;
	NLIST_INSERT_BEFORE(&term->val_l, n, tmp);
	NLIST_REMOVE(&term->val_l, n);
	NLIST_PUSH_BACK(&tmp->val_l, n);

	while (n_next)
	{
		node *save = n_next;
		n_next = NLIST_NEXT(save);
		NLIST_REMOVE(&term->val_l, save);
		NLIST_PUSH_BACK(&tmp->val_l, save);
	}

	return tmp;
}

static node *attach_op_prefix(lexer *l, node *term, node *n)
{
	node *n_next = NLIST_NEXT(n);
	if (!n_next) return NULL;

	node *tmp = make_structure();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n))
	{
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	NLIST_INSERT_BEFORE(&term->val_l, n, tmp);
	NLIST_REMOVE(&term->val_l, n);
	NLIST_REMOVE(&term->val_l, n_next);
	NLIST_PUSH_BACK(&tmp->val_l, n);
	NLIST_PUSH_BACK(&tmp->val_l, n_next);
	const char *functor = n->val_s;

	if (!strcmp(functor, "\\+"))
	{
		node *n2 = make_cutfail();
		n2->flags |= FLAG_HIDDEN;

		if (!strcmp(n_next->val_s, ","))
			NLIST_PUSH_BACK(&n_next->val_l, n2);
		else
			NLIST_PUSH_BACK(&tmp->val_l, n2);
	}

	tmp = flatten(term, tmp);
	return tmp;
}

static node *attach_op_postfix(lexer *l, node *term, node *n)
{
	node *tmp = make_structure();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n))
	{
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	node *n_prev = NLIST_PREV(n);
	if (!n_prev) return NULL;
	NLIST_INSERT_BEFORE(&term->val_l, n, tmp);
	NLIST_REMOVE(&term->val_l, n);
	NLIST_REMOVE(&term->val_l, n_prev);
	NLIST_PUSH_BACK(&tmp->val_l, n);
	NLIST_PUSH_BACK(&tmp->val_l, n_prev);
	tmp = flatten(term, tmp);
	return tmp;
}

static int attach_ops(lexer *l, node *term)
{
	if (!is_compound(term) || (term->flags & FLAG_ATTACHED))
		return 0;

	unsigned priority = UINT_MAX;
	int was_operator = 1;
	int xfy = 0;

	for (node *n = NLIST_FRONT(&term->val_l); n; n = NLIST_NEXT(n))
	{
		while (attach_ops(l, n));

		if (!is_atom(n))
		{
			was_operator = 0;
			continue;
		}

		const char *functor = n->val_s;

		if (!strcmp(functor, g_list_cons))
		{
			was_operator = 1;
			continue;
		}

		const ops *optr = get_op(&l->pl->db, functor, !NLIST_PREV(n)?1:0);

		if (!optr->op)
		{
			was_operator = 0;
			continue;
		}

		if (was_operator && !strcmp(functor, "-"))
		{
			node *tmp = NLIST_NEXT(n);

			if (is_number(tmp))
			{
				if (is_float(tmp))
					tmp->val_f = -tmp->val_f;
				else
					tmp->val_i = -tmp->val_i;

				NLIST_REMOVE(&term->val_l, n);
				term_heapcheck(n);
				n = tmp;
				continue;
			}

			free(n->val_s);
			functor = n->val_s = (char*)"--";
			n->flags |= FLAG_CONST;
			n->bifptr = bif_iso_reverse;
		}
		else if (was_operator && !strcmp(functor, "+"))
		{
			node *tmp = NLIST_NEXT(n);
			NLIST_REMOVE(&term->val_l, n);
			term_heapcheck(n);
			n = tmp;
			continue;
		}

		was_operator = 0;

		if (!optr->op)
			continue;

		//term->flags &= ~FLAG_TUPLE;
		was_operator = 1;

		if (optr->priority < priority)
		{
			xfy = !strcmp(optr->spec, "xfy");
			priority = optr->priority;
		}
	}

	int did_something = 0;

	for (node *n = xfy?NLIST_BACK(&term->val_l):NLIST_FRONT(&term->val_l);
			n; n = xfy?NLIST_PREV(n):NLIST_NEXT(n))
	{
		if (!is_atom(n) || (n->flags & FLAG_QUOTED))
			continue;

		const char *functor = n->val_s;
		const ops *optr = get_op(&l->pl->db, functor, !NLIST_PREV(n)?1:0);

		if (!strcmp(functor, "once"))
		{
			node *n2 = make_cut();
			n2->flags |= FLAG_HIDDEN;
			NLIST_PUSH_BACK(&term->val_l, n2);
		}

		if (!optr->op)
			continue;

		if (optr->priority != priority)
			continue;

		//printf("### attach_ops FUNCTOR = '%s' / '%s' arity=%d\n", functor, optr->spec, (int)NLIST_COUNT(&term->val_l));

		if (OP_PREFIX(optr->spec))
		{
			const funcs *fptr = get_bif(l, functor);

			if (fptr->arity == -1)
				n = attach_op_prefix_n(l, term, n);
			else
				n = attach_op_prefix(l, term, n);
		}
		else if (OP_POSTFIX(optr->spec))
			n = attach_op_postfix(l, term, n);
		else if (OP_INFIX(optr->spec))
			n = attach_op_infix(l, term, n, functor);
		else
			return 0;

		did_something = 1;
		if (!n) break;
	}

	if (did_something)
		return 1;

	term->flags |= FLAG_ATTACHED;

	for (node *n = NLIST_FRONT(&term->val_l); n; n = NLIST_NEXT(n))
	{
		while (attach_ops(l, n))
			;
	}

	if (is_builtin(term))
	{
		const char *functor = NLIST_FRONT(&term->val_l)->val_s;
		int arity = NLIST_COUNT(&term->val_l)-1;
		term->bifptr = get_bifarity(l, functor, arity)->bifptr;
	}

	return 0;
}

static void rule_done(void *p)
{
	rule *r = (rule*)p;
	term_destroy(NLIST_FRONT(&r->clauses));
	sl_done(&r->idx, NULL);
	FREE(r);
}

static void db_init(module *self, trealla *pl, const char *name)
{
	self->pl = pl;
	self->name = strdup(name);
	sl_init(&self->dict, 0, &strcmp, &free);
	sl_init(&self->rules, 0, &strcmp, NULL);
	sl_init(&self->exports, 0, &strcmp, &free);
#ifndef ISO_ONLY
	self->guard = lock_create();
#endif
}

static void db_done(module *self)
{
	sl_done(&self->rules, &rule_done);
	sl_done(&self->dict, NULL);
	sl_done(&self->exports, NULL);
#ifndef ISO_ONLY
	node *tmp = NLIST_FRONT(&self->tran_queue);

	if (tmp)
		term_heapcheck(tmp);

	lock_destroy(self->guard);
#endif
	free(self->name);
}

static void db_free(void *p)
{
	if (!p) return;
	db_done((module*)p);
	free(p);
}

static void dir_optimize(lexer *l, node *n)
{
	node *term1 = n;
	if (!is_atom(term1)) return;

	if (!strcmp(term1->val_s, "none"))
		l->pl->noopt = 2;
	else if (!strcmp(term1->val_s, "partial"))
		l->pl->noopt = 1;
	else if (!strcmp(term1->val_s, "full"))
		l->pl->noopt = 0;
}

static void dir_set_prolog_flag(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = NLIST_NEXT(term1);
	if (!term2) return;
	if (!is_atom(term1)) return;
	if (!is_atom(term2)) return;
	const char *flag = n->val_s;
	if (!strcmp(flag, "char_conversion")) l->pl->flag_char_conversion = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "debug")) l->pl->flag_debug = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "unknown")) l->pl->flag_unknown = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "double_quotes")) l->pl->flag_double_quotes = !strcmp(term2->val_s,"true")?1:0;
	else if (!strcmp(flag, "character_escapes")) l->pl->flag_character_escapes = !strcmp(term2->val_s,"true")?1:0;
}

int dir_dynamic(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = NLIST_NEXT(term1);
	if (!is_compound(term1)) return 0;
	node *head = NLIST_NEXT(NLIST_FRONT(&term1->val_l));
	if (!is_integer(NLIST_NEXT(head))) return 0;
	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", head->val_s, ARITY_CHAR, (int)NLIST_NEXT(head)->val_i);
	rule *r = NULL;

	if (!sl_get(&l->db->rules, tmpbuf, (void**)&r))
	{
		r = CALLOC(rule);
		r->dynamic = 1;
		sl_init(&r->idx, 1, &strcmp, NULL);
		sl_set(&l->db->rules, strdup(tmpbuf), r);
	}

	if (!term2)
		return 1;

#ifndef ISO_ONLY
	if (!is_list(term2))
		return 1;

	node *n2 = NLIST_NEXT(NLIST_FRONT(&term2->val_l));
	int i = 1;

	while (n2)
	{
		if (is_atom(n2))
		{
			if (!strcmp(n2->val_s, "unique"))
			{
				//????
			}
			else if (!strcmp(n2->val_s, "persist"))
			{
				r->persist = 1;
			}
			else if (!strcmp(n2->val_s, "storage"))
			{
				r->storage = r->persist = 1;
			}
		}

		n2 = NLIST_NEXT(n2);

		if (!is_list(n2))
			break;

		n2 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
		i++;
	}
#endif

	return 1;
}

static void dir_op(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = NLIST_NEXT(term1);
	if (!term2) return;
	if (!is_integer(term1)) return;
	if (!is_atom(term2)) return;
	if (!OP_VALID(term2->val_s)) return;
	node *term3 = NLIST_NEXT(term2);
	if (!is_atom(term3)) return;

	if (!strcmp(term3->val_s, ",") ||
		!strcmp(term3->val_s, "[]") ||
		!strcmp(term3->val_s, "|"))
		return;

	DBLOCK(l->db);

	if (term1->val_i > 0)
	{
		int idx = l->db->uops_cnt++;
		l->db->uops[idx].priority = term1->val_i;
		l->db->uops[idx].spec = dict(l->db, term2->val_s);
		l->db->uops[idx].op = dict(l->db, term3->val_s);
		DBUNLOCK(l->db);
		return;
	}

	for (int i = 0; i < l->db->uops_cnt; i++)
	{
		if (!strcmp(term3->val_s, l->db->uops[i].op))
			l->db->uops[i] = l->db->uops[--l->db->uops_cnt];
	}

	DBUNLOCK(l->db);
}

static int dir_initialization(lexer *l, node *n)
{
	char tmpbuf[FUNCTOR_SIZE];
	sprint_term(tmpbuf, sizeof(tmpbuf), l->pl, NULL, n, 1);
	l->init = strdup(tmpbuf);
	return 1;
}

#ifndef ISO_ONLY
static int dir_module(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = NLIST_NEXT(term1);
	if (!is_atom(term1)) return 0;
	char *name = term1->val_s;

	DBSLOCK(l->pl);

	if (sl_get(&l->pl->mods, name, NULL))
	{
		DBSUNLOCK(l->pl);
		l->error = 1;
		return 0;
	}

	l->db = (module*)calloc(1, sizeof(module));
	db_init(l->db, l->pl, name);
	sl_set(&l->pl->mods, strdup(name), l->db);
	DBSUNLOCK(l->pl);

	//printf("DEBUG Module '%s'\n", name);
	sl_set(&l->ns, strdup(name), NULL);
	add_define(l, "MODULE", name);

	if (term2 && is_list(term2))
	{
		node *n2 = NLIST_NEXT(NLIST_FRONT(&term2->val_l));

		while (n2)
		{
			if (is_atom(n2))
			{
				if (!strcmp(n2->val_s, "[]"))
					break;
			}
			else if (is_compound(n2))
			{
				node *n3 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
				const char *functor = n3->val_s;
				int arity = (int)NLIST_NEXT(n3)->val_i;
				char tmpbuf[FUNCTOR_SIZE+10];
				snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
				sl_set(&l->db->exports, strdup(tmpbuf), NULL);
				//printf("DEBUG: export %s\n", tmpbuf);
			}

			n2 = NLIST_NEXT(n2);

			if (is_list(n2))
				n2 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
		}
	}

	char filename[FUNCTOR_SIZE];
	snprintf(filename, sizeof(filename), "%s.conf", name);
	struct stat st = {0};
	if (stat(filename, &st) != 0) return 1;
	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) return 1;
	char *dstbuf = (char*)malloc(st.st_size+1);
	size_t len = fread(dstbuf, 1, st.st_size, fp);
	dstbuf[len] = '\0';
	fclose(fp);
	char nambuf[256], tmpbuf[1024*4];
	char *dstbuf2 = (char*)malloc(st.st_size+1);
	char *dst = dstbuf2;
	const char *src = dstbuf;

	while (*src)
	{
		char ch = *src++;

		if ((ch == '\t') || (ch == '\r') || (ch == '\n'))
			continue;

		*dst++ = ch;
	}

	*dst = '\0';
	int i = 0;

	while (jsonqi(dstbuf2, i++, nambuf, sizeof(nambuf), tmpbuf, sizeof(tmpbuf)) != NULL)
	{
		if (!isdigit(tmpbuf[0]))
		{
			char tmpbuf2[sizeof(tmpbuf)*2+20];
			char *dst = tmpbuf2;
			*dst++ = '\'';
			deescape(dst, tmpbuf, '\'');
			dst += strlen(dst);
			*dst++ = '\'';
			*dst = '\0';
			add_define(l, nambuf, tmpbuf2);
		}
		else
			add_define(l, nambuf, tmpbuf);
	}

	free(dstbuf);
	return 1;
}

static int dir_export(lexer *l, node *n)
{
	node *term1 = n;

	if (!is_list(term1))
		return 0;

	node *n2 = NLIST_NEXT(NLIST_FRONT(&term1->val_l));

	while (n2)
	{
		if (is_atom(n2))
		{
			if (!strcmp(n2->val_s, "[]"))
				break;
		}
		else if (is_compound(n2))
		{
			node *n3 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
			const char *functor = n3->val_s;
			int arity = (int)NLIST_NEXT(n3)->val_i;
			char tmpbuf[FUNCTOR_SIZE+10];
			snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
			sl_set(&l->db->exports, strdup(tmpbuf), NULL);
			//printf("DEBUG: export %s\n", tmpbuf);
		}

		n2 = NLIST_NEXT(n2);

		if (is_list(n2))
			n2 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
	}

	return 1;
}

int dir_using(lexer *l, node *n)
{
	node *term1 = n;

	if (is_atom(term1))
	{
		sl_set(&l->ns, strdup(term1->val_s), NULL);
		return 1;
	}

	if (!is_list(term1))
		return 0;

	node *n2 = NLIST_NEXT(NLIST_FRONT(&term1->val_l));

	while (n2)
	{
		if (is_atom(n2))
		{
			if (!strcmp(n2->val_s, "[]"))
				break;

			sl_set(&l->ns, strdup(n2->val_s), NULL);
		}

		n2 = NLIST_NEXT(n2);

		if (is_list(n2))
			n2 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
	}

	return 1;
}

static int dir_define(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = NLIST_NEXT(term1);
	if (!is_atom(term1) && !is_var(term1)) return 0;
	if (!term2) return 0;
	if (!is_atomic(term2)) return 0;
	char tmpbuf[KEY_SIZE];
	sprint_term(tmpbuf, sizeof(tmpbuf), l->pl, NULL, term2, 1);
	add_define(l, term1->val_s, tmpbuf);
	return 1;
}

int dir_use_module(lexer *l, node *n)
{
	node *term1 = n;
	if (!is_atom(term1)) return 0;
	const char *name = term1->val_s;
	//sl_set(&l->ns, strdup(name), NULL);

	DBSLOCK(l->pl);

	if (sl_get(&l->pl->mods, name, NULL))
	{
		DBSUNLOCK(l->pl);
		return 1;
	}

	DBSUNLOCK(l->pl);
	module *save = l->db;
	struct library *lib = libs;

	while (lib->name != NULL)
	{
		if (!strcmp(lib->name, name))
		{
			char *src = strndup((const char*)lib->code, (size_t)lib->len);
			trealla_consult_text(l->pl, src, name);
			free(src);
			l->db = save;
			return 1;
		}

		lib++;
	}

	int ok = trealla_consult_file(l->pl, name);
	l->db = save;
	return ok;
}

int dir_unload_file(lexer *l, node *n)
{
	node *term1 = n;
	if (!is_atom(term1)) return 0;
	return trealla_deconsult(l->pl, term1->val_s);
}

int dir_function(lexer *l, node *n)
{
	node *term1 = n;

	if (is_structure(term1))
	{
		add_function(l, term1);
	}
	else if (is_list(term1))
	{
		node *n2 = NLIST_NEXT(NLIST_FRONT(&term1->val_l));

		while (n2)
		{
			if (is_atom(n2))
			{
				if (!strcmp(n2->val_s, "[]"))
					break;
			}
			else if (is_structure(n2))
			{
				add_function(l, n2);
			}

			n2 = NLIST_NEXT(n2);

			if (is_list(n2))
				n2 = NLIST_NEXT(NLIST_FRONT(&n2->val_l));
		}
	}

	return 1;
}
#endif

int dir_include(lexer *l, node *n)
{
	node *term1 = n;
	if (!is_atom(term1)) return 0;
	return lexer_consult_file(l, term1->val_s);
}

static void directive(lexer *l, node *n)
{
	if (NLIST_COUNT(&n->val_l) < 2)
		return;

	if (!is_compound(n)) return;
	node *head = NLIST_FRONT(&n->val_l);
	if (!is_atom(head)) return;
	const char *functor = head->val_s;
	node *n3 = NLIST_NEXT(head);

	if (!strcmp(functor, "include"))
		dir_include(l, n3);
	else if (!strcmp(functor, "initialization"))
		dir_initialization(l, n3);
	else if (!strcmp(functor, "set_prolog_flag"))
		dir_set_prolog_flag(l, n3);
	else if (!strcmp(functor, "dynamic"))
		dir_dynamic(l, n3);
	else if (!strcmp(functor, "op"))
		dir_op(l, n3);
#ifndef ISO_ONLY
	else if (!strcmp(functor, "module"))
		dir_module(l, n3);
	else if (!strcmp(functor, "using"))
		dir_using(l, n3);
	else if (!strcmp(functor, "export"))
		dir_export(l, n3);
	else if (!strcmp(functor, "define"))
		dir_define(l, n3);
	else if (!strcmp(functor, "function"))
		dir_function(l, n3);
	else if (!strcmp(functor, "ensure_loaded"))
		dir_use_module(l, n3);
	else if (!strcmp(functor, "use_module"))
		dir_use_module(l, n3);
	else if (!strcmp(functor, "unload_file"))
		dir_unload_file(l, n3);
#endif
	else if (!strcmp(functor, "optimize"))
		dir_optimize(l, n3);
}

typedef struct
{
	char *buf, *dst;
	size_t maxlen;
}
 token;

static void token_init(token *t)
{
	t->dst = t->buf = (char*)malloc((t->maxlen=15)+1);
	*t->dst = '\0';
}

static void token_put(token *t, int _ch)
{
	unsigned ch = (unsigned)_ch;
	size_t len = t->dst - t->buf;

	if ((len+4) >= t->maxlen)
	{
		t->buf = (char*)realloc(t->buf, (t->maxlen*=2)+1);
		t->dst = t->buf + len;
	}

	t->dst += put_char_utf8(t->dst, ch);
	*t->dst = '\0';
}

static char *token_take(token *t)
{
	return t->buf;
}

const char *parse_number(char ch, const char *s, nbr_t *value, int *numeric)
{
#ifndef ISO_ONLY
	if ((ch == '0') && (*s == 'b'))
	{
		unbr_t v = 0;
		s++;

		while ((*s == '0') || (*s == '1'))
		{
			v <<= 1;

			if (*s == '1')
				v |= 1;

			s++;
		}

		*numeric = 3;
		*value = v;
		return s;
	}
#endif

	if ((ch == '0') && (*s == 'o'))
	{
		unbr_t v = 0;
		s++;

		while ((*s >= '0') && (*s <= '7'))
		{
			v *= 8;
			v += *s - '0';
			s++;
		}

		*numeric = 4;
		*value = v;
		return s;
	}

	if ((ch == '0') && (*s == 'x'))
	{
		unbr_t v = 0;
		s++;

		while (((*s >= '0') && (*s <= '9')) ||
			((toupper(*s) >= 'A') && (toupper(*s) <= 'F')))
		{
			v *= 16;

			if ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))
				v += 10 + (toupper(*s) - 'A');
			else
				v += *s - '0';

			s++;
		}

		*numeric = 5;
		*value = v;
		return s;
	}

	int neg = 0;

	if (ch == '-')
	{
		neg = 1;
		ch = '0';
	}

	nbr_t v = ch - '0';
	int real = 0, exp = 0;

	while ((ch = *s++) != '\0')
	{
		if (ch == '_')
			;
		else if ((ch == '.') && isdigit(*s))
			real = 1;
		else if (!exp && ((ch == 'e') || (ch == 'E')))
			real = exp = 1;
		else if (exp && ((ch == '-') || (ch == '+')))
			exp = 0;
		else if (!isdigit(ch))
			{ s--; break; }
		else
		{
			v *= 10;
			v += ch - '0';
		}
	}

	*numeric = real ? 1 : 2;
	*value = neg ? -v : v;
	return s;
}

static const char *get_token(lexer *l, const char *s, char **line)
{
	if (!s)
		return NULL;

	while (isspace(*s))
		s++;

	if (*s == '%')
	{
		char ch;

		while ((ch = *s++) != 0)
		{
			if (ch == '\n')
				break;
		}

		return NULL;
	}

	if (!*s || (*s == '\n'))
		return NULL;

	token t;
	token_init(&t);
	l->numeric = l->quoted = 0;
	char quote;
#ifndef ISO_ONLY
	int is_def = 0;
#endif

	while (*s)
	{
		int ch = get_char_utf8(&s);

		if ((t.dst != t.buf) && (ch == '?') && (isupper(*s)))
		{
			s--;
			break;
		}

		if ((ch == '?') && isupper(*s))
		{
			token_put(&t, '?');
#ifndef ISO_ONLY
			is_def = 1;
#endif
			continue;
		}

		if ((ch == '\'') || (ch == '"') || (ch == '`'))
		{
			quote = ch;
			l->quoted = quote == '`' ? 3 : quote == '"' ? 2 : 1;

			while (l->quoted)
			{
				while ((ch = get_char_utf8(&s)) != 0)
				{
					if (ch == quote)
					{
						if (*s == quote)
							s++;
						else
							break;
					}

					if (l->pl->flag_character_escapes &&
						(l->quoted <= 2) && (ch == '\\'))
					{
						if (*s == '\n')
							s++;
						else
						{
							const char *ptr = strchr(g_anti_escapes, ch = *s++);
							if (ptr) token_put(&t, g_escapes[ptr-g_anti_escapes]);
							else token_put(&t, ch);
						}
					}
					else
						token_put(&t, ch);
				}

				if (!ch && l->fp)
				{
					char *newline = trealla_readline(l->fp);

					if (newline)
					{
						free(*line);
						*line = newline;
						s = newline;
						continue;
					}
				}

				break;
			}

			break;
		}

		if (isalpha_utf8(ch) || (ch == '_'))
		{
			token_put(&t, ch);

			while (*s)
			{
				ch = get_char_utf8(&s);

				if (!isalnum_utf8(ch) && (ch != '_'))
				{
#ifndef ISO_ONLY
					if ((ch == ':') && get_ns(l, t.buf))
						;
					else
#endif
					{
						s--;
						break;
					}
				}

				token_put(&t, ch);
			}

			break;
		}

		if (isdigit(ch))
		{
			const char *save_s = s;
			nbr_t v = 0;
			s = parse_number(ch, s, &v, &l->numeric);

			if (l->numeric > 1)
			{
				t.dst = t.buf = (char*)realloc(t.buf, (t.maxlen=255)+1);
				t.dst += sprint_int(t.buf, t.maxlen, v, 10);
				break;
			}
			else
			{
				token_put(&t, ch);
				int exp = 0;

				while ((ch = *save_s++) != 0)
				{
					if (ch == '_')
						;
					else if ((ch == '.') && isdigit(*save_s))
						;
					else if (!exp && ((ch == 'e') || (ch == 'E')))
						exp = 1;
					else if (exp && ((ch == '-') || (ch == '+')))
						exp = 1;
					else if (!isdigit(ch))
						{ save_s--; break; }

					token_put(&t, ch);
				}

				break;
			}
		}

		token_put(&t, ch);

		// These are some hacks...

		if ((ch == '[') && (*s == ']'))
			token_put(&t, ch = *s++);


		if ((ch == '{') && (*s == '}'))
			token_put(&t, ch = *s++);

		if ((ch == '=') && (s[0] == '.') && s[1] == '.')
		{
			token_put(&t, ch = *s++);
			token_put(&t, ch = *s++);
		}

		static const char seps[] = ".!()[]{}_\"'` \t\r\n";

		if (strchr(seps, ch) || strchr(seps, *s) || isalnum_utf8(*s))
			break;
	}

	while (isspace(*s))
		s++;

	if (*s == '%')
	{
		char ch;

		while ((ch = *s++) != 0)
		{
			if (ch == '\n')
				break;
		}
	}

	l->tok = token_take(&t);

#ifndef ISO_ONLY
	if (l->tok && is_def)
	{
		const char *key = l->tok+1;					// skip the '?'

		if (!strcmp(key, "RANDOM"))
		{
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "%d", (int)rand());
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "RANDOMSTR"))
		{
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "'%d'", (int)rand());
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "TIME"))
		{
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "%lld", (long long)time(NULL));
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "TIMESTR"))
		{
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "'%lld'", (long long)time(NULL));
			get_token(l, tmpbuf, line);
			return s;
		}

		const char *value = get_define(l, key);

		if (value != NULL)
		{
			free(l->tok);
			get_token(l, value, line);
			return s;
		}

		printf("Warning: undefined constant '%s'\n", key);
		get_token(l, key, line);
		l->error = 1;
	}
#endif

	//printf("### TOKEN \"%s\" numeric=%d, quoted=%d --> \"%s\"\n", l->tok, l->numeric, l->quoted, s);
	return s;
}

void lexer_init(lexer *self, trealla *pl)
{
	memset(self, 0, sizeof(lexer));
	sl_init(&self->symtab, 0, &strcmp, NULL);
	sl_init(&self->ns, 0, &strcmp, &free);
	sl_init(&self->defines, 0, &strcmp, &free);
	sl_init(&self->funs, 0, &strcmp, &free);
	self->pl = pl;
	self->db = &pl->db;
}

void lexer_done(lexer *self)
{
	sl_done(&self->funs, NULL);
	sl_done(&self->defines, &free);
	sl_done(&self->ns, NULL);
	sl_done(&self->symtab, NULL);
	memset(self, 0, sizeof(lexer));
}

static void lexer_finalize(lexer *self)
{
	if (self->fact)
	{
		node *tmp = new_node();
		tmp->flags |= TYPE_ATOM|FLAG_CONST|FLAG_BUILTIN;
		tmp->val_s = (char*)":-";
		NLIST_PUSH_BACK(&self->r->val_l, tmp);
		tmp = new_node();
		tmp->flags |= TYPE_ATOM|FLAG_CONST|FLAG_BUILTIN;
		tmp->val_s = (char*)"true";
		tmp->bifptr = bif_iso_true;
		NLIST_PUSH_BACK(&self->r->val_l, tmp);
	}

	//{ printf("### parse b4 "); dump_term(NULL, self->r); printf("\n"); }

	while (attach_ops(self, self->r))
		;

	node *r = NLIST_FRONT(&self->r->val_l);
	r->frame_size = self->vars;
	r->flags |= FLAG_RULE;
	if (self->fact) r->flags |= FLAG_FACT;

	//{ printf("### parse a4 "); dump_term(NULL, r); printf("\n"); }

	const char *functor = NLIST_FRONT(&r->val_l)->val_s;
	int arity = NLIST_COUNT(&r->val_l)-1;

	if (self->consult && !self->fact && (arity == 1) && !strcmp(functor, ":-"))
		directive(self, NLIST_NEXT(NLIST_FRONT(&r->val_l)));
	else if (self->consult && !strcmp(functor, "?-"))
		;
	else
	{
		NLIST_REMOVE(&self->r->val_l, r);
		NLIST_PUSH_BACK(&self->clauses, r);
	}

	term_heapcheck(self->r);
	self->r = NULL;
	sl_clear(&self->symtab, NULL);
}

const char *lexer_parse(lexer *self, node *term, const char *src, char **line)
{
	self->depth++;
	int first = 1;

	while ((src = get_token(self, src, line)) != NULL)
	{
		if (!self->r)
		{
			self->r = term = make_structure();
			self->term = NULL;
			self->vars = self->anons = 0;
			self->fact = 1;
		}

		if (!self->quoted && !*self->tok)
		{
			free(self->tok);
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, ".") &&
			(!*src || isspace(*src)))
		{
			free(self->tok);

			if (self->depth > 1)
				continue;

			lexer_finalize(self);

			if (self->error)
				return src;

			continue;
		}

		if (!self->quoted && !strcmp(self->tok, "-") && first)
		{
			free(self->tok);
			self->tok = strdup("--");
		}

		first = 0;

		if (!self->quoted &&
			(!strcmp(self->tok, "]") || !strcmp(self->tok, ")")))
		{
			if (term->flags & FLAG_CONSING)
			{
				node *tmp = new_node();
				tmp->flags |= TYPE_ATOM|FLAG_CONST;
				tmp->val_s = (char*)"[]";
				NLIST_PUSH_BACK(&term->val_l, tmp);
			}

			free(self->tok);

			if (!--self->depth)
			{
				self->error = 1;
				return NULL;
			}

			if (NLIST_COUNT(&term->val_l) == 1)
			{
				node *save = term;
				*term = *NLIST_FRONT(&term->val_l);
				term_heapcheck(save);
			}

			return src;
		}

		if (!self->quoted && !strcmp(self->tok, "}"))
		{
			free(self->tok);

			if (!--self->depth)
			{
				self->error = 1;
				return NULL;
			}

			return src;
		}

		if (!self->quoted && !is_tuple(term) &&
			!(term->flags & FLAG_BARE) && !strcmp(self->tok, ","))
		{
			if (term->flags & FLAG_CONSING)
			{
				free(self->tok);
				node *tmp = make_list();
				tmp->flags |= FLAG_CONSING;
				NLIST_PUSH_BACK(&term->val_l, tmp);
				term = tmp;
				continue;
			}

			free(self->tok);
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, "|"))
		{
			free(self->tok);
			term->flags &= ~FLAG_CONSING;
			continue;
		}

		if (!self->quoted &&
			(!strcmp(self->tok, ":-") || !strcmp(self->tok, "?-")))
			self->fact = 0;

		node *n = new_node();

		if (!self->quoted && !strcmp(self->tok, "{}"))
		{
			free(self->tok);
			n->flags |= TYPE_ATOM|FLAG_CONST;
			n->val_s = (char*)"{}";
		}
		else if (!self->quoted && !strcmp(self->tok, "[]"))
		{
			free(self->tok);
			n->flags |= TYPE_ATOM|FLAG_CONST;
			n->val_s = (char*)"[]";
		}
		else if (!self->quoted && !strcmp(self->tok, "["))
		{
			free(self->tok);
			n->flags |= TYPE_COMPOUND|FLAG_LIST|FLAG_CONSING;
			NLIST_INIT(&n->val_l);
			node *tmp = new_node();
			tmp->flags |= TYPE_ATOM|FLAG_CONST|FLAG_QUOTED;
			tmp->val_s = (char*)g_list_cons;
			NLIST_PUSH_BACK(&n->val_l, tmp);
			src = lexer_parse(self, n, src, line);
			if (self->error) return src;
		}
		else if (!self->quoted && !strcmp(self->tok, "{"))
		{
			free(self->tok);
			n->flags |= TYPE_COMPOUND|FLAG_TUPLE;
			NLIST_INIT(&n->val_l);
			node *tmp = new_node();
			tmp->flags |= TYPE_ATOM|FLAG_CONST;
			tmp->val_s = (char*)"{}";
			NLIST_PUSH_BACK(&n->val_l, tmp);
			src = lexer_parse(self, n, src, line);
			if (self->error) return src;
		}
		else if (!self->quoted && !strcmp(self->tok, "("))
		{
			free(self->tok);
			n->flags |= TYPE_COMPOUND|FLAG_BARE;
			NLIST_INIT(&n->val_l);

			if ((NLIST_COUNT(&term->val_l) != 0) /*&&
				!(term->flags & FLAG_CONSING)*/)
			{
				node *tmp = NLIST_BACK(&term->val_l);

				if (is_atom(tmp))
				{
					const char *functor = tmp->val_s;
					const ops *optr = get_op(&self->pl->db, functor, 0);
					int doit = !strcmp(functor, ",") && !self->quoted && is_tuple(term);

					if ((!optr->op /*&& strcmp(functor, g_list_cons)*/ &&
						strcmp(functor, "!")) || doit)
					{
						NLIST_REMOVE(&term->val_l, tmp);
						NLIST_PUSH_BACK(&n->val_l, tmp);
						n->flags &= ~FLAG_BARE;

						if (!strcmp(functor, "call") ||
							!strcmp(functor, "write_file") ||
							!strcmp(functor, "sys:write_file") ||
							!strcmp(functor, "put_file") ||
							!strcmp(functor, "http:put_file") ||
							!strcmp(functor, "findnsols"))
						{
							node *tmp = new_node();
							tmp->flags |= TYPE_VAR|FLAG_ANON|FLAG_HIDDEN;
							char tmpbuf[40];
							snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->anons++);
							tmp->val_s = strdup(tmpbuf);
							attach_vars(self, tmp);
							NLIST_PUSH_BACK(&n->val_l, tmp);
						}

						if (!strcmp(functor, g_list_cons) && !(tmp->flags&FLAG_QUOTED))
							n->flags |= FLAG_LIST;

						if (is_builtin(tmp))
						{
							n->flags |= FLAG_BUILTIN;
							n->bifptr = tmp->bifptr;
						}
					}
				}
			}

			src = lexer_parse(self, n, src, line);

#ifndef ISO_ONLY
			if (get_function(self, n))
			{
				node *tmp = new_node();
				tmp->flags |= TYPE_VAR|FLAG_ANON|FLAG_HIDDEN;
				char tmpbuf[40];
				snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->anons++);
				tmp->val_s = strdup(tmpbuf);
				attach_vars(self, tmp);
				NLIST_PUSH_BACK(&n->val_l, tmp);
			}
#endif

			if (self->error) return src;
		}
		else if (self->numeric >= 2)
		{
			n->flags |= TYPE_INTEGER;
			if (self->numeric == 3) n->flags |= FLAG_BINARY;
			else if (self->numeric == 4) n->flags |= FLAG_OCTAL;
			else if (self->numeric == 5) n->flags |= FLAG_HEX;
			n->val_i = dec_to_int(self->tok);
			free(self->tok);
		}
		else if (self->numeric == 1)
		{
			n->flags |= TYPE_FLOAT;
			n->val_f = (flt_t)strtod(self->tok, NULL);
			free(self->tok);
		}
		else if (!self->quoted && !strcmp(self->tok, "pi"))
		{
			n->flags |= TYPE_FLOAT|FLAG_PI;
			n->val_f = PI;
			free(self->tok);
		}
		else if (!self->quoted &&
			((self->tok[0] == '_') || isupper(self->tok[0])))
		{
			n->flags |= TYPE_VAR;

			if (!strcmp(self->tok, "_"))
			{
				n->flags |= FLAG_ANON;
				char tmpbuf[40];
				snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->anons++);
				n->val_s = strdup(tmpbuf);
			}
			else
			{
				n->flags |= FLAG_CONST;
				n->val_s = dict(self->db, self->tok);
			}

			free(self->tok);
			attach_vars(self, n);
		}
		else if (!self->quoted && !is_op(self->db, self->tok) &&
			!isalnum_utf8(self->tok[0]) && strcmp(self->tok, "!"))
		{
			printf("ERROR: unknown operator: '%s'\n", self->tok);
			self->error = 1;
		}
		else
		{
			n->flags |= TYPE_ATOM;

			if (self->quoted)
				n->flags |= FLAG_QUOTED;

			if (self->quoted == 2)
				n->flags |= FLAG_DOUBLE_QUOTE;

			if ((n->bifptr = get_bif(self, self->tok)->bifptr) != NULL)
				n->flags |= FLAG_BUILTIN;

			if (self->quoted && !*self->tok)
			{
				free(self->tok);
				n->flags |= FLAG_CONST;
				self->tok = (char*)"";
			}
			else if (!self->quoted)
			{
				char *src = dict(self->db, self->tok);
				n->flags |= FLAG_CONST;
				free(self->tok);
				self->tok = src;
			}
#ifndef ISO_ONLY
			else if (self->quoted == 3)
			{
				n->flags |= FLAG_BLOB;
				char *dstbuf = (char*)malloc(strlen(self->tok)+1);
				size_t len = b64_decode(self->tok, strlen(self->tok), &dstbuf);
				n->val_len = len;
				free(self->tok);
				self->tok = dstbuf;
			}
#endif

			n->val_s = self->tok;
		}

		NLIST_PUSH_BACK(&term->val_l, n);
	}

	self->depth--;

	if (self->depth && self->fp && line)
	{
		free(*line);
		*line = trealla_readline(self->fp);
		if (!*line) return NULL;
		src = lexer_parse(self, term, *line, line);
		if (self->error) return src;
		if (!src) free(*line);
	}
	else if (self->depth != 0)
	{
		printf("ERROR: check parentheses, brackets or braces\n");
		self->error = 1;
	}

	return src;
}

static const char *exts[] = {".prolog",".pro",".pl",".P"};

int lexer_consult_fp(lexer *self, FILE *fp)
{
	node *save_rule = self->r;
	FILE *save_fp = self->fp;
	self->r = NULL;
	self->fp = fp;
	char *line;
	int nbr = 1;

	while ((line = trealla_readline(fp)) != NULL)
	{
		if ((nbr == 1) &&
			(line[0] == '#') &&
			(line[1] == '!'))
			continue;

		const char *src = lexer_parse(self, self->r, line, &line);
		free(line);

		if (self->error)
		{
			printf("ERROR: consult '%s'\n>>> "
				"Syntax error,"
				"line=%d\n>>> %s\n", self->name, nbr, src);
			return 0;
		}

		nbr++;
	}

	self->fp = save_fp;
	self->r = save_rule;
	return 1;
}

int lexer_consult_file(lexer *self, const char *filename)
{
	FILE *fp = fopen(filename, "rb");
	size_t i = 0;

	while (!fp && (i < (sizeof(exts)/sizeof(char*))))
	{
		char tmpbuf[1024];
		strncpy(tmpbuf, filename, sizeof(tmpbuf)-10);
		strcat(tmpbuf, exts[i++]);
		fp = fopen(tmpbuf, "rb");
	}

	if (!fp)
	{
		printf("ERROR: consult '%s': %s\n", filename, strerror(errno));
		return 0;
	}

	self->name = strdup(filename);		// FIXME: memory leak
	lexer_consult_fp(self, fp);
	fclose(fp);
	return 1;
}

static rule *xref_term2(lexer *l, module *db, const char *functor, node *term, int arity)
{
	if (!db)
		return NULL;

	if (!strcmp(functor, "[]") || !strcmp(functor, g_list_cons) || (term->flags&FLAG_QUOTED))
		return NULL;

	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);
	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void**)&r))
		return NULL;

	char *save = NULL;

	if (!(term->flags & FLAG_CONST))
		save = term->val_s;

	term->flags |= FLAG_CONST;
	term->val_s = dict(l->db, term->val_s);

	if (save != NULL)
		free(save);

	return r;
}

rule *xref_term(lexer *l, node *term, int arity)
{
	const char *functor = term->val_s;
	const char *src = strchr(functor, ':');
	rule *r = NULL;

	if (src)
	{
		char tmpbuf2[FUNCTOR_SIZE+10];
		strncpy(tmpbuf2, functor, src-functor);
		tmpbuf2[src-functor] = '\0';
		functor = src+1;
		module *db = NULL;

		if (sl_get(&l->pl->mods, tmpbuf2, (void**)&db))
		{
			if (!db)
			{
				printf("WARN: not exported '%s:%s/%d'\n", tmpbuf2, functor, arity);
				return NULL;
			}

			char tmpbuf[FUNCTOR_SIZE+10];
			snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", functor, ARITY_CHAR, arity);

			if (!sl_get(&db->exports, tmpbuf, NULL))
			{
				printf("WARN: in '%s', not exported '%s:%s'\n", l->db->name, db->name, tmpbuf);
				return NULL;
			}

			if (!sl_get(&db->rules, tmpbuf, (void**)&r))
			{
				r = CALLOC(rule);
				sl_init(&r->idx, 1, &strcmp, NULL);
				sl_set(&db->rules, strdup(tmpbuf), r);
			}

			char *save = NULL;

			if (!(term->flags & FLAG_CONST))
				save = term->val_s;

			term->flags |= FLAG_CONST;
			term->val_s = dict(db, functor);

			if (save != NULL)
				free(save);

			return r;
		}
	}

	r = xref_term2(l, l->db, functor, term, arity);
	if (r != NULL) return r;
	r = xref_term2(l, &l->pl->db, functor, term, arity);
	if (r != NULL) return r;

	sl_start(&l->ns);
	const char *key;

	while ((key = sl_next(&l->ns, NULL)) != NULL)
	{
		module *db = NULL;

		if (!sl_get(&l->pl->mods, key, (void**)&db))
			continue;

		if ((term->bifptr = get_bifarity(l, functor, arity)->bifptr) != NULL)
		{
			term->flags |= FLAG_BUILTIN;
			return r;
		}

		if (db == NULL)
			continue;

		r = xref_term2(l, db, functor, term, arity);

		if (r != NULL)
			break;
	}

	return r;
}

int xref_rule(lexer *l, node *n)
{
	// Cross-reference all body functors with the index, and
	// point to the actual index rule to allow for assert etc.

	node *tmp = NLIST_FRONT(&n->val_l);
	node *head = NLIST_NEXT(tmp);
	if (!head) return 0;
	node *body = NLIST_NEXT(head);
	const char *head_functor = "";

#ifndef ISO_ONLY
	int arity = 0;
#endif

	if (is_compound(head))
	{
		head_functor = NLIST_FRONT(&head->val_l)->val_s;
#ifndef ISO_ONLY
		arity = (int)NLIST_COUNT(&head->val_l)-1;
#endif
	}
	else if (is_atom(head))
		head_functor = head->val_s;

#ifndef ISO_ONLY
	char tmpbuf[FUNCTOR_SIZE+10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s%c%d", head_functor, ARITY_CHAR, arity);

	if (sl_get(&l->db->exports, tmpbuf, NULL))
	{
		node *s = make_structure();
		s->flags |= FLAG_BUILTIN|FLAG_HIDDEN;
		s->bifptr = &bif_dbs_enter;
		node *tmp = make_const_atom("enter", 0);
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = &bif_dbs_enter;
		NLIST_PUSH_BACK(&s->val_l, tmp);
		tmp = make_atom(strdup(l->db->name), 0);
		NLIST_PUSH_BACK(&s->val_l, tmp);
		NLIST_INSERT_AFTER(&n->val_l, head, s);
	}
#endif

	for (n = body; n; n = NLIST_NEXT(n))
	{
		const char *functor = NULL;

		if (is_compound(n))
		{
			node *tmp = NLIST_FRONT(&n->val_l);

			if (is_atom(tmp) && !(tmp->flags & FLAG_BUILTIN))
			{
				n->match = xref_term(l, tmp, NLIST_COUNT(&n->val_l)-1);
				functor = tmp->val_s;
			}
			else
				continue;
		}
		else if (is_atom(n) && !(n->flags & FLAG_BUILTIN))
		{
			n->match = xref_term(l, n, 0);
			functor = n->val_s;
		}
		else
			continue;

		if (!NLIST_NEXT(n) && (n != head) && !l->pl->noopt)
		{
			if (!strcmp(functor, head_functor))
				n->flags |= FLAG_LASTCALL;
		}
	}

	return 1;
}

static void add_clauses(lexer *l, int internal)
{
	list tmp_l;
	list_init(&tmp_l);
	node *n;

	while ((n = NLIST_POP_FRONT(&l->clauses)) != NULL)
	{
		if (!is_rule(n))
			continue;

		int persist;
		assertz_index(l, n, 0, &persist);
		node *tmp_n = CALLOC(node);
		tmp_n->orig = n;
		NLIST_PUSH_BACK(&tmp_l, tmp_n);

		if (internal)
			n->flags |= FLAG_HIDDEN;
	}

	while ((n = NLIST_POP_FRONT(&tmp_l)) != NULL)
	{
		xref_rule(l, n->orig);
		FREE(n);
	}
}

static int trealla_make_rule(trealla *self, const char *src)
{
	lexer l;
	lexer_init(&l, self);
	int ok;

	if (lexer_parse(&l, l.r, src, NULL) != NULL)
	{
		term_heapcheck(NLIST_FRONT(&l.clauses));
		printf("WARN: error make_rule\n");
		ok = 0;
	}
	else
	{
		add_clauses(&l, 1);
		ok = 1;
	}

	lexer_done(&l);
	return ok;
}

int query_parse(tpl_query *self, const char *src)
{
	lexer_init(self->lex, self->pl);

	if (self->lex->clauses.cnt)
	{
		term_heapcheck(NLIST_FRONT(&self->lex->clauses));
		NLIST_INIT(&self->lex->clauses);
	}

	char *line = (char*)malloc(strlen(src)+10);
	sprintf(line, "?- %s", src);
	if (src[strlen(src)-1] != '.') strcat(line, ".");	// FIXME
	lexer_parse(self->lex, self->lex->r, line, NULL);
	free(line);

	if (self->lex->error)
	{
		printf("ERROR: parse -> %s\n", src);
		lexer_done(self->lex);
		return 0;
	}

	if (!xref_rule(self->lex, NLIST_FRONT(&self->lex->clauses)))
	{
		self->halt = 0; self->line_nbr = __LINE__;
		return 0;
	}

	begin_query(self, NLIST_FRONT(&self->lex->clauses));
	return 1;
}

void trace(tpl_query *q, int fail, int leave)
{
	if (!q->curr_term) return;
	if (q->curr_term->flags & FLAG_HIDDEN) return;
	const int save_context = q->latest_context;
	q->latest_context = q->curr_frame;
	size_t dstlen = PRINTBUF_SIZE;
	char *dstbuf = (char*)malloc(dstlen+1);
	char *dst = dstbuf;
	dst += sprintf(dst, "%s", fail?"Fail:":q->retry?"Redo:":leave?"Exit:":"Call:");
	dst += sprintf(dst, "%s", q->parent?" ... ":" ");
	sprint2_term(&dstbuf, &dstlen, &dst, q->pl, q, q->curr_term, 1);

#if 1
	dst = dstbuf+1024;
	*dst++ = '.';
	*dst++ = '.';
	*dst++ = '.';
	*dst = '\0';
#endif

	fprintf(stdout, "%s\n", dstbuf);
	free(dstbuf);
	q->latest_context = save_context;
}

int query_run(tpl_query *self)
{
	self->ok = 1;
	self->started = gettimeofday_usec();
	self->is_running++;
	allocate_frame(self);

	if (!self->parent)
	{
		self->env_point = FUDGE_FACTOR;
		self->envs_used = self->env_point;
	}

	while (!g_abort && !self->pl->abort)
	{
		if (self->trace) trace(self, 0, 0);

		if (!call(self))
		{
			if (self->trace && !self->is_yielded) trace(self, 1, 0);

			if (self->is_yielded || self->halt)
				break;

			if (!retry_me(self))
				break;

			continue;
		}

		if (self->trace) trace(self, 0, 1);				// Exit
		if (!follow(self)) break;
	}

	self->is_running--;
	self->elapsed = gettimeofday_usec() - self->started;

	if (!self->is_yielded && self->halt)
	{
		if (!self->pl->abort && (self->halt > ABORT_HALT))
			printf("WARN: ERROR %s\n", self->halt_s?self->halt_s:"ABORT");
		else if (!self->pl->abort && (self->halt == ABORT_HALT))
			printf("Halted\n");

		self->pl->halt_code = self->halt_code;
		self->ok = 0;
	}

	return self->ok;
}

void query_reset(tpl_query *self)
{
	env *e = &self->envs[FUDGE_FACTOR];

	for (size_t i = 0; i < self->envs_used; i++, e++)
	{
		if (e->term)
			term_heapcheck(e->term);

		e->term = NULL;
		e->binding = 0;
	}

	term_heapcheck(NLIST_FRONT(&self->lex->clauses));
	NLIST_INIT(&self->lex->clauses);
	lexer_done(self->lex);
}

int query_continue(tpl_query *self)
{
	self->ok = 1;
	if (self->halt) return 0;
	if (!retry_me(self)) return 0;
	self->started = gettimeofday_usec();
	self->is_running++;

	while (!g_abort && !self->pl->abort)
	{
		if (self->trace) trace(self, 0, 0);

		if (!call(self))
		{
			if (self->trace && !self->is_yielded) trace(self, 1, 0);

			if (self->is_yielded || self->halt)
				break;

			if (!retry_me(self))
				break;

			continue;
		}

		if (self->trace) trace(self, 0, 1);				// Exit
		if (!follow(self)) break;
	}

	self->is_running--;
	self->elapsed = gettimeofday_usec() - self->started;

	if (!self->is_yielded && self->halt)
	{
		if (!self->pl->abort && (self->halt > ABORT_ABORT))
			printf("ERROR: %s\n", self->halt_s?self->halt_s:"ABORT");

		self->ok = 0;
	}

	return self->ok;
}

int query_inline(tpl_query *self)
{
	self->ok = 1;

	while (!g_abort && !self->pl->abort)
	{
		if (self->trace) trace(self, 0, 0);

		if (!call(self))
		{
			if (self->trace && !self->is_yielded) trace(self, 1, 0);

			if (self->is_yielded || self->halt)
				break;

			if (!retry_me(self))
				break;

			continue;
		}

		if (self->trace) trace(self, 0, 1);				// Exit
		if (!follow(self)) break;
	}

	return self->ok;
}

double query_elapsed(tpl_query *self) { if (self) return (double)self->elapsed / 1000.0 / 1000.0; else return 0.0; }
void query_abort(tpl_query *self) { if (self) { self->halt_s = (char*)strdup("ABORT_INTERRUPTED"); self->halt = 1; }; }

double query_get_float(tpl_query *self, unsigned idx)
{
	env *e = get_env(self, idx);
	if (!e->term) return 0.0;
	return e->term->val_f;
}

long long query_get_integer(tpl_query *self, unsigned idx)
{
	env *e = get_env(self, idx);
	if (!e->term) return 0;
	return e->term->val_i;
}

char *query_get_text(tpl_query *self, unsigned idx)
{
	if (idx >= self->frame_size) return strdup("ERROR");
	env *e = get_env(self, idx);
	if (!e->term) return strdup("_");
	char tmpbuf[PRINTBUF_SIZE];
	sprint_term(tmpbuf, sizeof(tmpbuf), self->pl, self, e->term, 0);
	return strdup(tmpbuf);
}

void query_trace(tpl_query *self)
{
	self->trace = !self->trace;
}

int query_choices(tpl_query *self)
{
	return self->choice_point > 1;
}

static int collect_vars(tpl_query *q, node *n)
{
	int cnt = 0;

	if (is_compound(n))
	{
		for (n = NLIST_FRONT(&n->val_l); n; n = NLIST_NEXT(n))
			cnt += collect_vars(q, n);
	}
	else if (is_var(n))
	{
		env *e = get_env(q, FUDGE_FACTOR+n->slot);

		if (!sl_get(q->d, (char*)e, NULL))
		{
			sl_set(q->d, (char*)e, n);
			cnt++;
		}
	}

	return cnt;
}

void query_dump(tpl_query *self)
{
	skiplist vars;
	sl_init(&vars, 0, NULL, NULL);
	self->d = &vars;
	collect_vars(self, NLIST_FRONT(&self->lex->clauses));
	self->d = NULL;
	int any = 0;
	sl_start(&vars);
	node *n;

	while (sl_next(&vars, (void**)&n) != NULL)
	{
		char tmpbuf[PRINTBUF_SIZE];
		sprint_term(tmpbuf, sizeof(tmpbuf), self->pl, self, n, 1);
		printf(" %s: %s", n->val_s, tmpbuf);
		any++;
	}

	if (any)
		putchar(' ');

	sl_clear(&vars, NULL);
}

static tpl_query *trealla_create_query2(trealla *pl, tpl_query *q);

tpl_query *query_create_subquery(tpl_query *self)
{
	tpl_query *q = trealla_create_query2(self->pl, self);
	if (!q) return NULL;
	q->noopt = self->noopt;
	q->trace = self->trace;
	q->curr_db = self->curr_db;
	env *e_to = q->envs;

	for (size_t i = 0; i < self->frame_size; i++, e_to++)
	{
		env *e_from = get_env(self, self->curr_frame+i);
		node *n = e_from->term;

		if (n == NULL)
			continue;

		e_to->term = n;
		n->refcnt++;
		e_to->binding = -1;
	}

	q->curr_context = q->curr_frame = 0;
	q->frame_size = self->frame_size;
	q->envs_used = q->curr_frame+q->frame_size;
	q->env_point = q->curr_frame+q->frame_size;
	return q;
}

#ifndef ISO_ONLY
tpl_query *query_create_proc(tpl_query *self)
{
	tpl_query *q = query_create_subquery(self);
	if (!q) return NULL;
	q->refcnt++;

	if (self->curr_pid)
	{
		if (!--self->curr_pid->refcnt)
			query_destroy(self->curr_pid);
	}

	self->curr_pid = q;
	return q;
}
#endif

void query_stats(tpl_query *self)
{
	if (!g_enqueues)
	{
		printf("Heap-used: %llu, Backtracks: %llu, Executes: %llu, Reexecutes: %llu\n",
			(long long unsigned)g_heap_used, (long long unsigned)g_backtracks, (long long unsigned)g_executes, (long long unsigned)g_reexecutes);
		printf("Allocates: %llu, Reallocates: %llu, Deallocates: %llu\n",
			(long long unsigned)g_allocates, (long long unsigned)g_reallocates, (long long unsigned)g_deallocates);
		printf("Cuts: %llu, Choicepoints: %llu\n",
			(long long unsigned)g_cuts, (long long unsigned)g_choicepoints);
		printf("Match Try: %llu, ok: %llu (%.0lf%%)\n",
			(long long unsigned)g_match_try, (long long unsigned)g_match_ok, g_match_try?100.0*(double)g_match_ok/g_match_try:0.0);
		printf("Max Env depth: %llu, Choice depth: %llu\n",
			(long long unsigned)self->envs_used, (long long unsigned)self->choices_used);
		printf("System calls: %llu, User: %llu\n",
			(long long unsigned)g_s_resolves, (long long unsigned)g_u_resolves);
	}
#ifndef ISO_ONLY
	else
	{
		printf("System calls: %llu, User: %llu\n",
			(long long unsigned)g_s_resolves, (long long unsigned)g_u_resolves);
		if (g_enqueues) printf("Process msgs: %llu, Busy: %.1f%%, Reschedules: %.1f%%\n",
			(long long unsigned)g_enqueues, (100.0*g_busy)/g_enqueues, (100.0*g_rescheds)/g_enqueues);
	}
#endif
}

#ifndef ISO_ONLY
static void kvs_done(skiplist *d)
{
	sl_start(d);
	node *n = NULL;

	while ((sl_next(d, (void**)&n)) != NULL)
		term_heapcheck(n);

	sl_done(d, NULL);
}
#endif

void query_destroy(tpl_query *self)
{
#ifndef ISO_ONLY
	if (self->is_running)
	{
		printf("*** abort: running\n");
		//abort();
		return;
	}

	if (self->is_dead)
	{
		FREE(self);
		return;
	}

	if (self->kvs)
	{
		kvs_done(self->kvs);
		FREE(self->kvs);
	}

	node *tmp = NLIST_FRONT(&self->queue);

	if (tmp)
		term_heapcheck(tmp);

	if (self->name)
	{
		PIDLOCK(self->pl);
		sl_del(&self->pl->names, (char*)self->name, NULL);
		PIDUNLOCK(self->pl);
	}

	if (self->linked && self->halt)
		process_error(self);

	if (self->curr_pid)
	{
		if (!--self->curr_pid->refcnt)
			query_destroy(self->curr_pid);
	}

	if (self->name)
		free(self->name);
#endif

	if (self->did_getc)
	{
		while (getc(stdin) != '\n')
			;
	}

	env *e = &self->envs[0];

	for (size_t i = 0; i < self->envs_possible; i++, e++)
	{
		if (e->term)
			term_heapcheck(e->term);
	}

	if (!self->parent)
	{
		node *n = NLIST_FRONT(&self->lex->clauses);
		if (n != NULL) term_heapcheck(n);
		NLIST_INIT(&self->lex->clauses);
		lexer_done(self->lex);
	}

	if (self->halt_s)
		free(self->halt_s);

	if (!self->def_choice)
		free(self->choices);

	if (!self->def_env)
		free(self->envs);

#ifndef ISO_ONLY
	if (--self->refcnt > 0)
	{
		self->is_dead = 1;
		return;
	}
#endif

	FREE(self);
}

int trealla_consult_fp(trealla *self, FILE *fp)
{
	if (!fp) return 0;

	lexer l;
	lexer_init(&l, self);
	l.consult = 1;

	if (!lexer_consult_fp(&l, fp))
	{
		if (l.init) free (l.init);
		l.init = NULL;
		lexer_done(&l);
		return 0;
	}

	add_clauses(&l, 0);

	if (l.init)
	{
		if (!trealla_run_query(self, l.init))
			self->abort = 1;

		free(l.init);
	}

	lexer_done(&l);
	return 1;
}

int trealla_consult_file(trealla *self, const char *name)
{
	if (!name) return 0;
	if (!*name) return 0;

	lexer l;
	lexer_init(&l, self);
	l.consult = 1;

	if (!lexer_consult_file(&l, name))
	{
		if (l.init) free (l.init);
		l.init = NULL;
		lexer_done(&l);
		return 0;
	}

	add_clauses(&l, 0);

	if (l.init)
	{
		if (!trealla_run_query(self, l.init))
			self->abort = 1;

		free(l.init);
	}

	lexer_done(&l);
	return 1;
}

int trealla_consult_text(trealla *self, const char *src, const char *name)
{
	if (!src || !name) return 0;
	if (!*src || !*name) return 0;
	if (strlen_utf8(name) >= FUNCTOR_LEN) return 0;
	char *line = strdup(src);
	char *dst = line;

	lexer l;
	lexer_init(&l, self);
	l.consult = 1;
	int nbr = 1;

	while (*src)
	{
		if (*src == '%')
		{
			while (*src != '\n')
				src++;
		}

		if (*src != '\n')
		{
			*dst++ = *src++;
			continue;
		}

		src++;
		*dst = '\0';
		lexer_parse(&l, l.r, line, &line);

		if (l.error)
		{
			printf("ERROR: consult '%s'\n>>> Near line=%d\n>>> %s\n", name, nbr, line);
			return 0;
		}

		dst = line;
		nbr++;
	}

	free(line);
	add_clauses(&l, 0);

	if (l.init)
	{
		if (!trealla_run_query(self, l.init))
			self->abort = 1;

		free(l.init);
	}

	lexer_done(&l);
	return 1;
}

int trealla_deconsult(trealla *self, const char *name)
{
	if (!name) return 0;

	sl_start(&self->db.rules);
	const char *key;
	rule *r;

	while ((key = sl_next(&self->db.rules, (void**)&r)) != NULL)
	{
		if (!r->modname)
			continue;

		if (strcmp(r->modname, name) != 0)
			continue;

		sl_del(&self->db.rules, key, NULL);
		rule_done(r);
	}

	return 1;
}

int trealla_run_query(trealla *self, const char *src)
{
	if (!src) return 0;
	tpl_query *q = trealla_create_query(self);
	if (!q) return 0;
	lexer l;
	q->lex = &l;
	int ok = query_parse(q, src);

	if (!q->lex->error)
		ok = query_run(q);

	query_destroy(q);
	return ok;
}

static tpl_query *trealla_create_query2(trealla *self, tpl_query *parent)
{
	tpl_query *q = CALLOC(tpl_query);
	if (!q) return NULL;
	q->parent = parent;
	q->pl = self;
	q->tmo_msecs = -1;
	q->noopt = self->noopt;
	q->trace = self->trace;
	q->lex = parent ? parent->lex : &self->lex;
	q->curr_db = &self->db;

#ifndef ISO_ONLY
	if (q->parent)
		parent->refcnt++;
	else
		q->name = strdup("default");

	q->refcnt = 1;
	q->curr_pid = parent;
#endif

	q->def_choice = q->def_env = 1;
	q->choices_possible = DEF_CHOICES_BYTES/sizeof(choice);
	q->envs_possible = DEF_ENVS_BYTES/sizeof(env);
	q->choices = q->choice_stack;
	q->envs = q->env_stack;
	return q;
}

tpl_query *trealla_create_query(trealla *self) { return trealla_create_query2(self, NULL); }
void trealla_trace(trealla *self, int mode) { self->trace = mode; }
void trealla_noopt(trealla *self, int mode) { self->noopt = mode; }

#ifndef ISO_ONLY
static int tmocmp(const char *k1, const char *k2)
{
	const tpl_query *q1 = (const tpl_query*)k1;
	const tpl_query *q2 = (const tpl_query*)k2;

	if (q1->tmo_when_msecs < q2->tmo_when_msecs)
		return -1;
	else if (q1->tmo_when_msecs > q2->tmo_when_msecs)
		return 1;
	else
		return 0;
}
#endif

trealla *trealla_create(const char *name)
{
	if (!name) name = "default";
	if (!name[0]||(strlen_utf8(name) >= FUNCTOR_LEN)) name = "default";
	trealla *pl = CALLOC(trealla);
	if (!pl) return NULL;
	g_instances++;
	static int first_time = 1;
	pl->tty = isatty(0);
	db_init(&pl->db, pl, name);
	sl_init(&pl->mods, 0, &strcmp, NULL);

	if (first_time)
	{
		first_time = 0;
		bifs_load_iso();

#ifndef ISO_ONLY
		bifs_load_sys();
		bifs_load_proc();
		bifs_load_net();
		bifs_load_dbs();
		bifs_load_http();
		bifs_load_ws();
		bifs_load_stomp();
		uuid_seed(time(NULL));
#endif
	}

	int idx = 0;

	while (g_bifs[idx].functor != NULL)
	{
		pl->keywords[idx] = g_bifs[idx].functor;
		idx++;
	}

	pl->keywords[idx++] = "maplist";
	pl->keywords[idx++] = "member";
	pl->keywords[idx++] = "select";
	pl->keywords[idx++] = "efface";
	pl->keywords[idx++] = "reverse";
	pl->keywords[idx++] = "append";
	pl->keywords[idx++] = "find";
	history_keywords((const char**)pl->keywords);

#ifndef ISO_ONLY
	sl_init(&pl->idle, 0, &tmocmp, NULL);
	sl_init(&pl->names, 0, &strcmp, &free);
	sl_set(&pl->mods, strdup("sys"), NULL);
	sl_set(&pl->mods, strdup("proc"), NULL);
	sl_set(&pl->mods, strdup("dbs"), NULL);
	sl_set(&pl->mods, strdup("net"), NULL);
	sl_set(&pl->mods, strdup("http"), NULL);
	sl_set(&pl->mods, strdup("ws"), NULL);

	pl->pid_guard = lock_create();
	pl->dbs_guard = lock_create();
#endif

	pl->flag_unknown = 1;
	pl->flag_char_conversion = 1;
	pl->flag_double_quotes = 1;
	pl->flag_character_escapes = 1;

	//trealla_make_rule(pl, "once(G) :- call(G),!.");
	//trealla_make_rule(pl, "A -> B :- call(A),!,call(B).");
	//trealla_make_rule(pl, "\\+ G :- call(G),!,fail.");
	//trealla_make_rule(pl, "\\+ _.");
	//trealla_make_rule(pl, "A ; _B :- call(A), !.");
	//trealla_make_rule(pl, "_A ; B :- call(B), !.");

	trealla_make_rule(pl, "stream_property(S,type(P)) :- stream_property_type(S,P).");
	trealla_make_rule(pl, "stream_property(S,mode(P)) :- stream_property_mode(S,P).");
	trealla_make_rule(pl, "stream_property(S,position(P)) :- stream_property_position(S,P).");
	trealla_make_rule(pl, "stream_property(S,file_name(F)) :- stream_property_file_name(S,F).");

#ifndef ISO_ONLY
	trealla_make_rule(pl, "maplist(P,[]).");
	trealla_make_rule(pl, "maplist(P,[X|Xs]) :- call(P,X),maplist(P,Xs).");
	trealla_make_rule(pl, "maplist(P,[],[]).");
	trealla_make_rule(pl, "maplist(P,[X1|X1s],[X2|X2s]) :- call(P,X1,X2),maplist(P,X1s,X2s).");
	trealla_make_rule(pl, "member(X,[X|_]).");
	trealla_make_rule(pl, "member(X,[_|T]) :- member(X,T).");
	trealla_make_rule(pl, "select(X,[X|T],T).");
	trealla_make_rule(pl, "select(X,[H|T],[H|Rest]) :- select(X,T,Rest).");
	trealla_make_rule(pl, "efface([],L,L) :- !.");
	trealla_make_rule(pl, "efface([H|T],L,L2) :- selectall(H,L,L1),efface(T,L1,L2).");
	trealla_make_rule(pl, "revzap([],L,L).");
	trealla_make_rule(pl, "revzap([X|L],L2,L3) :- revzap(L,[X|L2],L3).");
	trealla_make_rule(pl, "reverse(L1,L2) :- revzap(L1,[],L2).");
	trealla_make_rule(pl, "append([],L,L).");
	trealla_make_rule(pl, "append([User|Rest],L,L2) :- append(Rest,[User|L],L2).");
	trealla_make_rule(pl, "find(N,[],X).");
	trealla_make_rule(pl, "find(1,[H|_],H).");
	trealla_make_rule(pl, "find(N,[H|T],X) :- N1 is N-1, find(N1,T,X).");

#endif

	return pl;
}

void trealla_destroy(trealla *self)
{
	if (!self) return;
	sl_done(&self->mods, &db_free);

#ifndef ISO_ONLY
	sl_done(&self->idle, NULL);
	sl_done(&self->names, NULL);
	lock_destroy(self->pid_guard);
	lock_destroy(self->dbs_guard);

	if (self->tp)
		tpool_destroy(self->tp);

	if (self->h)
	{
		handler_shutdown(self->h);
		handler_destroy(self->h);
	}
#endif

	db_done(&self->db);
	FREE(self);
	g_instances--;

	if (!g_instances && (g_allocs > 0))
		printf("DEBUG: orphaned=%u\n", (unsigned)g_allocs);
}
