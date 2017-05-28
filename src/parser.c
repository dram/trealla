#include <ctype.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "trealla.h"

#include "bifs.h"
#include "internal.h"

#ifndef ISO_ONLY
#include "base64.h"
#include "jsonq.h"
#endif

#ifdef DEBUG
extern atomic int64_t g_allocs;
#endif

#if USE_SSL
int g_force_bignum = 0;
#endif

const char *g_escapes = "\a\f\b\t\v\r\n\0";
const char *g_anti_escapes = "afbtvrn0";

static op g_ops[] = {
	{":-", "xfx", 1200},
	{":-", "fx", 1200},
	{"-->", "xfx", 1200},
	{"?-", "fx", 1200},
	{";", "xfy", 1100},
	{"|", "xfy", 1100},
	{"->", "xfy", 1050},
	{"*->", "xfy", 1050},
	{",", "xfy", 1000},

#ifndef ISO_ONLY
	{"receive", "fy", 900},
	{"undo", "fy", 900},
	{"public", "fy", 1050},
	{"export", "fy", 1050},
	{"import", "fy", 1050},
	{"use_module", "fy", 1050},
	{"module", "fy", 1050},
#endif

	{"dynamic", "fy", 1050},
	{"initialization", "fy", 1050},

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
	{"*", "yfx", 400},
	{"/", "yfx", 400},
	{"//", "yfx", 400},
	{"div", "yfx", 400},
	{"\\/", "yfx", 400},
	{"/\\", "yfx", 400},
	{"xor", "yfx", 400},
	{"rem", "yfx", 400},
	{"mod", "yfx", 400},
	{"<<", "yfx", 400},
	{">>", "yfx", 400},
	{"**", "xfx", 200},
	{"^", "xfy", 200},
	{"--", "fy", 200},
	{"\\", "fy", 200},
	{"+", "fy", 200},
	{"-", "fy", 200},

	{0}
};

inline static node *term_last(node *s) { return NLIST_BACK(&s->val_l); }
inline static node *term_prev(node *n) { return NLIST_PREV(n); }

const op *get_op(module *db, const char *functor, int hint_prefix)
{
	const op *optr;
	int i;

	for (i = 0, optr = db->uops; (i < db->uops_cnt) && optr->fun; i++, optr++) {
		if (hint_prefix && !OP_PREFIX(optr->spec))
			continue;

		if (!strcmp(optr->fun, functor))
			return optr;
	}

	for (optr = g_ops; optr->fun; optr++) {
		if (hint_prefix && !OP_PREFIX(optr->spec))
			continue;

		if (!strcmp(optr->fun, functor))
			return optr;
	}

	return optr;
}

int needs_quoting(const char *s)
{
	if (!*s)
		return 1;

	if (isupper(*s) || isdigit(*s) || isspace(*s) || iscntrl(*s))
		return 1;

	int any_signs = 0, any_ans = 0;

	while (*s) {
		unsigned char ch = *s++;

		if (isspace(ch) || iscntrl(ch))
			return 1;

		if (isalpha_utf8(ch) || isdigit(ch))
			any_ans = 1;
		else
			any_signs = 1;

		if (any_ans && any_signs)
			return 1;
	}

	return 0;
}

static nbr_t dec_to_int(const char *src)
{
	nbr_t v = 0;
	int neg = 0;

	if (*src == '-') {
		neg = 1;
		src++;
	}

	while (isdigit(*src)) {
		v *= 10;
		v += *src - '0';
		src++;
	}

	return neg ? -v : v;
}

node *term_make(void)
{
	node *n = calloc(1, sizeof(node));

#ifdef DEBUG
	assert(n);
	g_allocs++;
	g_heap_used++;
#endif

	n->flags = FLAG_HEAP;
	n->refcnt = 1;
	return n;
}

node *make_int(nbr_t v)
{
	node *n = term_make();
	n->flags |= TYPE_INTEGER;
	n->val_i = v;
	return n;
}

node *make_float(flt_t v)
{
	node *n = term_make();
	n->flags |= TYPE_FLOAT;
	n->val_f = v;
	return n;
}

node *make_ptr(void *v)
{
	node *n = term_make();
	n->flags |= TYPE_INTEGER | FLAG_HEX | FLAG_PTR;
	n->val_ptr = v;
	return n;
}

node *make_stream(stream *v)
{
	node *n = term_make();
	n->flags |= TYPE_INTEGER | FLAG_HEX | FLAG_STREAM;
	n->val_str = v;
	return n;
}

static node *make_basic_atom(char *s, int quoted)
{
	node *n = term_make();
	n->flags |= TYPE_ATOM;

	if (quoted)
		n->flags |= FLAG_QUOTED;

	n->val_s = s;
	return n;
}

node *make_const_atom(const char *s, int quoted)
{
	node *n = make_basic_atom((char *)s, quoted);
	n->flags |= FLAG_CONST;
	return n;
}

node *make_atom(char *s, int quoted)
{
	node *n = make_basic_atom((char *)s, quoted);

	if (strlen(s) < sizeof(n->val_ch)) {
		n->flags |= FLAG_CONST | FLAG_SMALL;
		strcpy(n->val_ch, s);
		free(s);
	}

	return n;
}

#if USE_SSL
node *make_bignum(const char *s)
{
	node *n = term_make();
	n->flags |= TYPE_BIGNUM;
	BN_dec2bn(&n->val_bn, s);
	return n;
}
#endif

node *make_compound(void)
{
	node *n = term_make();
	n->flags |= TYPE_COMPOUND;
	return n;
}

node *make_list(void)
{
	node *n = make_compound();
	n->flags |= FLAG_LIST;
	term_append(n, make_const_atom(g_list_cons, 0));
	return n;
}

node *make_blob(void *s, size_t len)
{
	node *n = term_make();
	n->flags |= TYPE_ATOM | FLAG_QUOTED | FLAG_BLOB;
	n->val_s = (char *)s;
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
	node *n = make_compound();
	n->flags |= FLAG_TUPLE;
	term_append(n, make_const_atom("{}", 0));
	return n;
}

node *make_var(tpl_query *q)
{
	node *n = term_make();
	n->flags |= TYPE_VAR | FLAG_ANON | FLAG_CONST;
	n->val_s = (char *)"_";
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

node *make_and(void)
{
	node *n = make_const_atom(",", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_iso_and;
	return n;
}

static node *make_cut(void)
{
	node *n = make_const_atom("!", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_iso_cut;
	return n;
}

static node *make_cutfail(void)
{
	node *n = make_const_atom("!fail", 0);
	n->flags |= FLAG_BUILTIN;
	n->bifptr = bif_xtra_cutfail;
	return n;
}

char *dict(module *db, const char *key)
{
	char *value = NULL;

	if (sl_get(&db->dict, key, (void **)&value))
		return value;

	value = strdup(key);
	sl_set(&db->dict, value, value);
	return value;
}

#ifndef ISO_ONLY
static int get_ns(lexer *l, const char *name)
{
	int ok = sl_get(&l->ns, name, NULL);

	if (!ok) {
		SYSLOCK(l->pl);
		ok = sl_get(&l->pl->mods, name, NULL);
		SYSUNLOCK(l->pl);
	}

	return ok;
}

static void add_function(lexer *l, node *n)
{
	node *n2 = term_firstarg(n);
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", VAL_S(n2), (int)(term_next(n2)->val_i));
	sl_set(&l->funs, strdup(tmpbuf), NULL);
}

static int get_function(lexer *l, node *n)
{
	if (!sl_count(&l->funs))
		return 0;

	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", VAL_S(term_first(n)), term_arity(n));
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
		return (char *)tmp_value;

	return NULL;
}
#endif

static void dir_set_prolog_flag(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = term_next(term1);

	if (!term2)
		return;

	if (!is_atom(term1) || !is_atom(term2))
		return;

	const char *flag = VAL_S(n);

	if (!strcmp(flag, "char_conversion"))
		l->pl->flag_char_conversion = !strcmp(VAL_S(term2), "true") ? 1 : 0;
	else if (!strcmp(flag, "debug"))
		l->pl->flag_debug = !strcmp(VAL_S(term2), "true") ? 1 : 0;
	else if (!strcmp(flag, "unknown"))
		l->pl->flag_unknown = !strcmp(VAL_S(term2), "true") ? 1 : 0;
	else if (!strcmp(flag, "double_quotes"))
		l->pl->flag_double_quotes = !strcmp(VAL_S(term2), "true") ? 1 : 0;
	else if (!strcmp(flag, "character_escapes"))
		l->pl->flag_character_escapes = !strcmp(VAL_S(term2), "true") ? 1 : 0;
}

int dir_dynamic(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = term_next(term1);

	if (!is_compound(term1))
		return 0;

	node *head = term_firstarg(term1);

	if (!is_integer(term_next(head)))
		return 0;

	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", VAL_S(head), (int)term_next(head)->val_i);
	rule *r = NULL;

	if (sl_get(&l->db->rules, tmpbuf, (void **)&r))
		return 0;

	const char *functor = strdup(tmpbuf);
	r = calloc(1, sizeof(rule));
	r->functor = functor;
	r->dynamic = 1;

	if (term2 && is_list(term2)) {
#ifndef ISO_ONLY
		while (is_list(term2)) {
			node *n2 = term_firstarg(term2);

			if (is_atom(n2)) {
				if (!strcmp(VAL_S(n2), "notify")) {
					sl_init(&r->procs, 0, NULL, NULL);
					r->notify = 1;
				}
				else if (!strcmp(VAL_S(n2), "numeric"))
					r->numeric = 1;
				else if (!strcmp(VAL_S(n2), "persist"))
					r->persist = 1;
				else if (!strcmp(VAL_S(n2), "storage"))
					r->storage = r->persist = 1;
			}

			term2 = term_next(n2);
		}
#endif
	}

#ifndef ISO_ONLY
	if (r->numeric)
		r->idx = sb_int_create();
	else
#endif
		r->idx = sb_string_create();

	sl_set(&l->db->rules, functor, r);

	if (term2 && is_structure(term2))
		return dir_dynamic(l, term2);

	return 1;
}

static void dir_op(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = term_next(term1);

	if (!term2)
		return;

	if (!is_integer(term1) || !is_atom(term2))
		return;

	if (!OP_VALID(VAL_S(term2)))
		return;

	node *term3 = term_next(term2);

	if (!term3)
		return;

	if (!is_atom(term3))
		return;

	if (!strcmp(VAL_S(term3), ",") || !strcmp(VAL_S(term3), "[]") || !strcmp(VAL_S(term3), "|"))
		return;

	if (term1->val_i > 0) {
		int idx = l->db->uops_cnt++;
		l->db->uops[idx].priority = term1->val_i;
		l->db->uops[idx].spec = dict(l->db, VAL_S(term2));
		l->db->uops[idx].fun = dict(l->db, VAL_S(term3));
		return;
	}

	for (int i = 0; i < l->db->uops_cnt; i++) {
		if (!strcmp(VAL_S(term3), l->db->uops[i].fun))
			l->db->uops[i] = l->db->uops[--l->db->uops_cnt];
	}
}

static int dir_initialization(lexer *l, node *n)
{
	char tmpbuf[FUNCTOR_SIZE + 10];
	term_sprint(tmpbuf, sizeof(tmpbuf), l->pl, NULL, n, 0);
	strcat(tmpbuf, ".");
	l->init = strdup(tmpbuf);
	l->pl->quiet = 1;
	return 1;
}

#ifndef ISO_ONLY
static int dir_module(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = term_next(term1);

	if (is_compound(term1))
		term1 = term_firstarg(term1);

	if (!is_atom(term1))
		return 0;

	char *name = VAL_S(term1);
	SYSLOCK(l->pl);

	if (sl_get(&l->pl->mods, name, NULL)) {
		printf("ERROR: module '%s': already loaded\n", name);
		SYSUNLOCK(l->pl);
		l->error = 1;
		return 0;
	}

	l->db = (module *)calloc(1, sizeof(module));
	db_init(l->db, l->pl, name, l->name);
	sl_set(&l->pl->mods, strdup(name), l->db);
	SYSUNLOCK(l->pl);
	sl_set(&l->ns, strdup(name), NULL);
	add_define(l, "MODULE", name);

	if (!l->pl->quiet)
		printf("INFO: module: %s\n", name);

	if (term2 && is_list(term2)) {
		while (is_list(term2)) {
			node *n2 = term_firstarg(term2);

			if (is_compound(n2)) {
				node *n3 = term_firstarg(n2);
				const char *functor = VAL_S(n3);
				int arity = (int)term_next(n3)->val_i;
				char tmpbuf[FUNCTOR_SIZE + 10];
				snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
				sl_set(&l->db->exports, strdup(tmpbuf), NULL);
				//printf("DEBUG: EXPORT %s:%s\n", l->db->name, tmpbuf);
			}

			term2 = term_next(n2);
		}
	}

	char filename[FUNCTOR_SIZE + 10];
	snprintf(filename, sizeof(filename), "%s.conf", name);
	FILE *fp = fopen(filename, "rb");

	if (!fp)
		return 1;

	struct stat st = {0};

	if (stat(filename, &st) != 0)
		return 1;

	char *dstbuf = (char *)malloc(st.st_size + 1);
	size_t len = fread(dstbuf, 1, st.st_size, fp);
	dstbuf[len] = '\0';
	fclose(fp);
	char nambuf[256], tmpbuf[1024 * 8];
	char *dstbuf2 = (char *)malloc(st.st_size + 1);
	char *dst = dstbuf2;
	const char *src = dstbuf;

	while (*src) {
		char ch = *src++;

		if ((ch == '\t') || (ch == '\r') || (ch == '\n'))
			continue;

		*dst++ = ch;
	}

	*dst = '\0';
	int i = 0;

	while (jsonqi(dstbuf2, i++, nambuf, sizeof(nambuf), tmpbuf, sizeof(tmpbuf)) != NULL) {
		if (!isdigit(tmpbuf[0])) {
			char tmpbuf2[sizeof(tmpbuf) * 2 + 20];
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

	if (is_list(term1)) {
		while (is_list(term1)) {
			node *n2 = term_firstarg(term1);

			if (is_compound(n2)) {
				node *n3 = term_firstarg(n2);
				const char *functor = VAL_S(n3);
				int arity = (int)term_next(n3)->val_i;
				char tmpbuf[FUNCTOR_SIZE + 10];
				snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
				sl_set(&l->db->exports, strdup(tmpbuf), NULL);
				//printf("DEBUG: EXPORT %s:%s\n", l->db->name, tmpbuf);
			}

			term1 = term_next(n2);
		}

		return 1;
	}

	while (is_structure(term1)) {
		const char *functor = term_functor(term1);

		if (!strcmp(functor, ",")) {
			node *n2 = term_firstarg(term1);

			if (is_compound(n2)) {
				node *n3 = term_firstarg(n2);
				const char *functor = VAL_S(n3);
				int arity = (int)term_next(n3)->val_i;
				char tmpbuf[FUNCTOR_SIZE + 10];
				snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
				sl_set(&l->db->exports, strdup(tmpbuf), NULL);
				//printf("DEBUG: EXPORT %s:%s\n", l->db->name, tmpbuf);
			}

			term1 = term_next(n2);
			continue;
		}

		if (is_compound(term1)) {
			node *n3 = term_firstarg(term1);
			const char *functor = VAL_S(n3);
			int arity = (int)term_next(n3)->val_i;
			char tmpbuf[FUNCTOR_SIZE + 10];
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
			sl_set(&l->db->exports, strdup(tmpbuf), NULL);
			//printf("DEBUG: EXPORT %s:%s\n", l->db->name, tmpbuf);
		}

		term1 = term_next(term1);

		if (!term1)
			break;
	}

	return 1;
}

int dir_using(lexer *l, node *n)
{
	node *term1 = n;

	if (is_atom(term1)) {
		sl_set(&l->ns, strdup(VAL_S(term1)), NULL);
		return 1;
	}

	while (is_list(term1)) {
		node *n2 = term_firstarg(term1);

		if (is_atom(n2))
			sl_set(&l->ns, strdup(VAL_S(n2)), NULL);

		term1 = term_next(n2);
	}

	return 1;
}

static int dir_define(lexer *l, node *n)
{
	node *term1 = n;
	node *term2 = term_next(term1);

	if (!is_atom(term1) && !is_var(term1))
		return 0;

	if (!term2)
		return 0;

	if (!is_atomic(term2))
		return 0;

	char tmpbuf[KEY_SIZE];
	term_sprint(tmpbuf, sizeof(tmpbuf), l->pl, NULL, term2, 1);
	add_define(l, VAL_S(term1), tmpbuf);
	return 1;
}

int dir_use_module(lexer *l, node *n)
{
	node *term = n;
	int use_lib = 0;

	if (is_atom(term))
		;
	else if (is_compound(term)) {
		if (!strcmp(term_functor(term), "library")) {
			use_lib = 1;
			term = term_firstarg(term);
		}
	}
	else
		return 0;

	const char *name = VAL_S(term);
	// sl_set(&l->ns, strdup(name), NULL);
	SYSLOCK(l->pl);

	if (sl_get(&l->pl->mods, name, NULL)) {
		SYSUNLOCK(l->pl);
		return 1;
	}

	SYSUNLOCK(l->pl);
	module *save = l->db;
	library *lib = g_libs;

	while (use_lib && lib->name != NULL) {
		if (!strcmp(lib->name, name)) {
			char *src = strndup((const char *)lib->start, (lib->end-lib->start));
			trealla_consult_text(l->pl, src, name);
			free(src);
			l->db = save;
			return 1;
		}

		lib++;
	}

	char tmpbuf[FUNCTOR_SIZE * 2];

	if (use_lib) {
		const char *path = getenv("TREALLA_LIBRARY_PATH");

		if (path) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%s", path, name);
			struct stat st = {0};

			if (!stat(name, &st)) {
				use_lib = 0;
				name = tmpbuf;
			}
		}
	}

	if (use_lib) {
		const char *path = "./lib";

		if (path) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%s", path, name);
			struct stat st = {0};

			if (!stat(name, &st)) {
				use_lib = 0;
				name = tmpbuf;
			}
		}
	}

#if 0
	if (use_lib) {
		const char *path = "/usr/lib/trealla/library";

		if (path) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%s", path, name);
			struct stat st = {0};

			if (!stat(name, &st)) {
				use_lib = 0;
				name = tmpbuf;
			}
		}
	}
#endif

	int ok = trealla_consult_file(l->pl, name);
	l->db = save;
	return ok;
}

int dir_unload_file(lexer *l, node *n)
{
	node *term1 = n;

	if (!is_atom(term1))
		return 0;

	return trealla_deconsult(l->pl, VAL_S(term1));
}

int dir_function(lexer *l, node *n)
{
	node *term1 = n;

	if (is_structure(term1)) {
		add_function(l, term1);
		return 1;
	}

	while (is_list(term1)) {
		node *n2 = term_firstarg(term1);

		if (is_structure(n2)) {
			add_function(l, n2);
		}

		term1 = term_next(n2);
	}

	return 1;
}
#endif

int dir_include(lexer *l, node *n)
{
	node *term1 = n;

	if (!is_atom(term1))
		return 0;

	char tmpbuf[FUNCTOR_SIZE * 2 + 10];
	strcpy(tmpbuf, l->name);
	char *ptr = strrchr(tmpbuf, '/');

	if (ptr != NULL)
		*++ptr = '\0';
	else
		tmpbuf[0] = 0;

	strcat(tmpbuf, VAL_S(term1));
	return trealla_consult_file(l->pl, tmpbuf);
}

static void directive(lexer *l, node *n)
{
	if (term_arity(n) < 1)
		return;

	if (!is_compound(n))
		return;

	node *head = term_first(n);

	if (!is_atom(head))
		return;

	const char *functor = VAL_S(head);
	node *n3 = term_next(head);

	if (!strcmp(functor, "include"))
		dir_include(l, n3);
	else if (!strcmp(functor, "initialization"))
		dir_initialization(l, n3);
	else if (!strcmp(functor, "set_prolog_flag"))
		dir_set_prolog_flag(l, n3);
	else if (!strcmp(functor, "dynamic")) {
		if (!strcmp(term_functor(n3), ","))
			n3 = term_firstarg(n3);

		dir_dynamic(l, n3);
	}
	else if (!strcmp(functor, "op"))
		dir_op(l, n3);
#ifndef ISO_ONLY
	else if (!strcmp(functor, "module"))
		dir_module(l, n3);
	else if (!strcmp(functor, "using"))
		dir_using(l, n3);
	else if (!strcmp(functor, "export") || !strcmp(functor, "public"))
		dir_export(l, n3);
	else if (!strcmp(functor, "define"))
		dir_define(l, n3);
	else if (!strcmp(functor, "function"))
		dir_function(l, n3);
	else if (!strcmp(functor, "ensure_loaded") || !strcmp(functor, "import") || !strcmp(functor, "use_module"))
		dir_use_module(l, n3);
	else if (!strcmp(functor, "unload_file"))
		dir_unload_file(l, n3);
#endif
}

// This basically removes the effect of redundant parenthesis
// by promoting the inner structure of 'n' into 'term'.

static node *promote(node *term, node *n)
{
	if (term_arity(term) > 0)
		return n;

	term_remove(term, n);
	term_concat(term, n);

	if (is_builtin(n)) {
		term->flags |= FLAG_BUILTIN;
		term->bifptr = n->bifptr;
	}

	term->flags |= FLAG_ATTACHED | FLAG_PROMOTED;
	term_heapcheck(n);
	return NULL;
}

static node *attach_op_infix(lexer *l, node *term, node *n, const char *functor)
{
	node *n_prev, *n_next;

	if ((n_prev = term_prev(n)) == NULL)
		return (l->error = 1, NULL);

	if ((n_next = term_next(n)) == NULL)
		return (l->error = 1, NULL);

	node *tmp = make_compound();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n)) {
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	term_insert_before(term, n, tmp);
	term_remove(term, n);
	term_remove(term, n_prev);
	term_append(tmp, n);
	term_append(tmp, n_prev);

	if (!strcmp(functor, ";")) {
		node *n2 = make_true();
		n2->flags |= FLAG_HIDDEN | FLAG_NOFOLLOW;
		term_append(tmp, n2);
	}

	term_remove(term, n_next);
	term_append(tmp, n_next);

	if (strcmp(functor, ":-") && strcmp(functor, "-->"))
		tmp = promote(term, tmp);

	return tmp;
}

static node *attach_op_prefix(lexer *l, node *term, node *n)
{
	node *n_next = term_next(n);

	if (n_next == NULL)
		return (l->error = 1, NULL);

	node *tmp = make_compound();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n)) {
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	term_insert_before(term, n, tmp);
	term_remove(term, n);
	term_remove(term, n_next);
	term_append(tmp, n);
	term_append(tmp, n_next);
	const char *functor = VAL_S(n);

	if (!strcmp(functor, "\\+")) {
		node *n2 = make_cutfail();
		n2->flags |= FLAG_HIDDEN;
		term_append(tmp, n2);
	}

	tmp = promote(term, tmp);
	return tmp;
}

static node *attach_op_postfix(lexer *l, node *term, node *n)
{
	node *tmp = make_compound();
	tmp->flags |= FLAG_ATTACHED;

	if (is_builtin(n)) {
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = n->bifptr;
	}

	node *n_prev = term_prev(n);

	if (n_prev == NULL)
		return (l->error = 1, NULL);

	term_insert_before(term, n, tmp);
	term_remove(term, n);
	term_remove(term, n_prev);
	term_append(tmp, n);
	term_append(tmp, n_prev);
	tmp = promote(term, tmp);
	return tmp;
}

static int attach_ops(lexer *l, node *term)
{
	if (!is_compound(term) || (term->flags & FLAG_ATTACHED))
		return 0;

	unsigned priority = UINT_MAX;
	int was_operator = 0;
	int xfy = 0;

	for (node *n = term_first(term); n != NULL; n = term_next(n)) {
		while (attach_ops(l, n))
			;

		if (!is_atom(n)) {
			was_operator = 0;
			continue;
		}

		const char *functor = VAL_S(n);

		// Bit of a hack to allow op '' & op ' '

		if (is_quoted(n) && functor[0] && !isspace(functor[0])) {
			was_operator = 0;
			continue;
		}

		const op *optr = get_op(&l->pl->db, functor, !term_prev(n) ? 1 : 0);

		if (!optr->fun) {
			was_operator = 0;
			continue;
		}

		if (was_operator && !strcmp(functor, "-")) {
			node *tmp = term_next(n);

			if (!tmp)
				continue;

			if (is_number(tmp)) {
				if (is_float(tmp))
					tmp->val_f = -tmp->val_f;
#if USE_SSL
				else if (is_bignum(tmp))
					BN_set_negative(tmp->val_bn, !BN_is_negative(tmp->val_bn));
#endif
				else
					tmp->val_i = -tmp->val_i;

				term_remove(term, n);
				term_heapcheck(n);
				n = tmp;
				continue;
			}

			if (!(n->flags & FLAG_CONST))
				free(n->val_s);

			functor = n->val_s = (char *)"--";
			n->flags |= FLAG_CONST;
			n->bifptr = bif_iso_reverse;
		}
		else if (was_operator && !strcmp(functor, "+")) {
			node *tmp = term_next(n);

			if (!tmp)
				continue;

			term_remove(term, n);
			term_heapcheck(n);
			n = tmp;
			continue;
		}

		was_operator = 0;

		if (!optr->fun)
			continue;

		was_operator = 1;

		if (optr->priority < priority) {
			xfy = !strcmp(optr->spec, "xfy");
			priority = optr->priority;
		}
	}

	int did_something = 0;

	for (node *n = xfy ? term_last(term) : term_first(term); n != NULL; n = xfy ? term_prev(n) : term_next(n)) {
		if (!is_atom(n) /*|| (n->flags & FLAG_QUOTED)*/)
			continue;

		if (!strcmp(VAL_S(n), ",") && (n->flags & FLAG_QUOTED)) // HACK
			continue;

		const char *functor = VAL_S(n);
		const op *optr = get_op(&l->pl->db, functor, !term_prev(n) ? 1 : 0);

		if (!optr->fun)
			continue;

		if (optr->priority != priority)
			continue;

		//printf("### attach_ops FUNCTOR = '%s' / '%s'\n", functor, optr->spec);

		if (OP_PREFIX(optr->spec)) {
			n = attach_op_prefix(l, term, n);

			if ((n == NULL) && l->error) {
				printf("ERROR: prefix op '%s' missing params, line %d\n", functor, l->line_nbr);
				return 0;
			}
		}
		else if (OP_POSTFIX(optr->spec)) {
			n = attach_op_postfix(l, term, n);

			if ((n == NULL) && l->error) {
				printf("ERROR: postfix op '%s' missing params, line %d\n", functor, l->line_nbr);
				return 0;
			}
		}
		else if (OP_INFIX(optr->spec)) {
			n = attach_op_infix(l, term, n, functor);

			if ((n == NULL) && l->error) {
				printf("ERROR: infix op '%s' missing params, line %d\n", functor, l->line_nbr);
				l->error = 1;
				return 0;
			}
		}
		else
			return 0;

		if (n == NULL)
			return 0;

		did_something = 1;
	}

	if (did_something)
		return 1;

	term->flags |= FLAG_ATTACHED;

	for (node *n = term_first(term); n != NULL; n = term_next(n)) {
		while (attach_ops(l, n))
			;
	}

	if (is_builtin(term)) {
		const char *functor = VAL_S(term_first(term));
		int arity = term_arity(term);
		term->bifptr = get_bifarity(l, functor, arity)->bifptr;
	}

	return 0;
}

static int dcg_term(lexer *l, node *term, int i, int j)
{
	if (is_builtin(term) || is_passthru(term))
		return 0;

	if (is_atom(term)) {
		node *tmp = term_make();
		tmp->flags |= term->flags;

		if (term->flags & FLAG_SMALL)
			strcpy(tmp->val_ch, term->val_ch);
		else
			tmp->val_s = term->val_s;

		term->flags &= ~TYPE_ATOM;
		term->flags |= TYPE_COMPOUND;
		term->val_s = NULL;
		term_append(term, tmp);
	}

	char tmpbuf[40];
	node *tmp = term_make();
	tmp->flags |= TYPE_VAR | FLAG_CONST;
	snprintf(tmpbuf, sizeof(tmpbuf), "S%u", i);
	tmp->val_s = dict(l->db, tmpbuf);
	attach_vars(l, tmp);
	term_append(term, tmp);
	tmp = term_make();
	tmp->flags |= TYPE_VAR | FLAG_CONST;

	if (j != -1)
		snprintf(tmpbuf, sizeof(tmpbuf), "S%u", j);
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "S");

	tmp->val_s = dict(l->db, tmpbuf);
	attach_vars(l, tmp);
	term_append(term, tmp);
	l->dcg_last = tmp;
	return 1;
}

static node *dcg_list(lexer *l, node *term)
{
	extern int bif_iso_unify(tpl_query *q);
	node *tmp = make_compound();
	tmp->flags |= FLAG_BUILTIN;
	tmp->bifptr = bif_iso_unify;
	node *n = make_const_atom("=", 0);
	n->flags |= FLAG_BUILTIN;
	term_append(tmp, n);
	n = term_make();
	n->flags |= TYPE_VAR | FLAG_CONST;
	n->val_s = dict(l->db, "S0");
	attach_vars(l, n);
	term_append(tmp, n);
	term_append(tmp, term);
	node *nl = term;

	while (is_list(nl)) {
		node *head = term_firstarg(nl);
		node *tail = term_next(head);

		if (is_list(tail)) {
			nl = tail;
			continue;
		}

		term_remove(nl, tail);
		term_heapcheck(tail);
		tail = term_make();
		tail->flags |= TYPE_VAR | FLAG_CONST;
		tail->val_s = dict(l->db, "S");
		attach_vars(l, tail);
		term_append(nl, tail);
		break;
	}

	return tmp;
}

static void dcg_clause(lexer *l, node *term)
{
	if (strcmp(term_functor(term), "-->"))
		return;

	node *func = term_first(term);
	func->val_s = (char*)":-";
	node *head = term_next(func);
	node *body = term_next(head);
	dcg_term(l, head, 0, -1);
	l->dcg_last = NULL;
	int i = 0;

	while (body != NULL) {
		if (is_structure(body)) {
			const char *functor = term_functor(body);

			if (!strcmp(functor, ",") || !strcmp(functor, ";")) {
				head = term_firstarg(body);

				if (dcg_term(l, head, i, i+1))
					i++;

				body = term_next(head);
				continue;
			}
			else
				body = NULL;
		}
		else if (is_list(body)) {
			term_remove(term, body);
			body = dcg_list(l, body);
			term_append(term, body);
			body = NULL;
		}
		else if (is_atom(body)) {
			if (dcg_term(l, body, i, i+1))
				i++;

			body = NULL;
		}
		else
			body = NULL;
	}

	if (l->dcg_last) {
		l->vars--;
		l->dcg_last->val_s = dict(l->db, "S");
		attach_vars(l, l->dcg_last);
	}
}

typedef struct
{
	char *buf, *dst;
	size_t maxlen;
}
 token;

static void token_init(token *t)
{
	t->dst = t->buf = (char *)malloc((t->maxlen = 15) + 1);
	*t->dst = '\0';
}

static void token_put(token *t, int ch)
{
	size_t len = t->dst - t->buf;

	if ((len + 8) >= t->maxlen) {
		t->buf = (char *)realloc(t->buf, (t->maxlen *= 2) + 1);
		t->dst = t->buf + len;
	}

	t->dst += put_char_utf8(t->dst, ch);
	*t->dst = '\0';
}

static char *token_take(token *t)
{
	return t->buf;
}

enum { NUM_NONE=0, NUM_REAL=1, NUM_BIGNUM, NUM_INT, NUM_BINARY, NUM_OCTAL, NUM_HEX };

const char *parse_number(const char *s, nbr_t *value, int *numeric)
{
	if ((*s == '0') && (s[1] == 'b')) {
		unbr_t v = 0;
		s++;
		s++;

		while ((*s == '0') || (*s == '1')) {
			v <<= 1;

			if (*s == '1')
				v |= 1;

			s++;
		}

		*numeric = NUM_BINARY;
		*value = v;
		return s;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		unbr_t v = 0;
		s++;
		s++;

		while ((*s >= '0') && (*s <= '7')) {
			v *= 8;
			v += *s - '0';
			s++;
		}

		*numeric = NUM_OCTAL;
		*value = v;
		return s;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		unbr_t v = 0;
		s++;
		s++;

		while (((*s >= '0') && (*s <= '9')) || ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))) {
			v *= 16;

			if ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))
				v += 10 + (toupper(*s) - 'A');
			else
				v += *s - '0';

			s++;
		}

		*numeric = NUM_HEX;
		*value = v;
		return s;
	}

	*value = 0;
	*numeric = NUM_INT;
	int exp = 0;
	const char *save_s = s;
	char ch = *s;

	do {
		s++;

		if ((ch == '.') && isdigit(*s))
			*numeric = NUM_REAL;
		else if (!exp && ((ch == 'e') || (ch == 'E'))) {
			*numeric = NUM_REAL;
			exp = 1;
		}
		else if (exp && ((ch == '-') || (ch == '+')))
			exp = 0;
#if USE_SSL
		else if (ch == 'B')
			*numeric = NUM_BIGNUM;
#endif
		else if (!isdigit(ch)) {
			s--;
			break;
		}
		else {
			*value *= 10;
			*value += ch - '0';
		}
	}
	 while (ch = *s, (ch != '\0'));

#if USE_SSL
	if ((*numeric == NUM_INT) && g_force_bignum) {
		*numeric = NUM_BIGNUM;
		return s;
	}

	if ((*numeric == NUM_INT) && (strlen(save_s) > 12)) {
		BIGNUM *bn = NULL;
		char *tmpbuf = (char *)malloc(strlen(save_s)+1);
		char *dst = tmpbuf;

		while (isdigit(*save_s))
			*dst++ = *save_s++;

		*dst = '\0';
		BN_dec2bn(&bn, tmpbuf);

		if (BN_num_bits(bn) >= 64)
			*numeric = NUM_BIGNUM;

		if (bn)
			BN_free(bn);

		free(tmpbuf);
	}
#endif

	return s;
}

static const char *get_token(lexer *l, const char *s, char **line)
{
	l->tok = NULL;

	if (!s)
		return NULL;

LOOP:

	while (isspace(*s)) {
		if (*s == '\n')
			l->line_nbr++;

		s++;
	}

	while (*s) {
		if (!l->comment && (*s == '/') && (s[1] == '*')) {
			l->comment = 1;
			s += 2;
			continue;
		}
		else if (l->comment && (*s == '*') && (s[1] == '/')) {
			l->comment = 0;
			s += 2;
			goto LOOP;
		}

		if (!l->comment)
			break;

		s++;
	}

	if (*s == '%') {
		while (*s++)
			;

		return NULL;
	}

	if (!*s || (*s == '\n'))
		return NULL;

	token t;
	token_init(&t);
	l->numeric = NUM_NONE;
	l->quoted = 0;

	if (line)
		l->cpos = (s - *line) - 3;

#ifndef ISO_ONLY
	int is_def = 0;
#endif

	while (*s) {
		l->last = *s;
		int ch = get_char_utf8(&s);

		if ((t.dst != t.buf) && (ch == '?') && (isupper(*s))) {
			s--;
			break;
		}

		if ((ch == '?') && isupper(*s)) {
			token_put(&t, '?');
#ifndef ISO_ONLY
			is_def = 1;
#endif
			continue;
		}

		while (l->quoted) {
			while ((ch = get_char_utf8(&s)) != 0) {
				if (ch == l->quote) {
					if (*s == l->quote)
						s++;
					else
						break;
				}

				if ((ch == '\\') && !*s) {
					ch = '\0';
					break;
				}

				if (l->pl->flag_character_escapes && (l->quoted <= 2) && (ch == '\\')) {
					const char *ptr = strchr(g_anti_escapes, ch = *s++);
					if (ptr)
						token_put(&t, g_escapes[ptr - g_anti_escapes]);
					else
						token_put(&t, ch);
				}
				else
					token_put(&t, ch);

				if (!*s) {
					token_put(&t, '\n');
					ch = '\0';
					break;
				}
			}

			if (!ch && l->fp) {
				char *newline = trealla_readline(l, l->fp, 1);

				if (newline) {
					free(*line);
					*line = newline;
					s = newline;
					continue;
				}
			}

			break;
		}

		if (l->quoted)
			break;

		if ((ch == '\'') || (ch == '"') || (ch == '`')) {
			l->quote = ch;
			l->quoted = l->quote == '`' ? 3 : l->quote == '"' ? 2 : 1;

			while (l->quoted) {
				while ((ch = get_char_utf8(&s)) != 0) {
					if (ch == l->quote) {
						if (*s == l->quote)
							s++;
						else
							break;
					}

					if ((ch == '\\') && !*s) {
						ch = '\0';
						break;
					}

					if (l->pl->flag_character_escapes && (l->quoted <= 2) && (ch == '\\')) {
						const char *ptr = strchr(g_anti_escapes, ch = *s++);

						if (ptr)
							token_put(&t, g_escapes[ptr - g_anti_escapes]);
						else
							token_put(&t, ch);
					}
					else
						token_put(&t, ch);

					if (!*s) {
						token_put(&t, '\n');
						ch = '\0';
						break;
					}
				}

				if (!ch && l->fp) {
					char *newline = trealla_readline(l, l->fp, 1);

					if (newline) {
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

		if (isalpha_utf8(ch) || (ch == '_')) {
			token_put(&t, ch);

			while (*s) {
				ch = get_char_utf8(&s);

				if (!isalnum_utf8(ch) && (ch != '_')) {
#ifndef ISO_ONLY
					int nexch = *s;

					if ((ch == ':') && isalnum(nexch) && (0 || get_ns(l, t.buf)))
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

		if (isdigit(ch)) {
			const char *save_s = --s;
			nbr_t v = 0;
			s = parse_number(s, &v, &l->numeric);
			l->neg = 0;

			if (l->numeric >= NUM_INT) {
				t.dst = t.buf = (char *)realloc(t.buf, (t.maxlen = 255) + 1);
				t.dst += sprint_int(t.buf, t.maxlen, v, 10);
				break;
			} else {
				const char *src = save_s;

				while ((ch = *src++) && isdigit(ch))
					token_put(&t, ch);
			}

			break;
		}

		token_put(&t, ch);

		// These are some hacks...

		if ((ch == '[') || (ch == '{')) {
			while (isspace(*s))
				s++;
		}

		if ((ch == '[') && (*s == ']'))
			token_put(&t, ch = *s++);
		else if ((ch == '{') && (*s == '}'))
			token_put(&t, ch = *s++);
		else if ((ch == '=') && (s[0] == '.') && s[1] == '.') {
			token_put(&t, ch = *s++);
			token_put(&t, ch = *s++);
		}

		static const char seps[] = "%.,;!()[]{}_\"'` \t\r\n";

		if (strchr(seps, ch) || strchr(seps, *s) || isalnum_utf8(*s))
			break;
	}

	l->tok = token_take(&t);
	l->was_paren = l->is_paren;
	l->was_op = l->is_op;

	if (!l->quoted && !strcmp(l->tok, "("))
		l->is_paren = 1;
	else
		l->is_paren = 0;

	if (!l->quoted && !strcmp(l->tok, ","))
		l->is_op = 1;
	else
		l->is_op = 0;

	//printf("### TOKEN \"%s\" numeric=%d, quoted=%d --> \"%s\", is=%d, was=%d\n", l->tok, l->numeric, l->quoted, s, l->is_paren, l->was_paren);

#ifndef ISO_ONLY
	if (l->tok && is_def) {
		const char *key = l->tok + 1; // skip the '?'

		if (!strcmp(key, "RANDOM")) {
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "%d", (int)rand());
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "RANDOMSTR")) {
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "'%d'", (int)rand());
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "TIME")) {
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "%lld", (long long)time(NULL));
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "TIMESTR")) {
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "'%lld'", (long long)time(NULL));
			get_token(l, tmpbuf, line);
			return s;
		}
		else if (!strcmp(key, "TIMESTR")) {
			free(l->tok);
			char tmpbuf[80];
			sprintf(tmpbuf, "'%lld'", (long long)time(NULL));
			get_token(l, tmpbuf, line);
			return s;
		}

		const char *value = get_define(l, key);

		if (value != NULL) {
			free(l->tok);
			get_token(l, value, line);
			return s;
		}

		printf("Warning: undefined constant '%s'\n", key);
		l->error = 1;
		get_token(l, key, line);
	}
#endif

	return s;
}

void attach_vars(lexer *self, node *var)
{
	void *v;

	if (sl_get(&self->symtab, VAL_S(var), &v)) {
		var->slot = (uint16_t)(size_t)v;
		return;
	}

	var->slot = self->vars++;
	sl_set(&self->symtab, strdup(VAL_S(var)), (void *)(size_t)var->slot);
}

lexer *lexer_create(trealla *pl)
{
	lexer *l = (lexer *)calloc(1, sizeof(lexer));
	lexer_init(l, pl);
	return l;
}

void lexer_destroy(lexer *self)
{
	lexer_done(self);
	free(self);
}

void lexer_init(lexer *self, trealla *pl)
{
	memset(self, 0, sizeof(lexer));
	sl_init(&self->symtab, 0, &strcmp, &free);
	sl_init(&self->ns, 0, &strcmp, &free);

#ifndef ISO_ONLY
	sl_init(&self->defines, 0, &strcmp, &free);
	sl_init(&self->funs, 0, &strcmp, &free);
#endif

	self->pl = pl;
	self->db = &pl->db;
}

void lexer_done(lexer *self)
{
#ifndef ISO_ONLY
	sl_done(&self->funs, NULL);
	sl_done(&self->defines, &free);
#endif

	sl_done(&self->ns, NULL);
	sl_done(&self->symtab, NULL);

	if (self->name != NULL)
		free(self->name);

	if (self->init != NULL)
		free(self->init);

	memset(self, 0, sizeof(lexer));
}

static void lexer_finalize(lexer *self)
{
	if (self->fact) {
		node *tmp = make_const_atom(":-", 0);
		tmp->flags |= FLAG_BUILTIN;
		term_append(self->r, tmp);
		tmp = make_const_atom("true", 0);
		tmp->flags |= FLAG_BUILTIN | FLAG_HIDDEN;
		tmp->bifptr = bif_iso_true;
		term_append(self->r, tmp);
	}

	if ((self->r == NULL) || self->error)
		return;

	while (attach_ops(self, self->r))
		;

	if (!strcmp(term_functor(self->r), "?-")) {
		NLIST_PUSH_BACK(&self->val_l, self->r);
	}
	else if (!strcmp(term_functor(self->r), ":-")) {
		if (self->consult) {
			directive(self, term_firstarg(self->r));
			term_heapcheck(self->r);
		} else {
			self->r->flags |= FLAG_CLAUSE;
			NLIST_PUSH_BACK(&self->val_l, self->r);
		}
	} else {
		node *r = term_first(self->r);

		if (term_count(self->r) > 1) {
			printf("ERROR: syntax error, excess terms\n");
			term_heapcheck(self->r);
			self->error = 1;
			return;
		}

		dcg_clause(self, r);
		r->frame_size = self->vars;
		r->flags |= FLAG_CLAUSE;

		if (self->fact)
			r->flags |= FLAG_FACT;

		term_remove(self->r, r);
		NLIST_PUSH_BACK(&self->val_l, r);
		term_heapcheck(self->r);
	}

	sl_clear(&self->symtab, NULL);
	self->r = NULL;
}

const char *lexer_parse(lexer *self, node *term, const char *src, char **line)
{
	if (src == NULL)
		return NULL;

	self->depth++;
	int first_neg = 1;

	while ((src = get_token(self, src, line)) != NULL) {
		if (!self->quoted && !*self->tok) {
			free(self->tok);
			self->was_atomic = 0;
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, "->")) {
			free(self->tok);
			node *tmp = make_and();
			term_append(term, tmp);
			tmp = make_cut();
			term_append(term, tmp);
			tmp = make_and();
			term_append(term, tmp);
			self->was_atomic = 0;
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, ".") && (*src != '(')) {
			free(self->tok);
			self->finalized = 1;
			self->was_atomic = 0;
			break;
		}

		if (!self->r) {
			self->r = term = make_compound();
			self->term = NULL;
			self->vars = 0;
			self->fact = 1;
			self->dcg = 0;
			self->dcg_passthru = 0;
			term->flags |= FLAG_NOARGS;
		}

		if (!self->quoted && !strcmp(self->tok, ")") && is_atom(term_first(term))) {
			if (!strcmp(term_functor(term), "once")) {
				node *tmp = make_and();
				tmp->flags |= FLAG_HIDDEN;
				term_append(term, tmp);
				tmp = make_cut();
				tmp->flags |= FLAG_HIDDEN;
				term_append(term, tmp);
			}
		}

		if (!self->quoted && (!strcmp(self->tok, "]") || !strcmp(self->tok, ")"))) {
			if (term->flags & FLAG_CONSING) {
				node *tmp = term_make();
				tmp->flags |= TYPE_ATOM | FLAG_CONST;
				tmp->val_s = (char *)"[]";
				term_append(term, tmp);
			}

			free(self->tok);

			if (!--self->depth) {
				printf("ERROR: syntax error, no depth\n");
				self->error = 1;
				return NULL;
			}

			if (term_arity(term) == 0) {
				*term = *term_first(term);
#ifdef DEBUG
				g_allocs--;
#endif
			}

			self->was_atomic = 0;
			return src;
		}

		if (self->dcg && !self->quoted && !strcmp(self->tok, "|") && !self->dcg_passthru) {
			free(self->tok);
			self->tok = strdup(";");
		}
		else if (self->dcg && !self->quoted && !strcmp(self->tok, "}") && !--self->dcg_passthru) {
			free(self->tok);
			continue;
		}
		else if (self->dcg && !self->quoted && !strcmp(self->tok, "{") && !self->dcg_passthru++) {
			free(self->tok);
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, "}")) {
			free(self->tok);

			if (!--self->depth) {
				printf("ERROR: syntax error, no depth\n");
				self->error = 1;
				return NULL;
			}

			self->was_atomic = 0;
			return src;
		}

		if (!self->quoted && !strcmp(self->tok, ",") && !is_noargs(term)) {
			self->was_atom = 0;

			if (term->flags & FLAG_CONSING) {
				free(self->tok);
				node *tmp = make_list();
				tmp->flags |= FLAG_CONSING;
				term_append(term, tmp);
				term = tmp;
				self->was_atomic = 0;
				continue;
			}

			free(self->tok);
			self->was_atomic = 0;
			continue;
		}

		if (!self->quoted && !strcmp(self->tok, "-") && !is_noargs(term) && first_neg) {
			self->was_atom = 0;
			free(self->tok);
			self->was_atomic = 0;
			continue;
		}

		first_neg = 0;

		if (!self->quoted && !strcmp(self->tok, "|") && !(term->flags & FLAG_CONSING)) {
			printf("ERROR: extra bar: '%s'\n", self->tok);
			self->error = 1;
			free(self->tok);
			return src;
		}
		else if (!self->quoted && !strcmp(self->tok, "|") && (term->flags & FLAG_CONSING)) {
			free(self->tok);
			term->flags &= ~FLAG_CONSING;
			self->was_atomic = 0;
			continue;
		}

		if (!self->quoted && (!strcmp(self->tok, ":-") || !strcmp(self->tok, "-->") || !strcmp(self->tok, "?-"))) {
			if (!strcmp(self->tok, "-->"))
				self->dcg = 1;

			self->fact = 0;
		}

		node *n = NULL;

		//printf("*** tok='%s' is_paren=%d, was_paren=%d\n", self->tok, self->is_paren, self->was_paren);

		if (!self->quoted && !strcmp(self->tok, "{}")) {
			free(self->tok);
			n = make_const_atom("{}", 0);
		}
		else if (!self->quoted && !strcmp(self->tok, "[]")) {
			free(self->tok);
			n = make_const_atom("[]", 0);
		}
		else if (!self->quoted && !strcmp(self->tok, "[")) {
			free(self->tok);
			n = make_list();
			n->flags |= FLAG_CONSING;
			self->was_atomic = 0;
			src = lexer_parse(self, n, src, line);

			if (self->error) {
				term_heapcheck(n);
				return src;
			}
		}
		else if (!self->quoted && !strcmp(self->tok, "{")) {
			free(self->tok);
			n = make_tuple();
			n->flags |= FLAG_NOARGS;
			self->was_atomic = 0;
			src = lexer_parse(self, n, src, line);

			if (self->error) {
				term_heapcheck(n);
				return src;
			}
		}
		else if (!self->quoted && !strcmp(self->tok, "(")) {
			free(self->tok);
			n = make_compound();
			n->flags |= FLAG_NOARGS;

			if ((term_count(term) != 0) && !self->was_paren) {
				node *tmp = term_last(term);

				if (is_atom(tmp) && self->was_atom) {
					const char *functor = VAL_S(tmp);
					const op *optr = get_op(&self->pl->db, functor, 0);
					int doit = 0;

					if (!strcmp(functor, g_list_cons)) {
						n->flags |= FLAG_LIST;
						doit = 1;
					}

					if (!optr->fun || is_quoted(tmp) || doit) {
						term_remove(term, tmp);
						term_append(n, tmp);
						n->flags &= ~FLAG_NOARGS;
						n->cpos = tmp->cpos;

						if (!strcmp(functor, "call") || !strcmp(functor, "phrase") ||
							!strcmp(functor, "bagof") || !strcmp(functor, "setof") ||
						    !strcmp(functor, "sys:xmlq") || !strcmp(functor, "xmlq") ||
						    !strcmp(functor, "sys:write_file") || !strcmp(functor, "write_file") ||
							!strcmp(functor, "findnsols")) {
							node *tmp = term_make();
							tmp->flags |= TYPE_VAR | FLAG_ANON | FLAG_HIDDEN | FLAG_CONST;
							char tmpbuf[40];
							snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->vars);
							tmp->val_s = dict(self->db, tmpbuf);
							attach_vars(self, tmp);
							term_append(n, tmp);
						}

						if (is_builtin(tmp))
							n->flags |= FLAG_BUILTIN;
					}
				}
			}

			self->was_atomic = 0;
			src = lexer_parse(self, n, src, line);

#ifndef ISO_ONLY
			if (get_function(self, n)) {
				node *tmp = term_make();
				tmp->flags |= TYPE_VAR | FLAG_ANON | FLAG_HIDDEN | FLAG_CONST;
				char tmpbuf[40];
				snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->vars);
				tmp->val_s = dict(self->db, tmpbuf);
				attach_vars(self, tmp);
				term_append(n, tmp);
			}
#endif

			if (self->error) {
				term_heapcheck(n);
				return src;
			}
		}
#if USE_SSL
		else if (self->numeric == NUM_BIGNUM) {
			n = make_bignum(self->tok);
			free(self->tok);
		}
#endif
		else if (self->numeric >= NUM_INT) {
			n = make_int(dec_to_int(self->tok));

			if (self->numeric == NUM_BINARY)
				n->flags |= FLAG_BINARY;
			else if (self->numeric == NUM_OCTAL)
				n->flags |= FLAG_OCTAL;
			else if (self->numeric == NUM_HEX)
				n->flags |= FLAG_HEX;

			free(self->tok);
		}
		else if (self->numeric == NUM_REAL) {
			n = make_float(strtod(self->tok, NULL));
			free(self->tok);
		}
		else if (!self->quoted && !strcmp(self->tok, "pi")) {
			n = make_float(PI);
			n->flags |= FLAG_PI;
			free(self->tok);
		}
		else if (!self->quoted && ((self->tok[0] == '_') || isupper(self->tok[0]))) {
			n = term_make();
			n->flags |= TYPE_VAR | FLAG_CONST;

			if (!strcmp(self->tok, "_")) {
				n->flags |= FLAG_ANON;
				char tmpbuf[40];
				snprintf(tmpbuf, sizeof(tmpbuf), "_%d", self->vars);
				n->val_s = dict(self->db, tmpbuf);
			}
			else
				n->val_s = dict(self->db, self->tok);

			free(self->tok);
			attach_vars(self, n);
		}
		else if (!self->error && !self->quoted && !is_op(self->db, self->tok) &&
				!isalnum_utf8(self->tok[0]) && strcmp(self->tok, "!") && strcmp(self->tok, ".") &&
				needs_quoting(self->tok)) {
			printf("ERROR: unknown operator: '%s'\n", self->tok);
			self->error = 1;
			return NULL;
		} else {
			self->was_atom = 1;
			n = term_make();
			n->flags |= TYPE_ATOM;

			if ((self->was_paren || self->was_op) &&
					!self->quoted && is_op(self->db, self->tok) &&
						strcmp(self->tok, "\\+")) {	// HACK
				n->flags |= FLAG_NOOP;
				self->quoted = 1;
			}

			if (self->quoted)
				n->flags |= FLAG_QUOTED;

			if (self->quoted == 2)
				n->flags |= FLAG_DOUBLE_QUOTE;

#ifndef ISO_ONLY
			if (self->quoted == 3) {
				n->flags |= FLAG_BLOB;
				char *dstbuf = (char *)malloc(strlen(self->tok) + 1);
				size_t len = b64_decode(self->tok, strlen(self->tok), &dstbuf);
				n->val_len = len;
				free(self->tok);
				self->tok = dstbuf;
			}
#endif

			if (self->quoted < 2) {
				if ((n->bifptr = get_bif(self, self->tok)->bifptr) != NULL)
					n->flags |= FLAG_BUILTIN;
			}

			if (self->quoted && !*self->tok) {
				n->flags |= FLAG_CONST;
				free(self->tok);
				self->tok = (char *)"";
			}

			if (!self->quoted) {
				n->flags |= FLAG_CONST;
				n->val_s = dict(self->db, self->tok);
				free(self->tok);
			}
			else if (*self->tok && (strlen(self->tok) < sizeof(n->val_ch))) {
				n->flags |= FLAG_CONST | FLAG_SMALL;
				strcpy(n->val_ch, self->tok);
				free(self->tok);
			}
			else
				n->val_s = self->tok;
		}

		int is_op = 1;
		const op *optr = get_op(&self->pl->db, is_atom(n) ? VAL_S(n) : "| |", 0);

		if (!optr->fun)
			is_op = 0;

		if (!self->error && self->was_atomic && !is_op) {
			printf("ERROR: operator expected: '%s'\n", VAL_S(n));
			term_heapcheck(n);
			self->error = 1;
			return NULL;
		}

		self->was_atomic = !is_op;

		if (line)
			n->cpos = self->cpos;

		if (self->dcg_passthru)
			n->flags |= FLAG_PASSTHRU;

		term_append(term, n);
	}

	self->depth--;

	if (self->finalized) {
		if (!self->error && self->depth != 0) {
			printf("ERROR: check parentheses, brackets or braces\n");
			self->error = 1;
			return src;
		}

		lexer_finalize(self);

		if (self->error)
			return NULL;

		return src;
	}

	if (self->fp && !feof(self->fp) && (line != NULL)) {
		free(*line);
		*line = trealla_readline(self, self->fp, 1);

		if (*line == NULL)
			return NULL;

		src = lexer_parse(self, term, *line, line);

		if (!src && !self->error && self->r && !self->finalized && feof(self->fp)) {
			printf("ERROR: premature end found\n");
			self->error = 1;
		}

		if (!src) {
			if (*line == NULL)
				free(*line);

			return NULL;
		}

		if (self->error)
			return NULL;
	}

	return src;
}

static const char *exts[] = {".prolog",".pro",".pl",".P", NULL};

int lexer_consult_fp(lexer *self, FILE *fp)
{
	node *save_rule = self->r;
	FILE *save_fp = self->fp;
	self->r = NULL;
	self->fp = fp;
	self->line_nbr = 0;
	char *line;

	while ((line = trealla_readline(self, fp, 0)) != NULL) {
		if ((self->line_nbr == 1) && (line[0] == '#') && (line[1] == '!'))
			continue;

		const char *src = line;

		while (((src = lexer_parse(self, self->r, src, &line)) != NULL) && !self->error)
			self->finalized = 0;

		if (self->error && (line != NULL)) {
			printf("ERROR: consult '%s', "
			       "line=%d, '%s'\n",
			       (self->name ? self->name : "console"),
			       self->line_nbr,
			       (line ? line : "end_of_file reached unexpectedly"));

			free(line);
			break;
		}

		if (line)
			free(line);
	}

	if (self->error) {
		node *n = NLIST_FRONT(&self->val_l);
		term_heapcheck(n);
		term_heapcheck(self->r);
		return 0;
	}

	self->fp = save_fp;
	self->r = save_rule;
	return !self->error;
}

int lexer_consult_file(lexer *self, const char *orig_filename)
{
	char tmpbuf[FUNCTOR_SIZE * 2 + 10];

	if (orig_filename[0] == '~') {
		const char *path = getenv("HOME");

		if (path)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%s", path, orig_filename+1);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", orig_filename);
	}
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s", orig_filename);

	const char *filename = tmpbuf;
	FILE *fp = fopen(filename, "rb");
	size_t i = 0;

	while ((fp == NULL) && exts[i]) {
		char tmpbuf[1024 * 2];
		strncpy(tmpbuf, filename, sizeof(tmpbuf) - 10);
		strcat(tmpbuf, exts[i++]);
		fp = fopen(tmpbuf, "rb");
	}

	if (fp == NULL) {
		self->error = 1;
		return 0;
	}

	self->name = strdup(filename);
	lexer_consult_fp(self, fp);
	fclose(fp);
	return self->error ? 0 : 1;
}
