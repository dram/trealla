#include <ctype.h>
#include <float.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <io.h>
#define snprintf _snprintf
#define isatty _isatty
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "trealla.h"

#include "bifs.h"
#include "history.h"
#include "internal.h"
#include "jela.h"

#ifndef ISO_ONLY
#include "uuid.h"
#endif

static size_t g_instances = 0;
#ifdef DEBUG
atomic uint64_t g_allocs = 0;
#endif
int g_trealla_memlimit_mb = 1024;
volatile int g_abort = 0;
const char *g_trealla_version = "0.1alpha";
const char *g_list_cons = ".";

#ifdef DEBUG
uint64_t g_enqueues = 0, g_rescheds = 0, g_choicepoints = 0,
	g_heap_used = 0, g_backtracks = 0, g_executes = 0,
	g_reexecutes = 0, g_tailrecurses = 0, g_cuts = 0, g_u_resolves = 0,
	g_s_resolves = 0;
#endif

#ifndef ISO_ONLY
uint64_t g_busy = 0;
#endif

static void kvs_done(skiplist *d)
{
	sl_start(d);
	node *n = NULL;

	while ((sl_next(d, (void **)&n)) != NULL)
		term_heapcheck(n);

	sl_done(d, NULL);
}

static void stream_close(stream *sp)
{
	if (sp->filename)
		free(sp->filename);

	if (sp->mode)
		free(sp->mode);

	if (sp->type)
		free(sp->type);

	if (sp->fptr && (sp->fptr != stdin) && (sp->fptr != stdout))
		fclose(sp->fptr);

	if (sp->subqptr)
		query_destroy(sp->subqptr);

	if (sp->subqgoal)
		term_heapcheck(sp->subqgoal);

	if (sp->kvs) {
		kvs_done(sp->kvs);
		free(sp->kvs);
	}

#ifndef ISO_ONLY
	if (sp->sptr)
		session_close((session *)sp->sptr);
#endif
}

void term_destroy(node *n)
{
	while (n) {
		node *save = n;
		n = term_next(n);
		term_heapcheck(save);
	}
}

void term_heapclean(node *n)
{
	if (is_compound(n)) {
		node *n2 = term_first(n);

		while (n2) {
			node *save = n2;
			n2 = term_next(n2);
			term_heapcheck(save);
		}
	}
	else if (!(n->flags & FLAG_CONST)) {
		if (is_atom(n))
			free(n->val_s);
		else if (is_var(n))
			free(n->val_s);
		else if (is_stream(n)) {
			stream_close(n->val_str);
			free(n->val_str);
		}
	}

#ifdef DEBUG
	g_allocs--;
#endif
	free(n);
}

const char *make_key(trealla *pl, char *dstbuf, node *term)
{
	if (is_atom(term))
		return VAL_S(term);

	if (is_compound(term) && !is_op(&pl->db, term_functor(term)))
		snprintf(dstbuf, KEY_SIZE, "%s/%d", VAL_S(term_firstarg(term)), term_arity(term));
	else if (is_compound(term))
		term_sprint(dstbuf, KEY_SIZE, pl, NULL, term_firstarg(term), 0);
	else
		term_sprint(dstbuf, KEY_SIZE, pl, NULL, term, 0);

	return dstbuf;
}

// Should be called with DBLOCK in effect

static int assert_index(lexer *l, node *n, int manual, int *persist, int append_mode, int in_tran)
{
	module *db = l->db;
	node *tmp = term_first(n);
	node *head = tmp = term_next(tmp), *idx = NULL;
	int arity = 0;
	*persist = 0;

	if (is_compound(head)) {
		arity = term_arity(head);
		tmp = term_first(head);
		idx = term_next(tmp);
	}

	const char *functor = VAL_S(tmp);
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	char *save = NULL;

	if (!(tmp->flags & FLAG_CONST))
		save = tmp->val_s;

	tmp->flags |= FLAG_CONST;
	tmp->val_s = dict(db, VAL_S(tmp));

	if (save != NULL)
		free(save);

	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void **)&r)) {
		const char *functor = strdup(tmpbuf);
		r = calloc(1, sizeof(rule));
		r->functor = functor;
		r->db = db;
		r->dynamic = r->manual = manual > 0;
		r->hidden = is_hidden(n);
		//r->idx = sb_string_create();
		sl_set(&db->rules, functor, r);
	}

	// Don't override in-built predicates...

	if (r->hidden && (manual >= 0)) {
		if (!l->pl->quiet)
			printf("WARN: ignoring user predicate '%s'\n", tmpbuf);
		return 0;
	}

	if (idx && r->idx) {
		char tmpbuf[KEY_SIZE];

#ifndef ISO_ONLY
		if (r->numeric) {
			if (!is_integer(idx)) {
				printf("ERROR: index type mismatch\n");
				return 0;
			}

			const nbr_t key = idx->val_i;

			if (append_mode)
				sb_int_app(r->idx, key, n);
			else
				sb_int_set(r->idx, key, n);
		}
		else
#endif
		{
			const char *key = make_key(l->pl, tmpbuf, idx);

			if (append_mode)
				sb_string_app(r->idx, key, n);
			else
				sb_string_set(r->idx, key, n);
		}
	}

	if (append_mode)
		NLIST_PUSH_BACK(&r->val_l, n);
	else
		NLIST_PUSH_FRONT(&r->val_l, n);

#ifndef ISO_ONLY
	if (r->persist) {
		*persist = 1;
		nbr_t save_pos = dbs_get_fpos(db);

		if (!db->loading) {
			size_t buflen = 1024 * 64; // expandable
			char *dstbuf = (char *)malloc(buflen + 1);
			dbs_save_node(db, db->fp, &dstbuf, &buflen, n, in_tran);
			free(dstbuf);
		}
		else
			save_pos +=  n->cpos;

		if (r->storage) {
			node *head = term_firstarg(n);
			node *tmp_fa = term_firstarg(head);
			node *tmp_rest = term_next(tmp_fa);
			node *tmp;

			while ((tmp = term_remove(head, tmp_rest)) != NULL) {
				term_heapcheck(tmp_rest);
				tmp_rest = tmp;
			}

			term_heapcheck(tmp_rest);
			tmp = make_int(save_pos);
			tmp->flags |= FLAG_PTR;
			n->flags |= FLAG_DBS_STORAGE;
			term_append(head, tmp);
		}
	}

	if (r->notify) {
		slnode *iter = sl_startx(&r->procs);
		tpl_query *who;

		while ((who = (tpl_query*)sl_nextx(&iter, NULL)) != NULL)
			process_enqueue(l->pl, NULL, who, NULL, 0);

		sl_clear(&r->procs, NULL);
	}
#endif

	return 1;
}

int asserta_index(lexer *l, node *n, int manual, int *persist, int in_tran)
{
	return assert_index(l, n, manual, persist, 0, in_tran);
}

int assertz_index(lexer *l, node *n, int manual, int *persist, int in_tran)
{
	return assert_index(l, n, manual, persist, 1, in_tran);
}

// Should be called with DBLOCK in effect

int retract_index(lexer *l, node *n, node *n2, int *persist, int in_tran)
{
	module *db = l->db;
	node *tmp = term_first(n);
	node *head = tmp = term_next(tmp), *idx = NULL;
	int arity = 0;
	*persist = 0;

	if (is_compound(head)) {
		arity = term_arity(head);
		tmp = term_first(head);
		idx = term_next(tmp);
	}

	const char *functor = VAL_S(tmp);
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void **)&r))
		return 0;

	if (idx && r->idx) {
#ifndef ISO_ONLY
		if (r->numeric) {
			if (!is_integer(idx)) {
				printf("ERROR: index type mismatch\n");
				return 0;
			}

			const nbr_t key = idx->val_i;

			if (!sb_int_erase(r->idx, key, n, NULL))
				printf("DEBUG: retract %lld not found in index\n", (long long)key);
		}
		else
#endif
		{
			char tmpbuf[KEY_SIZE];
			const char *key = make_key(l->pl, tmpbuf, idx);

			if (!sb_string_erase(r->idx, key, n, NULL))
				printf("DEBUG: retract '%s' not found in index\n", key);
		}
	}

	NLIST_REMOVE(&r->val_l, n);
	term_heapcheck(n);

#ifndef ISO_ONLY
	if (r->persist) {
		*persist = 1;

		if (!db->loading) {
			size_t buflen = 1024 * 64; // expandable
			char *dstbuf = (char *)malloc(buflen + 1);
			dbs_save_node(db, db->fp, &dstbuf, &buflen, n2, in_tran);
			free(dstbuf);
		}
	}
#endif

	return 1;
}

char *trealla_readline(lexer *l, FILE *fp, int more)
{
	if ((fp == stdin) && l->pl->tty)
		return history_readline_eol(more ? "|: " : "?- ", '.');

	size_t maxlen = 0, blocksize = 1024 * 8;

	while (!feof(fp)) {
		char *line = NULL;
		maxlen += blocksize;

		if (!(line = (char *)realloc(line, maxlen + 1)))
			break;

		char *dst = (line + maxlen) - blocksize;
		int ch;

		while (ch = fgetc(fp), (ch == '\n'))
			l->line_nbr++;

		ungetc(ch, fp);
		clearerr(fp);

		for (;;) {
			int ch = fgetc(fp);

			if (ch == EOF) {
				if (dst == line) {
					free(line);
					return NULL;
				}

				l->line_nbr++;
				*dst = '\0';
				return line;
			}

			if (ch != '\n')
				*dst++ = ch;
			else {
				l->line_nbr++;
				*dst = '\0';
				return line;
			}

			if (dst == (line + maxlen - 8))	// 8 byte spare
				break;
		}

		blocksize *= 2;
	}

	return NULL;
}

#ifdef _WIN32
uint64_t gettimeofday_usec(void)
{
	static const uint64_t epoch = 116444736000000000ULL;
	FILETIME file_time;
	SYSTEMTIME system_time;
	ULARGE_INTEGER u;
	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	u.LowPart = file_time.dwLowDateTime;
	u.HighPart = file_time.dwHighDateTime;
	return (u.QuadPart - epoch) / 10 + (1000ULL * system_time.wMilliseconds);
}
#else
uint64_t gettimeofday_usec(void)
{
	struct timeval tp;
	gettimeofday(&tp, NULL);
	return ((uint64_t)tp.tv_sec * 1000 * 1000) + tp.tv_usec;
}
#endif

static void rule_done(void *p)
{
	rule *r = (rule *)p;
	term_destroy(NLIST_FRONT(&r->val_l));
	sb_destroy(r->idx);

#ifndef ISO_ONLY
	if (r->notify) {
		slnode *iter = sl_startx(&r->procs);
		tpl_query *who;

		while ((who = (tpl_query*)sl_nextx(&iter, NULL)) != NULL)
			query_destroy(who);

		sl_done(&r->procs, NULL);
	}
#endif

	free(r);
}

void db_init(module *self, trealla *pl, const char *name, const char *filename)
{
	self->pl = pl;
	self->name = strdup(name);
	self->filename = strdup(filename ? filename : name);
	sl_init(&self->dict, 0, &strcmp, &free);
	sl_init(&self->rules, 0, &strcmp, &free);
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

	node *tmp;

	while ((tmp = NLIST_POP_FRONT(&self->tran_queue)) != NULL) {
		tmp->n1->flags &= ~(FLAG_DBS_ASSERTA | FLAG_DBS_ASSERTZ | FLAG_DBS_RETRACT);
		tmp->n1->flags &= ~FLAG_DELETED;

		if (tmp->n2)
			term_heapcheck(tmp->n2);

		free(tmp);
	}

	lock_destroy(self->guard);
	dbs_done(self);
#endif
	free(self->filename);
	free(self->name);
}

static void db_free(void *p)
{
	if (!p)
		return;

	db_done((module *)p);
	free(p);
}

static rule *xref_term2(lexer *l, module *db, const char *functor, node *term, int arity)
{
	if (!db)
		return NULL;

	if (!strcmp(functor, "[]") || !strcmp(functor, g_list_cons) || (term->flags & FLAG_QUOTED))
		return NULL;

	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);
	rule *r = NULL;

	if (!sl_get(&db->rules, tmpbuf, (void **)&r))
		return NULL;

	char *save = NULL;

	if (!(term->flags & FLAG_CONST))
		save = VAL_S(term);

	if (r->dynamic)
		term->flags |= FLAG_DYNAMIC;

	term->flags |= FLAG_CONST;
	term->val_s = dict(l->db, VAL_S(term));

	if (save != NULL)
		free(save);

	return r;
}

rule *xref_term(lexer *l, node *term, int arity)
{
	// printf("*** XREF_TERM "); term_print(l->pl, NULL, term, 1); printf(" => arity=%d\n", arity);

	const char *functor = VAL_S(term);
	const char *src = strchr(functor, ':');
	rule *r = NULL;

	if (src) {
		char tmpbuf2[FUNCTOR_SIZE + 10];
		strncpy(tmpbuf2, functor, src - functor);
		tmpbuf2[src - functor] = '\0';
		functor = src + 1;
		module *db = NULL;

		if (sl_get(&l->pl->mods, tmpbuf2, (void **)&db)) {
			if (!db) {
				printf("ERROR: in '%s', no module '%s:%s/%d'\n", l->db->name, tmpbuf2, functor, arity);
				l->error = 1;
				return NULL;
			}

			char tmpbuf[FUNCTOR_SIZE + 10];
			snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", functor, arity);

			if (!sl_get(&db->exports, tmpbuf, NULL)) {
				printf("ERROR: in '%s', no export '%s:%s/%d'\n", l->db->name, tmpbuf2, functor, arity);
				l->error = 1;
				return NULL;
			}

			if (!sl_get(&db->rules, tmpbuf, (void **)&r)) {
				const char *functor = strdup(tmpbuf);
				r = calloc(1, sizeof(rule));
				r->functor = functor;
				//r->idx = sb_string_create();
				sl_set(&db->rules, functor, r);
			}

			char *save = NULL;

			if (!(term->flags & FLAG_CONST))
				save = VAL_S(term);

			term->flags |= FLAG_CONST;
			term->val_s = dict(db, functor);

			if (save != NULL)
				free(save);

			return r;
		}
		else if (tmpbuf2[0] && functor[0]) {
			printf("ERROR: in '%s', not exported '%s:%s/%d'\n", l->db->name, tmpbuf2, functor, arity);
			l->error = 1;
			return NULL;
		}
	}

	r = xref_term2(l, l->db, functor, term, arity);

	if (r != NULL)
		return r;

	r = xref_term2(l, &l->pl->db, functor, term, arity);

	if (r != NULL)
		return r;

	sl_start(&l->ns);
	const char *key;

	while ((key = sl_next(&l->ns, NULL)) != NULL) {
		module *db = NULL;

		if (!sl_get(&l->pl->mods, key, (void **)&db))
			continue;

		if ((term->bifptr = get_bifarity(l, functor, arity)->bifptr) != NULL) {
			term->flags |= FLAG_BUILTIN;
			return r;
		}

		if (!db)
			continue;

		r = xref_term2(l, db, functor, term, arity);

		if (r != NULL)
			break;
	}

	return r;
}

static int xref_body(lexer *l, node *term, const char *head_functor, int is_last)
{
	// printf("*** XREF_BODY "); term_print(l->pl, NULL, term, 1); printf("\n");

	const char *functor = NULL;

	if (is_compound(term)) {
		node *tmp = term_first(term);

		for (node *n = term_next(tmp); n != NULL; n = term_next(n))
			xref_body(l, n, head_functor, is_last);

		if (is_atom(tmp) && !(term->flags & FLAG_QUOTED) && !(tmp->flags & FLAG_BUILTIN)) {
			term->match = xref_term(l, tmp, term_arity(term));
			functor = VAL_S(tmp);
		}
	}
	else if (is_atom(term) && !(term->flags & FLAG_QUOTED) && !(term->flags & FLAG_BUILTIN)) {
		term->match = xref_term(l, term, 0);
		functor = VAL_S(term);
	}
	else
		return 0;

	if (functor && (term_next(term) == NULL) && is_last) {
		if (!strcmp(functor, head_functor))
			term->flags |= FLAG_TAILRECURSIVE;
	}

	return 1;
}

int xref_clause(lexer *l, node *term)
{
	//printf("*** XREF_RULE "); term_print(l->pl, NULL, term, 1); printf("\n");

	// Cross-reference all body functors with the index, and
	// point to the actual index rule to allow for assert etc.

	node *head = term_firstarg(term);
	node *body = term_next(head);

	if (!head)
		return 0;

	const char *head_functor = "";

#ifndef ISO_ONLY
	int arity = 0;
#endif

	if (is_compound(head)) {
		head_functor = VAL_S(term_first(head));
#ifndef ISO_ONLY
		arity = term_arity(head);
#endif
	}
	else if (is_atom(head))
		head_functor = VAL_S(head);

#ifndef ISO_ONLY
	char tmpbuf[FUNCTOR_SIZE + 10];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s/%d", head_functor, arity);

	if (sl_get(&l->db->exports, tmpbuf, NULL)) {
		node *s = make_compound();
		s->flags |= FLAG_BUILTIN | FLAG_HIDDEN;
		s->bifptr = &bif_xtra_enter;

		//if (body->bifptr == bif_iso_cut)
		//	s->flags |= FLAG_ISCUT;

		node *tmp = make_const_atom("enter", 0);
		tmp->flags |= FLAG_BUILTIN;
		tmp->bifptr = &bif_xtra_enter;
		term_append(s, tmp);
		tmp = make_atom(strdup(l->db->name), 0);
		term_append(s, tmp);
		term_insert_after(term, head, s);
	}
#endif

	int is_last = !term_next(term);

	for (node *n = body; n != NULL; n = term_next(n))
		xref_body(l, n, head_functor, is_last);

	return 1;
}

static void add_clauses(lexer *l, int internal)
{
	list tmp_l;
	list_init(&tmp_l);
	DBLOCK(l->db);
	node *tmp;
	node *n;

	while ((n = NLIST_POP_FRONT(&l->val_l)) != NULL) {
		if (!is_clause(n))
			continue;

		if (internal)
			n->flags |= FLAG_HIDDEN;

		int persist;

		if (!assertz_index(l, n, -internal, &persist, 1)) {
			term_destroy(n);
			continue;
		}

		tmp = term_make();
		tmp->n1 = n;
		NLIST_PUSH_BACK(&tmp_l, tmp);
	}

	while ((tmp = NLIST_POP_FRONT(&tmp_l)) != NULL) {
		n = tmp->n1;
		xref_clause(l, n);

		// This flattens conjunctions...

		node *head = term_firstarg(n);
		node *body = term_next(head);

		while (is_compound(body) && (l->pl->optimize > 0)) {
			if (strcmp(term_functor(body), ","))
				break;

			term_remove(n, body);
			node *fun = NLIST_POP_FRONT(&body->val_l);
			node *lhs = NLIST_POP_FRONT(&body->val_l);
			node *rhs = NLIST_POP_FRONT(&body->val_l);
			term_heapcheck(fun);
			term_heapcheck(body);
			term_append(n, lhs);
			term_append(n, rhs);
			body = rhs;
		}

		term_heapcheck(tmp);
	}

	DBUNLOCK(l->db);
}

static int trealla_make_rule(trealla *self, const char *src)
{
	lexer l;
	lexer_init(&l, self);
	lexer_parse(&l, l.r, src, NULL);
	int ok = !l.error;

	if (l.error) {
		node *r = NLIST_FRONT(&l.val_l);
		term_heapcheck(r);
		printf("ERROR: error make_rule\n");
	} else {
		add_clauses(&l, 1);
	}

	lexer_done(&l);
	return ok;
}

int query_parse(tpl_query *self, const char *src) { return query_parse_file(self, src, NULL); }

int query_parse_file(tpl_query *self, const char *src, FILE *fp)
{
	self->lex->fp = fp;

	if (self->lex->val_l.cnt) {
		node *r = NLIST_POP_FRONT(&self->lex->val_l);
		term_heapcheck(r);
		NLIST_INIT(&self->lex->val_l);
	}

	char *line = (char *)malloc(strlen(src) + 10);
	sprintf(line, "?- %s", src);
	src = line;

	while ((src = lexer_parse(self->lex, self->lex->r, src, &line)) != NULL)
		if (self->lex->finalized || self->lex->error)
			break;

	free(line);

	if (!self->lex->finalized || self->lex->error) {
		if (!self->lex->finalized && !self->lex->error)
			printf("ERROR: parse -> %s\n", (src ? src : "end_of_file"));

		node *n = NLIST_FRONT(&self->lex->val_l);
		term_heapcheck(n);
		term_heapcheck(self->lex->r);
		return 0;
	}

	if (!xref_clause(self->lex, NLIST_FRONT(&self->lex->val_l)) || self->lex->error) {
		self->halt = 0;
		return 0;
	}

	self->frame_size = self->lex->vars;
	begin_query(self, NLIST_FRONT(&self->lex->val_l));
	return 1;
}

void trace(tpl_query *q, int fail, int leave)
{
	if (!q->curr_term)
		return;

	if (q->curr_term->flags & FLAG_HIDDEN)
		return;

	const int save_context = q->latest_context;
	q->latest_context = q->curr_frame;
	size_t dstlen = PRINTBUF_SIZE;
	char *dstbuf = (char *)malloc(dstlen + 1);
	char *dst = dstbuf;
	dst += sprintf(dst, "%s", fail ? "Fail:" : q->retry ? "Redo:" : leave ? "Exit:" : "Call:");
	dst += sprintf(dst, "%s", q->parent ? " ... " : " ");
	term_sprint2(&dstbuf, &dstlen, &dst, q->pl, q, q->curr_term, 1);
	q->latest_context = save_context;

#if 1
	dst = dstbuf + 1024;
	*dst++ = '.';
	*dst++ = '.';
	*dst++ = '.';
	*dst = '\0';
#endif

	fprintf(stdout, "%s\n", dstbuf);
	fflush(stdout);
	free(dstbuf);
}

int query_run(tpl_query *self)
{
	self->ok = 1;
	self->started = gettimeofday_usec();
	self->env_point = self->curr_frame + self->frame_size;
	self->envs_used = self->env_point;
	prepare_frame(self, self->frame_size);
	run_me(self);
	self->elapsed = gettimeofday_usec() - self->started;

	if (!self->is_yielded && self->halt) {
		if (!self->pl->abort && (self->halt > ABORT_HALT))
			printf("ERROR: ERROR %s\n", self->halt_s ? self->halt_s : "ABORT");
		else if (!self->pl->abort && (self->halt == ABORT_HALT))
			printf("Halted\n");

		self->pl->halt_code = self->halt_code;
		self->pl->halt = self->halt;
		self->ok = 0;
	}

	return self->ok;
}

void query_reset(tpl_query *self)
{
	env *e = &self->envs[0];

	for (size_t i = 0; i < self->envs_used; i++, e++) {
		term_heapcheck(e->term);
		e->term = NULL;
		e->context = 0;
	}

	node *r = NLIST_POP_FRONT(&self->lex->val_l);
	term_heapcheck(r);
	lexer_done(self->lex);
	lexer_init(self->lex, self->pl);
	self->lex->db = self->curr_db;
}

int query_continue(tpl_query *self)
{
	if (self->halt)
		return 0;

	if (!retry_me(self))
		return 0;

	self->ok = 1;
	self->started = gettimeofday_usec();
	run_me(self);
	self->elapsed = gettimeofday_usec() - self->started;

	if (!self->is_yielded && self->halt) {
		if (!self->pl->abort && (self->halt > ABORT_ABORT))
			printf("ERROR: %s\n", self->halt_s ? self->halt_s : "ABORT");

		self->ok = 0;
	}

	return self->ok;
}

int query_inline(tpl_query *self)
{
	self->ok = 1;
	run_me(self);
	return self->ok;
}

double query_elapsed(tpl_query *self)
{
	if (self)
		return (double)self->elapsed / 1000.0 / 1000.0;
	else
		return 0.0;
}
void query_abort(tpl_query *self)
{
	self->halt_s = (char *)strdup("ABORT_INTERRUPTED");
	self->halt = 1;
}

double query_get_float(tpl_query *self, unsigned idx)
{
	env *e = get_env(self, idx);

	if (!e->term)
		return 0.0;

	return e->term->val_f;
}

long long query_get_integer(tpl_query *self, unsigned idx)
{
	env *e = get_env(self, idx);

	if (!e->term)
		return 0;

	return e->term->val_i;
}

char *query_get_text(tpl_query *self, unsigned idx)
{
	if (idx >= self->frame_size)
		return strdup("ERROR");

	env *e = get_env(self, idx);

	if (!e->term)
		return strdup("_");

	char tmpbuf[PRINTBUF_SIZE];
	term_sprint(tmpbuf, sizeof(tmpbuf), self->pl, self, e->term, 0);
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

int query_get_haltcode(tpl_query *self)
{
	return self->halt_code;
}

static void collect_vars(tpl_query *q, node *n)
{
	if (is_compound(n)) {
		for (n = term_first(n); n != NULL; n = term_next(n))
			collect_vars(q, n);

		return;
	}

	if (!is_var(n))
		return;

	if (sl_get(q->d, (char *)VAL_S(n), NULL))
		return;

	sl_set(q->d, (char *)VAL_S(n), n);
}

void query_dump(tpl_query *self)
{
	skiplist vars;
	sl_init(&vars, 0, &strcmp, NULL);
	self->d = &vars;
	collect_vars(self, NLIST_FRONT(&self->lex->val_l));
	self->d = NULL;
	sl_start(&vars);
	int any = 0;
	node *n;

	if (!sl_count(&vars)) {
		if (self->nv.flags == TYPE_INTEGER)
			printf(" %lld", (long long)self->nv.val_i);
		else if (self->nv.flags == TYPE_FLOAT)
			printf(" %.*g", DBL_DIG, (double)self->nv.val_f);
	} else {
		while (sl_next(&vars, (void **)&n) != NULL) {
			if (is_anon(n))
				continue;

			char tmpbuf[PRINTBUF_SIZE];
			self->latest_context = FUDGE_FACTOR;
			term_sprint(tmpbuf, sizeof(tmpbuf), self->pl, self, n, 0);
			printf(" %s: %s\n", VAL_S(n), tmpbuf);
			any++;
		}
	}

	sl_clear(&vars, NULL);
}

static tpl_query *trealla_create_query2(trealla *pl, tpl_query *q);

tpl_query *query_create_subquery(tpl_query *self)
{
	tpl_query *q = trealla_create_query2(self->pl, self);

	if (!q)
		return NULL;

	env *e_to = q->envs + q->curr_frame;

	for (size_t i = 0; i < self->frame_size; i++, e_to++) {
		const env *e_from = get_env(self, self->curr_frame + i);
		*e_to = *e_from;

		if (e_from->term)
			e_from->term->refcnt++;
	}

	q->frame_size = self->frame_size;
	return q;
}

#ifndef ISO_ONLY
tpl_query *query_create_proc(tpl_query *self)
{
	tpl_query *q = query_create_subquery(self);

	if (!q)
		return NULL;

	q->refcnt++;

	if (self->curr_pid) {
		if (!--self->curr_pid->refcnt)
			query_destroy(self->curr_pid);
	}

	self->curr_pid = q;
	return q;
}
#endif

void query_stats(tpl_query *self)
{
#ifdef DEBUG
	if (!g_enqueues) {
		printf("Heap-used: %llu, Backtracks: %llu, Executes: %llu, Reexecutes: %llu\n",
				(long long unsigned)g_heap_used, (long long unsigned)g_backtracks,
				(long long unsigned)g_executes, (long long unsigned)g_reexecutes);
		printf("Cuts: %llu, Choicepoints: %llu\n",
				(long long unsigned)g_cuts, (long long unsigned)g_choicepoints);
		printf("Max Env depth: %llu, Choice depth: %llu, Trail depth: %llu\n",
				(long long unsigned)self->envs_used,
				(long long unsigned)self->choices_used,
				(long long unsigned)self->trails_used);
		printf("System calls: %llu, User: %llu\n",
				(long long unsigned)g_s_resolves, (long long unsigned)g_u_resolves);
	}
#ifndef ISO_ONLY
	else {
		printf("System calls: %llu, User: %llu\n",
			(long long unsigned)g_s_resolves, (long long unsigned)g_u_resolves);

		if (g_enqueues)
			printf("Process msgs: %llu, Busy: %.1f%%, Reschedules: %.1f%%\n",
				(long long unsigned)g_enqueues,
				(100.0 * g_busy) / g_enqueues, (100.0 * g_rescheds) / g_enqueues);
	}
#endif
#endif
}

void query_destroy(tpl_query *self)
{
#ifndef ISO_ONLY
	if (self->is_running) {
		printf("*** abort: running\n");
		// abort();
		return;
	}

	if (self->is_dead) {
		free(self);
		return;
	}

	if (self->kvs) {
		kvs_done(self->kvs);
		free(self->kvs);
	}

	node *tmp = NLIST_FRONT(&self->queue);
	term_heapcheck(tmp);

	if (self->name) {
		PIDLOCK(self->pl);
		sl_del(&self->pl->names, (char *)self->name, NULL);
		PIDUNLOCK(self->pl);
	}

	if (self->linked && self->halt)
		process_error(self);

	if (self->curr_pid) {
		if (!--self->curr_pid->refcnt)
			query_destroy(self->curr_pid);
	}

	if (self->name)
		free(self->name);
#endif

	if (self->subq)
		query_destroy(self->subq);

	if (self->did_getc) {
		while ((getc(stdin) != '\n') && !feof(stdin))
			;
	}

	env *e = &self->envs[0];

	for (size_t i = 0; i < self->envs_possible; i++, e++)
		term_heapcheck(e->term);

	if (!self->parent) {
		node *r = NLIST_FRONT(&self->lex->val_l);
		term_heapcheck(r);
	}

	if (self->halt_s)
		free(self->halt_s);

	if (!self->def_choice)
		free(self->choices);

	if (!self->def_env)
		free(self->envs);

	if (!self->def_trail)
		free(self->trails);

#ifndef ISO_ONLY
	if (--self->refcnt > 0) {
		self->is_dead = 1;
		return;
	}
#endif

	if (!self->parent) {
		lexer_done(self->lex);
		free(self->lex);
	}

	free(self);
}

int trealla_consult_fp(trealla *self, FILE *fp)
{
	lexer l;
	lexer_init(&l, self);
	l.consult = 1;

	if (lexer_consult_fp(&l, fp)) {
		add_clauses(&l, 0);

		if (l.init && !l.error) {
			if (!(l.error = trealla_run_query(self, l.init)))
				self->abort = self->halt_code > 0;
		}
	}

	int ok = !l.error;
	lexer_done(&l);
	return ok;
}

int trealla_consult_file(trealla *self, const char *filename)
{
	lexer l;
	lexer_init(&l, self);
	l.consult = 1;

	if (lexer_consult_file(&l, filename)) {
		add_clauses(&l, 0);

		if (l.init && !l.error) {
			if (!(l.error = trealla_run_query(self, l.init)))
				self->abort = self->halt_code > 0;
		}
	}

	int ok = !l.error;
	lexer_done(&l);
	return ok;
}

int trealla_consult_text(trealla *self, const char *src, const char *filename)
{
	if (!*src || !*filename)
		return 0;

	if (strlen_utf8(filename) >= FUNCTOR_LEN)
		return 0;

	char *line = strdup(src);
	char *dst = line;

	lexer l;
	lexer_init(&l, self);
	l.name = strdup(filename);
	l.consult = 1;

	while (*src) {
		if (*src == '%') {
			while (*src && (*src != '\n'))
				src++;
		}

		// Certain directives must currently be parsed on a line by themselves,
		// hence this kludge, everything else can be parsed in one chunk...

		if ((*src == '.') && (src[1] == '\n')) {
			*dst++ = *src++;
			*dst++ = *src;
		}
		else if ((*src != '\n') || src[1]) {
			*dst++ = *src++;
			continue;
		}

		src++;
		*dst = '\0';
		// printf("*** [ %s ]\n", line);
		lexer_parse(&l, l.r, line, &line);

		if (l.error) {
			printf("ERROR: consult '%s'\n>>> Near line=%d\n>>> %s\n", filename, l.line_nbr, line);
			lexer_done(&l);
			return 0;
		}

		dst = line;
	}

	free(line);
	add_clauses(&l, 0);

	if (l.init && !l.error) {
		if (!trealla_run_query(self, l.init))
			self->abort = self->halt_code > 0;
	}

	lexer_done(&l);
	return 1;
}

int trealla_deconsult(trealla *self, const char *filename)
{
	SYSLOCK(self);
	sl_start(&self->mods);
	const char *key;
	module *db = NULL;

	while ((key = sl_next(&self->mods, (void **)&db)) != NULL) {
		if (!strcmp(db->filename, filename)) {
			sl_del(&self->mods, key, NULL);
			break;
		}
	}

	DBLOCK(db);
	sl_start(&db->rules);
	rule *r;

	while ((key = sl_next(&db->rules, (void **)&r)) != NULL) {
		// FIXME: just deleting rules is too dangerous. ???
		// sl_del(&db->rules, key, NULL);
		// rule_done(r);
	}

	DBUNLOCK(db);
	//db_free(db);
	SYSUNLOCK(self);
	return 1;
}

int trealla_run_query(trealla *self, const char *src)
{
	tpl_query *q = trealla_create_query(self);

	if (!q)
		return 0;

	int ok = query_parse(q, src);

	if (!q->lex->error)
		ok = query_run(q);

	node *r = NLIST_POP_FRONT(&q->lex->val_l);
	term_heapcheck(r);
	query_destroy(q);
	return ok;
}

static tpl_query *trealla_create_query2(trealla *self, tpl_query *parent)
{
	tpl_query *q = calloc(1, sizeof(tpl_query));

	if (!q)
		return NULL;

	q->parent = parent;
	q->pl = self;
	q->optimize = self->optimize;
	q->trace = self->trace;

	if (parent) {
		q->optimize = parent->optimize;
		q->trace = parent->trace;
		q->curr_db = parent->curr_db;
		q->lex = parent->lex;
		q->curr_stdin = parent->curr_stdin;
		q->curr_stdout = parent->curr_stdout;
		q->curr_db = parent->curr_db;
	} else {
		q->lex = calloc(1, sizeof(lexer));
		lexer_init(q->lex, self);
		q->curr_stdin = stdin;
		q->curr_stdout = stdout;
		q->curr_db = &self->db;
	}

#ifndef ISO_ONLY
	if (q->parent)
		parent->refcnt++;
	else
		q->name = strdup("default");

	q->refcnt = 1;
	q->curr_pid = parent;
	q->tmo_msecs = -1;
#endif

	q->def_trail = q->def_choice = q->def_env = 1;
	q->choices_possible = DEF_CHOICES_BYTES / sizeof(choice);
	q->envs_possible = DEF_ENVS_BYTES / sizeof(env);
	q->trails_possible = DEF_TRAILS_BYTES / sizeof(trail);
	q->choices = q->choice_stack;
	q->envs = q->env_stack;
	q->trails = q->trail_stack;
	q->curr_frame = FUDGE_FACTOR;
	return q;
}

tpl_query *trealla_create_query(trealla *self)
{
	 return trealla_create_query2(self, NULL);
}

void trealla_quiet(trealla *self, int mode)
{
	 self->quiet = mode;
}

void trealla_trace(trealla *self, int mode)
{
	self->trace = mode;
}

void trealla_optimize(trealla *self, int mode)
{
	self->optimize = mode;
}

int trealla_is_abort(trealla *self)
{
	return self->abort;
}

int trealla_is_halt(trealla *self)
{
	return self->halt == ABORT_HALT;
}

int trealla_get_haltcode(trealla *self)
{
	return self->halt_code;
}

char *trealla_find_library(const char *name)
{
#ifndef ISO_ONLY
	library *lib = g_libs;

	while (lib->name != NULL) {
		if (!strcmp(lib->name, name)) {
			return strndup((const char *)lib->start, (lib->end-lib->start));
		}

		lib++;
	}
#endif
	return NULL;
}

#ifndef ISO_ONLY
static int tmocmp(const char *k1, const char *k2)
{
	const tpl_query *q1 = (const tpl_query *)k1;
	const tpl_query *q2 = (const tpl_query *)k2;

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
	if (!name)
		name = "default";

	if (!name[0] || (strlen_utf8(name) >= FUNCTOR_LEN))
		name = "default";

	trealla *pl = calloc(1, sizeof(trealla));

	if (!pl)
		return NULL;

	g_instances++;
	pl->optimize = 0;
	pl->tty = isatty(0);
	db_init(&pl->db, pl, name, "default");
	sl_init(&pl->mods, 0, &strcmp, &free);
	static int first_time = 1;

	if (first_time) {
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

	while (g_bifs[idx].functor != NULL) {
		pl->keywords[idx] = g_bifs[idx].functor;
		idx++;
	}

	pl->keywords[idx++] = "member";
	pl->keywords[idx++] = "select";
	pl->keywords[idx++] = "efface";
	pl->keywords[idx++] = "reverse";
	pl->keywords[idx++] = "append";
	pl->keywords[idx++] = "find";
	history_keywords((const char **)pl->keywords);

#ifndef ISO_ONLY
	sl_init(&pl->idle, 0, &tmocmp, NULL);
	sl_init(&pl->names, 0, &strcmp, &free);
	sl_set(&pl->mods, strdup("sys"), NULL);
	sl_set(&pl->mods, strdup("proc"), NULL);
	sl_set(&pl->mods, strdup("dbs"), NULL);
	sl_set(&pl->mods, strdup("net"), NULL);
	sl_set(&pl->mods, strdup("http"), NULL);
	sl_set(&pl->mods, strdup("ws"), NULL);
	sl_set(&pl->mods, strdup("linda"), NULL);

	pl->pid_guard = lock_create();
	pl->dbs_guard = lock_create();
#endif

	pl->flag_unknown = 1;
	pl->flag_char_conversion = 1;
	pl->flag_double_quotes = 1;
	pl->flag_character_escapes = 1;

	trealla_make_rule(pl, "stream_property(S,type(P)) :- stream_property_type(S,P).");
	trealla_make_rule(pl, "stream_property(S,mode(P)) :- stream_property_mode(S,P).");
	trealla_make_rule(pl, "stream_property(S,position(P)) :- stream_property_position(S,P).");
	trealla_make_rule(pl, "stream_property(S,file_name(F)) :- stream_property_file_name(S,F).");

#ifndef ISO_ONLY
	trealla_make_rule(pl, "member(X,X) :- var(X),!,fail.");
	trealla_make_rule(pl, "member(X,[X|_]).");
	trealla_make_rule(pl, "member(X,[_|T]) :- member(X,T).");
	trealla_make_rule(pl, "select(X,[X|T],T).");
	trealla_make_rule(pl, "select(X,[H|T],[H|Rest]) :- select(X,T,Rest).");
	trealla_make_rule(pl, "efface([],L,L) :- !.");
	trealla_make_rule(pl, "efface([H|T],L,L2) :- selectall(H,L,L1),efface(T,L1,L2).");
	trealla_make_rule(pl, "revzap([],L,L) :- !.");
	trealla_make_rule(pl, "revzap([X|L],L2,L3) :- revzap(L,[X|L2],L3).");
	trealla_make_rule(pl, "reverse(L1,L2) :- revzap(L1,[],L2).");
	trealla_make_rule(pl, "append([],L,L).");
	trealla_make_rule(pl, "append([H|T],L2,[H|L3]) :- append(T,L2,L3).");
	trealla_make_rule(pl, "find(N,[],X) :- !.");
	trealla_make_rule(pl, "find(1,[H|_],H) :- !.");
	trealla_make_rule(pl, "find(N,[_|T],X) :- N1 is N-1,find(N1,T,X).");
#endif

	return pl;
}

void trealla_destroy(trealla *self)
{
	if (!self)
		return;

	sl_done(&self->mods, &db_free);

#ifndef ISO_ONLY
	sl_done(&self->idle, NULL);
	sl_done(&self->names, NULL);
	lock_destroy(self->pid_guard);
	lock_destroy(self->dbs_guard);

	if (self->tp)
		tpool_destroy(self->tp);

	if (self->h) {
		handler_shutdown(self->h);
		handler_destroy(self->h);
	}
#endif

	db_done(&self->db);
	g_instances--;

#ifdef DEBUG
	if (!g_instances && (g_allocs > 0) && !self->quiet)
		printf("DEBUG: orphaned=%u\n", (unsigned)g_allocs);
#endif

	free(self);
}
