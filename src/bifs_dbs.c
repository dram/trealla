#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#define fsync _commit
#define snprintf _snprintf
#else
#include <unistd.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

#define ffsync(fp) fsync(fileno(fp))

int g_dbs_merge = 0;

static int dbs_merge(module *db)
{
	if (db->fp)
	{
		fflush(db->fp);
		ffsync(db->fp);
		fclose(db->fp);
		db->fp = NULL;
	}

	mkdir("db", 0777);
	char pathname[1024];
	sprintf(pathname, "db/%s", db->name);
	mkdir(pathname, 0777);
	char filename[1024];
	snprintf(filename, sizeof(filename), "db/%s/%s.%s", db->name, db->name, "tmp.dbs");
	db->fp = fopen(filename, "wb");
	if (db->fp == NULL) return 0;
	printf("INFO: Saving '%s' ... ", filename);
	fflush(stdout);
	size_t buflen = 1024*64;					// expandable
	char *dstbuf = (char*)malloc(buflen+1);
	size_t any = 0;

	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void**)&r) != NULL)
	{
		for (node *n = NLIST_FRONT(&r->clauses); n; n = NLIST_NEXT(n))
		{
			if (!(n->flags & FLAG_DBS_ASSERTA) && !(n->flags & FLAG_DBS_ASSERTZ))
				continue;

			n->flags &= ~FLAG_DBS_ASSERTA;
			n->flags |= FLAG_DBS_ASSERTZ;
			dbs_save_node(db, &dstbuf, &buflen, n);
			any++;
		}
	}

	fflush(db->fp);
	ffsync(db->fp);
	fclose(db->fp);
	db->fp = NULL;
	free(dstbuf);
	printf("Saved %llu items\n", (unsigned long long)any);

	char tmpname[1024], tmpname2[1024];
	snprintf(tmpname, sizeof(tmpname), "db/%s/%s.new.dbs", db->name, db->name);
	rename(filename, tmpname);
	printf("DEBUG: Renamed '%s' -> '%s'\n", filename, tmpname);
	strcpy(filename, tmpname);
	snprintf(tmpname, sizeof(tmpname), "db/%s/%s.log.dbs", db->name, db->name);
	snprintf(tmpname2, sizeof(tmpname2), "db/%s/%s.%016llX.dbs", db->name, db->name, (long long)time(NULL));
	rename(tmpname, tmpname2);
	printf("DEBUG: Renamed '%s' -> '%s'\n", tmpname, tmpname2);
	rename(filename, tmpname);
	printf("DEBUG: Renamed '%s' -> '%s'\n", filename, tmpname);
	return 1;
}

static void dbs_load_file(module *db, const char *filename, int tail)
{
	printf("INFO: Loading '%s' ... ", filename);
	fflush(stdout);
	tpl_query *q = trealla_create_query(db->pl);
	if (!q) return;
	q->curr_db = db;
	lexer lex;
	lexer_init(&lex, db->pl);
	lex.db = db;
	q->lex = &lex;
	unsigned line_nbr = 0;
	size_t any = 0;
	char *line;
	db->loading = 1;

	do
	{
		while ((line = trealla_readline(db->fp)) != NULL)
		{
			line_nbr++;

			if (!isalpha(line[0]) && (line[0] != '_'))
				continue;

			if (!query_parse(q, line))
				printf("ERROR: '%s'\n", filename);
			else
			{
				query_run(q);
				if (q->ok) any++;
			}

			query_reset(q);
			free(line);
		}

		if (tail)
		{
			clearerr(db->fp);
			sleep(1);
		}
	}
	 while (tail);

	query_destroy(q);
	db->loading = 0;
	printf(" Loaded %llu updates\n", (unsigned long long)any);
}

static void dbs_load(module *db, int tail)
{
	mkdir("db", 0777);
	char pathname[1024];
	sprintf(pathname, "db/%s", db->name);
	mkdir(pathname, 0777);
	char filename[1024];
	snprintf(filename, sizeof(filename), "db/%s/%s.tmp.dbs", db->name, db->name);
	db->fp = fopen(filename, "rb");
	if (db->fp != NULL)
	{
		fclose(db->fp);
		db->fp = NULL;
		remove(filename);
	}

	snprintf(filename, sizeof(filename), "db/%s/%s.new.dbs", db->name, db->name);
	db->fp = fopen(filename, "rb");

	if (db->fp != NULL)
	{
		fclose(db->fp);
		db->fp = NULL;
		char tmpname[1024], tmpname2[1024];
		snprintf(tmpname, sizeof(tmpname), "db/%s/%s.log.dbs", db->name, db->name);
		snprintf(tmpname2, sizeof(tmpname2), "db/%s/%s.%016llX.dbs", db->name, db->name, (long long)time(NULL));
		rename(tmpname, tmpname2);
		rename(filename, tmpname);
	}

	// Load the transaction stream...

	snprintf(filename, sizeof(filename), "db/%s/%s.log.dbs", db->name, db->name);
	db->fp = fopen(filename, "rb");

	if (db->fp != NULL)
	{
		dbs_load_file(db, filename, tail);
		fclose(db->fp);
		db->fp = NULL;
	}

	if (g_dbs_merge)
	{
		g_dbs_merge = 0;
		dbs_merge(db);
	}
}

nbr_t dbs_get_fpos(module *db)
{
	return ftell(db->fp);
}

void dbs_save_node(module *db, char **dstbuf, size_t *buflen, node *n)
{
	if (!db->fp)
	{
		mkdir("db", 0777);
		char pathname[1024];
		sprintf(pathname, "db/%s", db->name);
		mkdir(pathname, 0777);
		char filename[1024];
		snprintf(filename, sizeof(filename), "db/%s/%s.log.dbs", db->name, db->name);
		db->fp = fopen(filename, "ab");
		assert(db->fp != NULL);
	}

	char *dst = *dstbuf;
	*buflen -= 20;						// a bit of leeway

	if (n->flags & FLAG_DBS_RETRACT)
	{
		dst += snprintf(dst, *buflen, "r_(");
		dst += sprint2_term(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (n->flags & FLAG_DBS_ASSERTZ)
	{
		dst += snprintf(dst, *buflen, "z_(");
		dst += sprint2_term(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (n->flags & FLAG_DBS_ASSERTA)
	{
		dst += snprintf(dst, *buflen, "a_(");
		dst += sprint2_term(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else
	{
		dst += sprint2_term(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
	}

	*dst++ = db->in_tran ?',':'.';
	if (!db->in_tran) *dst++ = '\n';
	*dst = '\0';
	fwrite(*dstbuf, 1, dst-*dstbuf, db->fp);
	if (!db->in_tran) fflush(db->fp);
}

static int dbs_end(tpl_query *q, int do_sync)
{
	if (!q->curr_db->in_tran)
		return 0;

	trust_me(q);

	node *n = NULL;
	int any = 0;

	while ((n = NLIST_POP_FRONT(&q->curr_db->tran_queue)) != NULL)
	{
		node *tmp = n->orig;

		if (tmp->flags & FLAG_DBS_RETRACT)
			any += bif_retract(q, tmp);
		else if (tmp->flags & FLAG_DBS_ASSERTZ)
			any += bif_assertz(q, tmp);
		else if (tmp->flags & FLAG_DBS_ASSERTA)
			any += bif_asserta(q, tmp);

		FREE(n);
	}

	if (any)
	{
		char tmpbuf[256];
		strcpy(tmpbuf, "true.\n");
		fwrite(tmpbuf, 1, strlen(tmpbuf), q->curr_db->fp);
		fflush(q->curr_db->fp);

		if (do_sync)
			ffsync(q->curr_db->fp);
	}

	q->curr_db->in_tran = 0;
	DBUNLOCK(q->curr_db);
	return 1;
}

static int bif_dbs_end0(tpl_query *q)
{
	dbs_end(q, 1);
	return 1;
}

static int bif_dbs_end1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	int do_sync;

	if (is_integer(term1))
		do_sync = term1->val_i != 0;
	else
		do_sync = !strcmp(term1->val_s, "true");

	dbs_end(q, do_sync);
	return 1;
}

static void dbs_bail(tpl_query *q)
{
	node *n = NULL;

	while ((n = NLIST_POP_FRONT(&q->curr_db->tran_queue)) != NULL)
	{
		node *tmp = n->orig;
		term_heapcheck(tmp);
		FREE(n);
	}

	q->curr_db->in_tran = 0;
	DBUNLOCK(q->curr_db);
}

int bif_dbs_begin(tpl_query *q)
{
	if (q->retry)
	{
		dbs_bail(q);
		return 0;
	}

	allocate_frame(q);
	try_me_nofollow(q);
	DBLOCK(q->curr_db);
	q->curr_db->in_tran = 1;
	return 1;
}

static int bif_dbs_log(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	size_t buflen = PRINTBUF_SIZE;					// expandable
	char *dstbuf = (char*)malloc(buflen+1);
	node *n = clone_term(q,term1);
	dbs_save_node(q->curr_db, &dstbuf, &buflen, n);
	term_heapcheck(n);
	free(dstbuf);
	return 1;
}

int bif_dbs_load(tpl_query *q)
{
	dbs_load(q->curr_db, 0);
	return 1;
}

int bif_dbs_tail(tpl_query *q)
{
	dbs_load(q->curr_db, 1);
	return 1;
}

void bifs_load_dbs(void)
{
	DEFINE_BIF("dbs:load", 0, bif_dbs_load);
	DEFINE_BIF("dbs:tail", 0, bif_dbs_tail);
	DEFINE_BIF("dbs:log", 1, bif_dbs_log);
	DEFINE_BIF("dbs:begin", 0, bif_dbs_begin);
	DEFINE_BIF("dbs:end", 0, bif_dbs_end0);
	DEFINE_BIF("dbs:end", 1, bif_dbs_end1);
}

