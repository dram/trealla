#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#define fsync _commit
#define snprintf _snprintf
#define fseeko _fseeki64
#define ftello _ftelli64
#define msleep Sleep
#else
#include <unistd.h>
#define msleep(ms) { struct timespec tv; tv.tv_sec = (ms)/1000; tv.tv_nsec = ((ms)%1000) * 1000 * 1000; nanosleep(&tv, &tv); }
#endif

#include "trealla.h"

#include "bifs.h"
#include "internal.h"
#include "jela.h"

#define ffsync(fp) fsync(fileno(fp))

int g_dbs_merge = 0;
const char *g_dbdir = ".";

static int dbs_merge(module *db)
{
	char pathname[10224], filename[1024 * 2];
	sprintf(pathname, "%s/db", g_dbdir);
	mkdir(filename, 0777);
	sprintf(pathname, "%s/db/%s", g_dbdir, db->name);
	mkdir(pathname, 0777);
	snprintf(filename, sizeof(filename), "%s/%s.%s", pathname, db->name, "tmp.dbs");
	FILE *fp = fopen(filename, "wb");

	if (!fp)
		return 0;

	if (!db->pl->quiet)
		printf("INFO: Saving '%s' ... ", filename);

	fflush(stdout);
	size_t buflen = 1024 * 64; // expandable
	char *dstbuf = (char *)malloc(buflen + 1);
	size_t any = 0;
	db->merging = 1;
	sl_start(&db->rules);
	rule *r;

	while (sl_next(&db->rules, (void **)&r) != NULL) {
		for (node *n = NLIST_FRONT(&r->val_l); n; n = term_next(n)) {
			if (!(n->flags & FLAG_DBS_ASSERTA) && !(n->flags & FLAG_DBS_ASSERTZ))
				continue;

			n->flags &= ~FLAG_DBS_ASSERTA;
			n->flags |= FLAG_DBS_ASSERTZ;
			dbs_save_node(db, fp, &dstbuf, &buflen, n, 0);
			any++;
		}
	}

	db->merging = 0;
	fflush(fp);
	ffsync(fp);
	fclose(fp);
	free(dstbuf);
	printf("Saved %llu items\n", (unsigned long long)any);

	char tmpname[1024 * 2], tmpname2[1024 * 2];
	snprintf(tmpname, sizeof(tmpname), "%s/%s.new.dbs", pathname, db->name);
	rename(filename, tmpname);
	printf("DEBUG: Renamed '%s' -> '%s'\n", filename, tmpname);
	strcpy(filename, tmpname);
	snprintf(tmpname, sizeof(tmpname), "%s/%s.log.dbs", pathname, db->name);
	snprintf(tmpname2, sizeof(tmpname2), "%s/%s.%016llX.dbs", pathname, db->name, (long long)time(NULL));
	rename(tmpname, tmpname2);
	printf("DEBUG: Renamed '%s' -> '%s'\n", tmpname, tmpname2);
	rename(filename, tmpname);
	printf("DEBUG: Renamed '%s' -> '%s'\n", filename, tmpname);
	return 1;
}

void dbs_save_node(module *db, FILE *fp, char **dstbuf, size_t *buflen, node *n, int in_tran)
{
	node *save_n = n;
	char *dst = *dstbuf;
	*buflen -= 20; // a bit of leeway

	node *term = term_first(n);
	node *head = term_next(term);
	node *save_head = head;

	if (is_storage(n) && db->merging) {
		node *tmp_arg1 = term_firstarg(head);
		node *tmp_rest = term_next(tmp_arg1);
		n = head = dbs_read_entry(db, tmp_rest->val_i);
	}

	if (save_n->flags & FLAG_DBS_RETRACT) {
		dst += snprintf(dst, *buflen, "r_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (save_n->flags & FLAG_DBS_ASSERTZ) {
		dst += snprintf(dst, *buflen, "z_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (save_n->flags & FLAG_DBS_ASSERTA) {
		dst += snprintf(dst, *buflen, "a_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
		*dst++ = ')';
	}
	else if (save_n->bifptr == bif_iso_retract) {
		dst += snprintf(dst, *buflen, "r_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, term_firstarg(n), 1);
		*dst++ = ')';
	}
	else if (save_n->bifptr == bif_iso_assertz) {
		dst += snprintf(dst, *buflen, "z_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, term_firstarg(n), 1);
		*dst++ = ')';
	}
	else if (save_n->bifptr == bif_iso_asserta) {
		dst += snprintf(dst, *buflen, "a_(");
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, term_firstarg(n), 1);
		*dst++ = ')';
	}
	else {
		dst += term_sprint2(dstbuf, buflen, &dst, db->pl, NULL, n, 1);
	}

	if (head != save_head)
		term_heapcheck(head);

	if (!in_tran) {
		*dst++ = '.';
		*dst++ = '\n';
	}
	else
		*dst++ = ',';

	*dst = '\0';
	fwrite(*dstbuf, dst - *dstbuf, 1, fp);

	if (!in_tran)
		fflush(fp);
}

node *dbs_read_entry(module *db, nbr_t fpos)
{
	nbr_t save_fpos = ftello(db->fp);
	fseeko(db->fp, fpos, SEEK_SET);
	lexer l;
	lexer_init(&l, db->pl);
	l.db = db;
	char *line = trealla_readline(&l, db->fp, 0);
	fseeko(db->fp, save_fpos, SEEK_SET);

	int len = strlen(line);

	if (line[len-1] == '.')
		len--;

	// Make a transaction. Note: trealla_readline
	// leaves enough empty space at the end for this.

	if (line[len-1] != '_') {
		line[len++] = ',';
		line[len++] = 't';
		line[len++] = '_';
		line[len++] = '.';
		line[len] = '\0';
	}

	if (!line)
		return NULL;

	lexer_parse(&l, NULL, line, NULL);
	free(line);
	node *term = NLIST_FRONT(&l.val_l);
	term = term_firstarg(term);
	node *n = term_firstarg(term);
	term_remove(term, n);
	node *r = NLIST_POP_FRONT(&l.val_l);
	term_heapcheck(r);
	lexer_done(&l);
	return n;
}

static void dbs_load_file(module *db, const char *filename, int tail)
{
	if (!db->pl->quiet)
		printf("INFO: Loading '%s' ... ", filename);

	fflush(stdout);
	db->loading = 1;
	unsigned line_nbr = 0;
	char buffer[1024 * 64];
	setvbuf(db->fp, buffer, _IOFBF, sizeof(buffer));
	size_t any = 0;
	lexer lex;
	lexer_init(&lex, db->pl);

	do {
		char *line;

		while (db->last_fpos = ftello(db->fp), ((line = trealla_readline(&lex, db->fp, 0)) != NULL)) {
			line_nbr++;

			if (!isalpha(line[0]) && (line[0] != '_'))
				continue;

			tpl_query *q = trealla_create_query(db->pl);

			if (!q)
				break;

			q->curr_db = db;

			if (!query_parse(q, line))
				printf("ERROR: '%s'\n", filename);
			else {
				query_run(q);

				if (q->ok)
					any++;
			}

			query_destroy(q);
			free(line);
		}

		if (tail) {
			clearerr(db->fp);
			msleep(tail);
		}
	}
	 while (tail);

	lexer_done(&lex);
	db->loading = 0;

	if (!db->pl->quiet)
		printf(" processed %llu updates\n", (unsigned long long)any);
}

static void dbs_load(module *db, int tail, int load)
{
	char pathname[1024], filename[1024 * 2];
	sprintf(pathname, "%s/db", g_dbdir);
	mkdir(pathname, 0777);
	sprintf(pathname, "%s/db/%s", g_dbdir, db->name);
	mkdir(pathname, 0777);
	snprintf(filename, sizeof(filename), "%s/%s.tmp.dbs", pathname, db->name);
	db->fp = fopen(filename, "rb");

	if (db->fp != NULL) {
		fclose(db->fp);
		db->fp = NULL;
		remove(filename);
	}

	snprintf(filename, sizeof(filename), "%s/%s.new.dbs", pathname, db->name);
	db->fp = fopen(filename, "rb");

	if (db->fp != NULL) {
		fclose(db->fp);
		db->fp = NULL;
		char tmpname[1024], tmpname2[1024];
		snprintf(tmpname, sizeof(tmpname), "%s/%s.log.dbs", pathname, db->name);
		snprintf(tmpname2, sizeof(tmpname2), "%s/%s.%016llX.dbs", pathname, db->name, (long long)time(NULL));
		rename(tmpname, tmpname2);
		rename(filename, tmpname);
	}

	// Load the transaction stream...

	if (load) {
		snprintf(filename, sizeof(filename), "%s/%s.log.dbs", pathname, db->name);
		db->fp = fopen(filename, "rb");

		if (db->fp != NULL) {
			dbs_load_file(db, filename, tail);

			if (g_dbs_merge)
				dbs_merge(db);

			fclose(db->fp);
		}
	}

	snprintf(filename, sizeof(filename), "%s/%s.log.dbs", pathname, db->name);
	db->fp = fopen(filename, "a+b");
	db->dbname = strdup(filename);
	assert(db->fp != NULL);
	g_dbs_merge = 0;
	db->loaded = 1;
}

nbr_t dbs_get_fpos(module *db)
{
	return db->loading ? db->last_fpos : ftello(db->fp);
}

int dbs_done(module *db)
{
	if (db->fp) {
		fseeko(db->fp, 0, SEEK_END);
		nbr_t save_fpos = ftello(db->fp);
		fclose(db->fp);
		db->fp = NULL;

		if (!save_fpos)
			remove(db->dbname);

		free(db->dbname);
		db->dbname = NULL;
	}

	return 1;
}

static int dbs_end(tpl_query *q, int do_sync)
{
	if (!q->in_tran)
		return 0;

	trust_me(q);
	int any = 0;

	node *tmp;

	while ((tmp = NLIST_POP_FRONT(&q->curr_db->tran_queue)) != NULL) {
		if (tmp->n1->flags & FLAG_DBS_RETRACT)
			any += bif_retract(q, tmp->n1, tmp->n2);
		else if (tmp->n1->flags & FLAG_DBS_ASSERTZ)
			any += bif_assertz(q, tmp->n1);
		else if (tmp->n1->flags & FLAG_DBS_ASSERTA)
			any += bif_asserta(q, tmp->n1);

		term_heapcheck(tmp);
	}

	if (any) {
		char tmpbuf[256];
		strcpy(tmpbuf, "t_.\n");
		fwrite(tmpbuf, strlen(tmpbuf), 1, q->curr_db->fp);
		fflush(q->curr_db->fp);

		if (do_sync)
			ffsync(q->curr_db->fp);
	}

	q->in_tran = 0;
	DBUNLOCK(q->curr_db);
	return 1;
}

static int bif_dbs_end_0(tpl_query *q)
{
	dbs_end(q, 1);
	return 1;
}

static int bif_dbs_end_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	int do_sync;

	if (is_integer(term1))
		do_sync = term1->val_i != 0;
	else
		do_sync = !strcmp(VAL_S(term1), "true");

	dbs_end(q, do_sync);
	return 1;
}

static void dbs_bail(tpl_query *q)
{
	node *tmp;

	while ((tmp = NLIST_POP_FRONT(&q->curr_db->tran_queue)) != NULL) {
		tmp->n1->flags &= ~(FLAG_DBS_ASSERTA | FLAG_DBS_ASSERTZ | FLAG_DBS_RETRACT);
		tmp->n1->flags &= ~FLAG_DELETED;

		if (tmp->n2)
			term_heapcheck(tmp->n2);

		term_heapcheck(tmp);
	}

	q->in_tran = 0;
	DBUNLOCK(q->curr_db);
}

int bif_dbs_begin_0(tpl_query *q)
{
	if (q->retry) {
		dbs_bail(q);
		return 0;
	}

	if (q->in_tran)
		return 0;

	allocate_frame(q);
	try_me_nofollow(q);
	DBLOCK(q->curr_db);
	q->in_tran = 1;
	return 1;
}

static int bif_dbs_log_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);
	size_t buflen = PRINTBUF_SIZE; // expandable
	char *dstbuf = (char *)malloc(buflen + 1);
	node *n = clone_term(q, term1);
	dbs_save_node(q->curr_db, q->curr_db->fp, &dstbuf, &buflen, n, q->in_tran);
	term_heapcheck(n);
	free(dstbuf);
	return 1;
}

int bif_dbs_init_0(tpl_query *q)
{
	if (!q->curr_db->loaded)
		dbs_load(q->curr_db, 0, 0);

	return 1;
}

int bif_dbs_load_0(tpl_query *q)
{
	if (!q->curr_db->loaded)
		dbs_load(q->curr_db, 0, 1);

	return 1;
}

int bif_dbs_tail_0(tpl_query *q)
{
	if (!q->curr_db->loaded)
		dbs_load(q->curr_db, 1000, 1);

	return 1;
}

int bif_dbs_tail_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);

	if (!q->curr_db->loaded)
		dbs_load(q->curr_db, term1->val_i, 1);

	return 1;
}

void bifs_load_dbs(void)
{
	DEFINE_BIF("dbs:load", 0, bif_dbs_load_0);
	DEFINE_BIF("dbs:init", 0, bif_dbs_init_0);
	DEFINE_BIF("dbs:tail", 0, bif_dbs_tail_0);
	DEFINE_BIF("dbs:tail", 1, bif_dbs_tail_1);
	DEFINE_BIF("dbs:log", 1, bif_dbs_log_1);
	DEFINE_BIF("dbs:begin", 0, bif_dbs_begin_0);
	DEFINE_BIF("dbs:end", 0, bif_dbs_end_0);
	DEFINE_BIF("dbs:end", 1, bif_dbs_end_1);
}
