#ifndef TREALLA_H
#define TREALLA_H

#include <stdio.h>

typedef struct trealla_ trealla;
typedef struct tpl_query_ tpl_query;

// Create a Prolog instance

extern trealla *trealla_create(const char *name);
extern void trealla_trace(trealla *pl, int mode);
extern void trealla_noopt(trealla *pl, int mode);
extern int trealla_consult_file(trealla *pl, const char *name);
extern int trealla_consult_fp(trealla *pl, FILE *fp);
extern int trealla_consult_text(trealla *pl, const char *s, const char *name);
extern int trealla_deconsult(trealla *pl, const char *name);
extern int trealla_run_query(trealla *pl, const char *s);	// 4 in one
extern void trealla_destroy(trealla *pl);

// Create a Query instance

extern tpl_query *trealla_create_query(trealla *pl);
extern int query_parse_file(tpl_query *q, const char *s, FILE *fp);
extern int query_parse(tpl_query *q, const char *s);
extern int query_run(tpl_query *q);						// Initial solution
extern int query_continue(tpl_query *q);				// Next solution
extern int query_choices(tpl_query *q);
extern void query_dump(tpl_query *q);
extern void query_trace(tpl_query *q);
extern void query_stats(tpl_query *q);
extern double query_elapsed(tpl_query *q);
extern double query_get_float(tpl_query *q, unsigned idx);
extern long long query_get_integer(tpl_query *q, unsigned idx);
extern char *query_get_text(tpl_query *q, unsigned idx);	// MUST free
extern void query_abort(tpl_query *q);
extern void query_destroy(tpl_query *q);

extern int g_trealla_memlimit_mb;
extern const char *g_trealla_version;
extern volatile int g_abort;

#endif
