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
#include <winsock2.h>
#define msleep Sleep
#define snprintf _snprintf
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <unistd.h>
#define msleep(ms) { struct timespec tv; tv.tv_sec = (ms)/1000; tv.tv_nsec = ((ms)%1000) * 1000 * 1000; nanosleep(&tv, &tv); }
#endif

#ifndef USE_SSL
#define USE_SSL 0
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"
#include "uncle.h"

int g_tpool_size = 4;

int process_start_handler(void *data)
{
	tpl_query *q = (tpl_query *)data;
	q->is_yielded = 0;
	query_run(q);

	if (!q->is_yielded)
		query_destroy(q);

	return 0;
}

int process_restart_handler(void *data)
{
	tpl_query *q = (tpl_query *)data;
	q->is_yielded = 0;
	query_continue(q);

	if (!q->is_yielded)
		query_destroy(q);

	return 0;
}

int process_yield(tpl_query *q, int locked)
{
	if (!q->tmo_msecs) {
		if (locked)
			PIDUNLOCK(q->pl);

		return 1;
	}

	try_me_nofollow(q);

	if (q->is_forked) {
		q->is_yielded = 1;
		q->is_busy = 0;
		q->timed_out = 0;

		if (locked)
			PIDUNLOCK(q->pl);
	}
	else {
		if (locked)
			PIDUNLOCK(q->pl);

		msleep(1); // TODO: wait on handler
	}

	return 0;
}

static void set_curr_pid(tpl_query *q, tpl_query *who)
{
	if (who)
		who->refcnt++;

	if (q->curr_pid) {
		if (!--q->curr_pid->refcnt)
			query_destroy(q->curr_pid);
	}

	q->curr_pid = who;
}

int process_enqueue(trealla *pl, tpl_query *q, tpl_query *who, node *term, int noerror)
{
	if (pl->abort)
		return 0;

	if (who->is_dead) {
		if (noerror)
			return 0;

		if (q)
			QABORT(ABORT_PIDCLOSED);

		return 0;
	}

	if (term != NULL) {
		node *orig = term;
		orig->refcnt++;
		term = term_make();
		term->val_ptr = orig;
		term->pid = q;
		q->refcnt++;
		who->refcnt++;
	}

	PIDLOCK(pl);

#ifdef DEBUG
	g_enqueues++;
#endif

	if (term != NULL)
		NLIST_PUSH_BACK(&who->queue, term);

	if (who->is_busy || !who->is_forked) {
		g_busy++;
		PIDUNLOCK(pl);
		return 1;
	}

	if (who->is_idle)
		sl_del(&pl->idle, (const char *)who, NULL);

	who->is_idle = 0;
	who->is_busy = 1;

#ifdef DEBUG
	g_rescheds++;
#endif

	PIDUNLOCK(pl);
	tpool_schedule(pl->tp, process_restart_handler, who);
	return 1;
}

static int process_check(tpl_query *q, const tpl_query *who, node *term)
{
	if (q->pl->abort || q->halt)
		return 0;

	if (!NLIST_COUNT(&q->queue) && (q->tmo_msecs < 0)) {
		process_yield_unlocked(q);
		return 0;
	}

	PIDLOCK(q->pl);

	for (node *n = NLIST_FRONT(&q->queue); n; n = term_next(n)) {
		if (who) {
			if (n->pid != who)
				continue;
		}

		if (n->flags & FLAG_SKIPPED)
			continue;

		if (!unify_term(q, term, n->val_ptr, q->curr_frame)) {
			reallocate_frame(q);
			continue;
		}

		q->timed_out = 0;
		NLIST_REMOVE(&q->queue, n);
		PIDUNLOCK(q->pl);
		q->refcnt--;
		n->pid->refcnt--;
		set_curr_pid(q, n->pid);
		term_heapcheck(n->val_ptr);
		term_heapcheck(n);
		return 1;
	}

	if (!q->tmo_msecs || q->timed_out) {
		for (node *n = NLIST_FRONT(&q->queue); n; n = term_next(n))
			n->flags &= ~FLAG_SKIPPED;

		q->timed_out = 1;
		PIDUNLOCK(q->pl);
		return 1;
	}

	if (q->tmo_msecs > 0) {
		q->tmo_when_msecs = gettimeofday_usec() / 1000;
		q->tmo_when_msecs += q->tmo_msecs;
		q->is_idle = 1;
		sl_set(&q->pl->idle, (const char *)q, NULL);
	}

	process_yield_locked(q);
	return 0;
}

void process_error(tpl_query *q)	// FIXME
{
	tpl_query *who = q->parent;
	node *n = make_tuple();
	node *n2 = make_compound();
	term_append(n2, make_and());
	term_append(n2, make_const_atom("EXIT", 1));
	node *n3 = make_compound();
	term_append(n3, make_and());
	term_append(n3, make_int(q->halt));
	term_append(n3, make_atom(strdup(q->halt_s), 1));
	term_append(n2, n3);
	term_append(n, n2);
	process_enqueue(q->pl, q, who, n, 1);
}

static tpl_query *guess_who(tpl_query *q, node *term)
{
	if (is_atom(term)) {
		if (!strcmp(VAL_S(term), "parent") && q->parent && q->linked)
			return q->parent;

		if (!strcmp(VAL_S(term), "self"))
			return q;
	}

	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term);
	tpl_query *who = NULL;
	PIDLOCK(q->pl);
	sl_get(&q->pl->names, key, (void **)&who);
	PIDUNLOCK(q->pl);
	return who;
}

static int bif_proc_fork_0(tpl_query *q)
{
	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	tpl_query *who = query_create_proc(q);

	if (!who) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	who->is_forked = 1;
	begin_query(who, term_next(q->curr_term));

	if (!q->is_forked && q->name) {
		PIDLOCK(q->pl);
		sl_set(&q->pl->names, strdup(q->name), q);
		PIDUNLOCK(q->pl);
	}

	who->is_busy = 1;
	q->pl->end_wait = 0;
	thread_run(process_start_handler, who);
	return 0;
}

static int bif_proc_procinfo_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	size_t cnt = 0;

	if (!strcmp(VAL_S(term1), "idle"))
		cnt = sl_count(&q->pl->idle);
	else if (!strcmp(VAL_S(term1), "names"))
		cnt = sl_count(&q->pl->names);
	else if (!strcmp(VAL_S(term1), "msgs"))
		cnt = NLIST_COUNT(&q->queue);

	put_int(q, q->curr_frame + term2->slot, cnt);
	return 1;
}

static int bif_proc_procinfo_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	tpl_query *who = NULL;
	size_t cnt = 0;

	if ((who = guess_who(q, term1)) != NULL) {
		if (!strcmp(VAL_S(term2), "idle"))
			cnt = who->is_idle;
		else if (!strcmp(VAL_S(term2), "busy"))
			cnt = who->is_busy;
		else if (!strcmp(VAL_S(term2), "msgs"))
			cnt = NLIST_COUNT(&who->queue);
	}

	put_int(q, q->curr_frame + term3->slot, cnt);
	return 1;
}

static int bif_proc_spawn_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	node *term2 = get_callable(term2);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	tpl_query *who = query_create_proc(q);

	if (!who) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	who->linked = 0;
	who->is_forked = 1;
	who->is_busy = 1;
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	who->name = strdup(key);
	PIDLOCK(q->pl);

	if (!q->is_forked)
		if (q->name)
			sl_set(&q->pl->names, strdup(q->name), q);

	int ok = sl_set(&q->pl->names, strdup(who->name), who);
	PIDUNLOCK(q->pl);

	if (!ok) {
		query_destroy(who);
		QABORT(ABORT_DUPLICATENAME);
		return 0;
	}

	begin_query(who, term2);
	tpool_schedule(q->pl->tp, process_start_handler, who);
	return 1;
}

static int bif_proc_spawn_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	tpl_query *who = query_create_proc(q);

	if (!who) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	who->linked = 0;
	who->is_forked = 1;
	who->is_busy = 1;

	if (!q->is_forked) {
		if (q->name) {
			PIDLOCK(q->pl);
			sl_set(&q->pl->names, strdup(q->name), q);
			PIDUNLOCK(q->pl);
		}
	}

	begin_query(who, term1);
	tpool_schedule(q->pl->tp, process_start_handler, who);
	return 1;
}

static int bif_proc_spawn_link_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	node *term2 = get_callable(term2);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	tpl_query *who = query_create_proc(q);

	if (!who) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	who->linked = 1;
	who->is_forked = 1;
	who->is_busy = 1;
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term1);
	who->name = strdup(key);
	PIDLOCK(q->pl);

	if (!q->is_forked)
		if (q->name)
			sl_set(&q->pl->names, strdup(q->name), q);

	int ok = sl_set(&q->pl->names, strdup(who->name), who);
	PIDUNLOCK(q->pl);

	if (!ok) {
		query_destroy(who);
		QABORT(ABORT_DUPLICATENAME);
		return 0;
	}

	begin_query(who, term2);
	tpool_schedule(q->pl->tp, process_start_handler, who);
	return 1;
}

static int bif_proc_spawn_link_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	tpl_query *who = query_create_proc(q);

	if (!who) {
		QABORT(ABORT_OUTOFMEMORY);
		return 0;
	}

	who->linked = 1;
	who->is_forked = 1;
	who->is_busy = 1;

	if (!q->is_forked) {
		if (q->name) {
			PIDLOCK(q->pl);
			sl_set(&q->pl->names, strdup(q->name), q);
			PIDUNLOCK(q->pl);
		}
	}

	begin_query(who, term1);
	tpool_schedule(q->pl->tp, process_start_handler, who);
	return 1;
}

static int proc_callback(tpl_query *q, session *s, node *goal, node *var)
{
	if (session_on_connect(s)) {
		// printf("DEBUG: CONNECT\n");
		session_set_udata_flag(s, HELLO);
		tpl_query *who = query_create_proc(q);

		if (!who) {
			QABORT(ABORT_OUTOFMEMORY);
			return 0;
		}

		begin_query(who, goal);
		stream *sp = calloc(1, sizeof(stream));
		sp->sptr = s;
		sp->subqptr = q;
		node *n = make_socket(sp);
		n->flags |= FLAG_PID;
		put_env(who, var->slot, n, -1);
		term_heapcheck(n);
		session_set_udata_int(s, (size_t)(void *)who);
		who->is_forked = 1;
		process_start_handler(who);
	}
	else if (session_on_disconnect(s)) {
		// printf("DEBUG: DISCONNECT\n");
		session_clr_udata_flag(s, HELLO);
		session_set_udata_flag(s, BYE);
		tpl_query *who = (tpl_query *)session_get_udata_int(s);
		set_curr_pid(who, NULL);
		who->halt = 1;
		process_enqueue(q->pl, q, who, NULL, 0);
		return 0;
	}
	else {
		// printf("DEBUG: DATA\n");
		session_clr_udata_flag(s, HELLO);
	}

	tpl_query *who = (tpl_query *)session_get_udata_int(s);
	char *line = NULL;

	if (!session_readmsg(s, &line))
		return 0;

	// printf("DEBUG: GOT: %s", line);
	size_t len = strlen(line) - 1;
	line[len] = '\0';

	if (!line[0]) {
		free(line);
		return 1;
	}

	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, line, NULL);
	free(line);
	xref_clause(&l, l.r);
	node *term = term_first(l.r);
	process_enqueue(q->pl, q, who, term, 0);
	term_heapcheck(l.r);
	lexer_done(&l);
	return 1;
}

static int proc_callback3(session *s, void *data)
{
	tpl_query *q = (tpl_query *)data;
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_var(term3);
	node *goal = term2;
	return proc_callback(q, s, goal, term3);
}

static int bif_proc_server_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_var(term3);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	handler *h = handler_create(-1);
	handler_add_tpool(h, q->pl->tp);

#if USE_SSL
	const char *keyfile = KEY_PEMFILE;
	const char *certfile = CERT_PEMFILE;
	struct stat st = {0};

	if (!stat(keyfile, &st))
		handler_set_tls(h, keyfile, certfile);

	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";
	if (!stat(cafile, &st))
		handler_set_ca(h, cafile, capath);
#endif

	int has_uncle = 0;
	node *l = term1;

	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = get_arg(q, head, q->latest_context);
		configure_server(q, h, n, &proc_callback3, &has_uncle);
		node *tail = term_next(head);
		l = get_arg(q, tail, q->latest_context);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int bif_proc_server5(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_var(term3);
	node *term4 = get_atom(term4);
	node *term5 = get_atom(term5);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	handler *h = handler_create(-1);
	handler_add_tpool(h, q->pl->tp);

#if USE_SSL
	const char *keyfile = VAL_S(term4);
	const char *certfile = VAL_S(term5);
	struct stat st = {0};

	if (!stat(keyfile, &st))
		handler_set_tls(h, keyfile, certfile);

	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";
	if (!stat(cafile, &st))
		handler_set_ca(h, cafile, capath);
#endif

	int has_uncle = 0;

	while (is_list(term1)) {
		term1 = term_first(term1);
		term1 = term_next(term1);
		node *n = get_arg(q, term1, q->curr_frame);
		configure_server(q, h, n, &proc_callback3, &has_uncle);
		term1 = term_next(term1);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int proc_callback2(session *s, void *data)
{
	tpl_query *q = (tpl_query *)data;
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_var(term2);
	node *goal = term_next(q->curr_term);
	return proc_callback(q, s, goal, term2);
}

static int bif_proc_server_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_var(term2);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	handler *h = handler_create(-1);
	handler_add_tpool(h, q->pl->tp);

	const char *keyfile = KEY_PEMFILE;
	const char *certfile = CERT_PEMFILE;
	struct stat st = {0};

	if (!stat(keyfile, &st))
		handler_set_tls(h, keyfile, certfile);

	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";

	if (!stat(cafile, &st))
		handler_set_ca(h, cafile, capath);

	int has_uncle = 0;
	node *l = term1;

	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = get_arg(q, head, q->latest_context);
		configure_server(q, h, n, &proc_callback2, &has_uncle);
		node *tail = term_next(head);
		l = get_arg(q, tail, q->latest_context);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int bif_proc_server4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_var(term2);
	node *term3 = get_atom(term3);
	node *term4 = get_atom(term4);

	if (!q->pl->tp)
		q->pl->tp = tpool_create(g_tpool_size);

	handler *h = handler_create(-1);
	handler_add_tpool(h, q->pl->tp);

	const char *keyfile = VAL_S(term3);
	const char *certfile = VAL_S(term4);
	struct stat st = {0};

	if (!stat(keyfile, &st))
		handler_set_tls(h, keyfile, certfile);

	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";

	if (!stat(cafile, &st))
		handler_set_ca(h, cafile, capath);

	int has_uncle = 0;
	node *l = term1;

	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = get_arg(q, head, q->latest_context);
		configure_server(q, h, n, &proc_callback2, &has_uncle);
		node *tail = term_next(head);
		l = get_arg(q, tail, q->latest_context);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int client_callback(session *s, void *data)
{
	tpl_query *q = (tpl_query *)data;

	if (session_on_disconnect(s)) {
		set_curr_pid(q, NULL);
		return 1;
	}

	char *line = NULL;

	if (!session_readmsg(s, &line))
		return 0;

	size_t len = strlen(line) - 1;
	line[len] = '\0';

	if (!line[0]) {
		free(line);
		return 1;
	}

	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, line, NULL);
	free(line);
	xref_clause(&l, l.r);
	node *term = term_first(l.r);
	process_enqueue(q->pl, q, q, term, 0);
	term_heapcheck(l.r);
	lexer_done(&l);
	return 1;
}

static int start_netwait(tpl_query *q)
{
	handler_wait_indefinitely(q->pl->h);
	return 1;
}

static int bif_proc_abort_1(tpl_query *q)
{
	if (!q->parent)
		return 0;

	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	tpl_query *who = NULL;

	if (!is_stream(term1)) {
		if (!(who = guess_who(q, term1)))
			return 0;
	}
	else {
		stream *sp = term1->val_str;
		who = sp->subqptr;
	}

	if ((who == q->parent) || !who->parent)
		return 0;

	who->halt = 1;
	process_enqueue(q->pl, q, who, NULL, 0);
	return who != q ? 1 : 0;
}

static int bif_proc_pid_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);
	node *term2 = get_var(term2);
	tpl_query *pid = guess_who(q, term1);

	if (pid) {
		stream *sp = calloc(1, sizeof(stream));
		sp->subqptr = pid;
		node *tmp = make_stream(sp);
		tmp->flags |= FLAG_PID;
		put_env(q, q->curr_frame + term2->slot, tmp, -1);
		term_heapcheck(tmp);
		return 1;
	}

	if (!q->pl->h) {
		if (!q->pl->tp)
			q->pl->tp = tpool_create(g_tpool_size);

		q->pl->h = handler_create(-1);
		handler_add_tpool(q->pl->h, q->pl->tp);
		thread_run((int (*)(void *)) & start_netwait, q);
	}

	char host[256], userid[256], passwd[256];
	host[0] = userid[0] = passwd[0] = '\0';
	unsigned port = 0;
	const char *src = VAL_S(term1);

	if (src[0] == ':')
		sscanf(src, "%*c%u", &port);
	else if (strchr(src, '@'))
		sscanf(src, "%255[^:]:%255[^@]@%255[^:/]%*c%u", userid, passwd, host, &port);
	else
		sscanf(src, "%255[^:/]%*c%u", host, &port);

	userid[sizeof(userid) - 1] = passwd[sizeof(passwd) - 1] = '\0';
	host[sizeof(host) - 1] = '\0';

	int pri = strstr(VAL_S(term1), ";pri") ? 1 : 0;
	int tcp = strstr(VAL_S(term1), ";udp") ? 0 : 1;
	int ssl = strstr(VAL_S(term1), "+ssl") ? 1 : 0;
	ssl += strstr(VAL_S(term1), "+tls") ? 1 : 0;
	int ws = strstr(VAL_S(term1), "+ws") ? 1 : 0;

	if (ssl || ws)
		tcp = 1;

	char scope[256], name[256];
	scope[0] = name[0] = '\0';
	const char *ptr;

	if ((ptr = strstr(VAL_S(term1), ";scope=")) != NULL) {
		sscanf(ptr, ";%*[^=]=%255[^;]", scope);
		scope[sizeof(scope) - 1] = '\0';
	}

	if ((ptr = strstr(src, ";name=")) != NULL) {
		unsigned uport = port;

		if (!uport)
			uport = UNCLE_DEFAULT_PORT;

		if (!scope[0])
			strcpy(scope, SCOPE_DEFAULT);

		sscanf(ptr, "%255[^;]", name);
		name[sizeof(name) - 1] = '\0';
		printf("DEBUG: Add discovery: %s,port=%u,scope=%s\n", "*", uport, scope);
		uncle *u = uncle_create(NULL, uport, scope, NULL, NULL);

		if (!u)
			return 0;

		int ms = 1, found = 0;
		unsigned tmp_port = 0;

		for (int i = 0; i < 10; i++, ms *= 2) {
			msleep(ms);

			printf("DEBUG: Search: scope=%s,name=%s,tcp=%d,tls=%d,pri=%d\n", scope, name, tcp, ssl, pri);

			if (!uncle_query(u, name, host, &tmp_port, &tcp, &ssl, &pri))
				continue;

			printf("DEBUG: Discovery: scope=%s,name=%s,port=%u,tcp=%d,tls=%d,pri=%d\n", scope, name, tmp_port, tcp, ssl, pri);
			found = 1;
			break;
		}

		uncle_destroy(u);

		if (!found) {
			QABORT(ABORT_NOTEXISTREMOTE);
			return 0;
		}

		port = tmp_port;
	}

	// printf("DEBUG: Client: userid=%s,passwd=%s,host=%s,port=%u,tcp=%d,tls=%d,pri=%d\n", userid, passwd, host, port, tcp, ssl,
	// pri);

	session *s = session_open(host, port, tcp, ssl);

	if (!s) {
		QABORT(ABORT_CANTCONNECT);
		return 0;
	}

	stream *sp = calloc(1, sizeof(stream));
	sp->sptr = s;
	sp->subqptr = q;

	if (ws)
		session_set_websocket(s);

	session_set_stash(s, "HOST", VAL_S(term1));
	session_set_stash(s, "USER", userid);
	session_set_stash(s, "PASS", passwd);
	node *n = make_socket(sp);
	n->flags |= FLAG_PID;
	n->pid = q;
	handler_add_client(q->pl->h, &client_callback, q, s);
	put_env(q, q->curr_frame + term2->slot, n, -1);
	term_heapcheck(n);
	return 1;
}

static int bif_proc_pid_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);

	if (!q->curr_pid) {
		QABORT(ABORT_NOCURRENTPID);
		return 0;
	}

	stream *sp = calloc(1, sizeof(stream));
	sp->subqptr = q->curr_pid;
	node *tmp = make_stream(sp);
	tmp->flags |= FLAG_PID;
	put_env(q, q->curr_frame + term1->slot, tmp, -1);
	term_heapcheck(tmp);
	return 1;
}

static int bif_proc_send_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_nonvar(term2);

	if (!is_socket(term1)) {
		tpl_query *who;

		if (!(who = guess_who(q, term1))) {
			QABORT(ABORT_NOTEXISTPROCESS);
			return 0;
		}

		return process_enqueue(q->pl, q, who, term2, 0);
	}

	if (!is_pid(term1)) {
		QABORT(ABORT_INVALIDARGNOTPID);
		return 0;
	}

	stream *sp = term1->val_str;

	if (!is_socket(term1)) {
		tpl_query *who = sp->subqptr;
		return process_enqueue(q->pl, q, who, term2, 0);
	}

	if (!is_socket(term1)) {
		QABORT(ABORT_INVALIDARGNOTSOCKET);
		return 0;
	}

	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 1);
	tmpbuf[len++] = '\n';
	tmpbuf[len] = '\0';

	if (!session_write((session *)sp->sptr, tmpbuf, len)) {
		free(tmpbuf);
		QABORT(ABORT_STREAMCLOSED);
		return 0;
	}

	free(tmpbuf);
	return 1;
}

static int bif_proc_send_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term2 = get_nonvar(term2);

	if (!q->curr_pid) {
		QABORT(ABORT_NOCURRENTPID);
		return 0;
	}

	tpl_query *who = q->curr_pid;
	return process_enqueue(q->pl, q, who, term2, 0);
}

static int bif_proc_recv_2(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);

	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	tpl_query *who;

	if (!is_socket(term1)) {
		if (!(who = guess_who(q, term1))) {
			QABORT(ABORT_NOTEXISTPROCESS);
			return 0;
		}
	}
	else if (!is_pid(term1)) {
		QABORT(ABORT_INVALIDARGNOTPID);
		return 0;
	}
	else {
		stream *sp = term1->val_str;
		who = sp->subqptr;
	}

	return process_check(q, who, term2);
}

static int bif_proc_recv_1(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);

	node *args = get_args(q);
	node *term1 = get_term(term1);
	return process_check(q, NULL, term1);
}

static int bif_proc_undo_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *tmp = clone_term(q, term1);
	tmp->flags |= FLAG_SKIPPED;
	tmp->pid = q->curr_pid;
	PIDLOCK(q->pl);
	NLIST_PUSH_BACK(&q->queue, tmp);
	PIDUNLOCK(q->pl);
	return 1;
}

static int bif_proc_rsvp_3(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);

	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_nonvar(term2);
	node *term3 = get_term(term3);
	tpl_query *who;

	if (q->retry)
		return process_check(q, q->curr_pid, term3);

	if (!is_socket(term1)) {
		if (!(who = guess_who(q, term1))) {
			QABORT(ABORT_NOTEXISTPROCESS);
			return 0;
		}
	}
	else if (!is_pid(term1)) {
		QABORT(ABORT_INVALIDARGNOTPID);
		return 0;
	}
	else {
		stream *sp = term1->val_str;
		who = sp->subqptr;
	}

	if (!is_socket(term1)) {
		if (!process_enqueue(q->pl, q, who, term2, 0))
			return 0;
	}
	else if (!is_socket(term1)) {
		QABORT(ABORT_INVALIDARGNOTSOCKET);
		return 0;
	}
	else {
		stream *sp = term1->val_str;
		size_t max_len = PRINTBUF_SIZE;
		char *tmpbuf = (char *)malloc(max_len + 1);
		char *dst = tmpbuf;
		size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 1);
		tmpbuf[len++] = '\n';
		tmpbuf[len] = '\0';

		if (!session_write((session *)sp->sptr, tmpbuf, len)) {
			QABORT(ABORT_STREAMCLOSED);
			return 0;
		}

		free(tmpbuf);
	}

	set_curr_pid(q, who);
	return process_check(q, who, term3);
}

static int bif_proc_rsvp_2(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);

	node *args = get_args(q);
	node *term1 = get_nonvar(term1);
	node *term2 = get_term(term2);

	if (!q->curr_pid) {
		QABORT(ABORT_NOCURRENTPID);
		return 0;
	}

	tpl_query *who = q->curr_pid;

	if (q->retry)
		return process_check(q, who, term2);

	if (!process_enqueue(q->pl, q, who, term1, 0))
		return 0;

	return process_check(q, who, term2);
}

static int bif_proc_tmo_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_int(term1);

	if (is_atom(term1) && strcmp(VAL_S(term1), "inf") && strcmp(VAL_S(term1), "infinite")) {
		QABORT(ABORT_INVALIDARGNOTINT);
		return 0;
	}

	if (!is_atom(term1))
		q->tmo_msecs = get_word(term1);
	else
		q->tmo_msecs = -1;

	q->timed_out = 0;
	return 1;
}

static int bif_proc_after_0(tpl_query *q)
{
	return q->timed_out;
}

static int bif_proc_erase_0(tpl_query *q)
{
	if (q->kvs)
		sl_clear(q->kvs, (void (*)(void *)) & term_heapcheck);

	return 1;
}

static int bif_proc_erase_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);

	if (q->kvs) {
		node *n = NULL;

		if (sl_del(q->kvs, VAL_S(term1), (void **)&n))
			term_heapcheck(n);
	}

	return 1;
}

static int bif_proc_lput_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	node *term3 = get_nonvar(term3);
	node *value = NULL;
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	if (!q->kvs)
		value = make_const_atom("[]", 0);
	else if (!sl_get(q->kvs, VAL_S(term1), (void **)&value))
		value = make_const_atom("[]", 0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	if (!ok)
		return 0;

	if (!q->kvs) {
		q->kvs = malloc(sizeof(skiplist));
		sl_init(q->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(q->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(q->kvs, strdup(VAL_S(term1)), term3);
	term3->refcnt++;
	return 1;
}

static int bif_proc_put_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	node *term3 = get_nonvar(term3);
	node *value = NULL;
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	if (!q->kvs)
		value = make_quick_int(0);
	else if (!sl_get(q->kvs, VAL_S(term1), (void **)&value))
		value = make_quick_int(0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	if (!ok)
		return 0;

	if (!q->kvs) {
		q->kvs = malloc(sizeof(skiplist));
		sl_init(q->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(q->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(q->kvs, strdup(VAL_S(term1)), term3);
	term3->refcnt++;
	return 1;
}

static int bif_proc_put_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_nonvar(term2);
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	if (!q->kvs) {
		q->kvs = malloc(sizeof(skiplist));
		sl_init(q->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(q->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(q->kvs, strdup(VAL_S(term1)), term2);
	term2->refcnt++;
	return 1;
}

static int bif_proc_get_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	node *value = NULL;

	if (!q->kvs)
		value = make_quick_int(0);
	else if (!sl_get(q->kvs, VAL_S(term1), (void **)&value))
		value = make_quick_int(0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	return ok;
}

static int bif_proc_lget_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	node *value = NULL;

	if (!q->kvs)
		value = make_const_atom("[]", 0);
	else if (!sl_get(q->kvs, VAL_S(term1), (void **)&value))
		value = make_const_atom("[]", 0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	return ok;
}

static int bif_proc_get_keys_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	node *value, *l = make_list();
	node *save_l = l;
	int cnt = 0, ok;

	if (q->kvs) {
		sl_start(q->kvs);
		const char *key;

		while ((key = sl_next(q->kvs, (void **)&value)) != NULL) {
			if (!unify_term(q, value, term1, q->curr_frame)) {
				reallocate_frame(q);
				continue;
			}

			node *tmp = make_atom(strdup(key), 1);
			term_append(l, tmp);
			cnt++;

			if (!q->kvs->iter)
				break;

			tmp = make_list();
			term_append(l, tmp);
			l = tmp;
		}
	}

	if (cnt) {
		term_append(l, make_const_atom("[]", 0));
		ok = unify_term(q, term2, save_l, q->curr_frame);
	}
	else {
		node *tmp = make_const_atom("[]", 0);
		ok = unify_term(q, term2, tmp, q->curr_frame);
		term_heapcheck(tmp);
	}

	term_heapcheck(save_l);
	return ok;
}

static int bif_proc_get_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	node *value, *l = make_list();
	node *save_l = l;
	const char *key;
	int cnt = 0, ok;

	if (q->kvs) {
		sl_start(q->kvs);

		while ((key = sl_next(q->kvs, (void **)&value)) != NULL) {
			node *n = make_tuple();
			node *n2 = make_compound();
			term_append(n2, make_and());
			term_append(n2, make_atom(strdup(key), 1));
			term_append(n2, clone_term(q, value));
			term_append(n, n2);
			term_append(l, n);
			cnt++;

			if (!q->kvs->iter)
				break;

			node *tmp;
			term_append(l, tmp = make_list());
			l = tmp;
		}
	}

	if (cnt) {
		term_append(l, make_const_atom("[]", 0));
		ok = unify_term(q, term1, save_l, q->curr_frame);
	}
	else {
		node *tmp = make_const_atom("[]", 0);
		ok = unify_term(q, term1, tmp, q->curr_frame);
		term_heapcheck(tmp);
	}

	term_heapcheck(save_l);
	return ok;
}

static int bif_proc_until_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_callable(term1);

	while (!g_abort && !q->halt) {
		tpl_query *who = query_create_subquery(q);

		if (!who) {
			QABORT(ABORT_OUTOFMEMORY);
			return 0;
		}

		begin_query(who, term1);
		int ok = query_run(who);
		query_destroy(who);

		if (ok)
			break;

		msleep(1); // FIXME
	}

	return 1;
}

static int bif_proc_end_wait_0(tpl_query *q)
{
	q->pl->end_wait = 1;

	if (q->pl->h)
		handler_shutdown(q->pl->h);

	return 1;
}

static int bif_proc_wait_0(tpl_query *q)
{
	skiplist tmplist;
	sl_init(&tmplist, 0, NULL, NULL);

	while (!g_abort && !q->pl->abort && !q->pl->end_wait) {
		if (!sl_count(&q->pl->idle)) {
			msleep(1);
			continue;
		}

		uint64_t now_msecs = gettimeofday_usec() / 1000;
		PIDLOCK(q->pl);
		sl_start(&q->pl->idle);
		tpl_query *who;

		while ((who = (tpl_query *)sl_next(&q->pl->idle, NULL)) != NULL) {
			if (who->tmo_when_msecs > now_msecs)
				break;

			sl_set(&tmplist, (char *)who, NULL);

			if (q->pl->end_wait)
				break;
		}

		PIDUNLOCK(q->pl);

		if (!sl_count(&tmplist)) {
			msleep(1);
			continue;
		}

		sl_start(&tmplist);

		while ((who = (tpl_query *)sl_next(&tmplist, NULL)) != NULL) {
			PIDLOCK(q->pl);

			if (!sl_del(&q->pl->idle, (char *)who, NULL)) {
				PIDUNLOCK(q->pl);
				continue;
			}

			for (node *n = NLIST_FRONT(&who->queue); n; n = term_next(n))
				n->flags &= ~FLAG_SKIPPED;

			who->timed_out = 1;
			who->is_idle = 0;
			who->is_busy = 1;
			PIDUNLOCK(q->pl);
			tpool_schedule(q->pl->tp, process_restart_handler, who);
		}

		sl_clear(&tmplist, NULL);
	}

	sl_done(&tmplist, NULL);
	msleep(1);
	return 1;
}

static int bif_linda_init_0(tpl_query *q)
{
	node *tmp = make_compound();
	term_append(tmp, make_const_atom("dynamic", 0));
	node *tmp2 = make_compound();
	term_append(tmp2, make_const_atom("/", 0));
	term_append(tmp2, make_const_atom("{}", 0));
	term_append(tmp2, make_int(1));
	term_append(tmp, tmp2);
	node *tmp3 = make_list();
	term_append(tmp3, make_const_atom("notify", 0));
	term_append(tmp3, make_const_atom("[]", 0));
	term_append(tmp, tmp3);
	dir_dynamic(q->lex, tmp2);
	term_heapcheck(tmp);
	return 1;
}

static int bif_linda_init_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_list(term1);

	if (is_atom(term1)) {
		if (strcmp(VAL_S(term1), "[]")) {
			return 0;
		}
	}

	node *tmp = make_compound();
	term_append(tmp, make_const_atom("dynamic", 0));
	node *tmp2 = make_compound();
	term_append(tmp2, make_const_atom("/", 0));
	term_append(tmp2, make_const_atom("{}", 0));
	term_append(tmp2, make_int(1));
	term_append(tmp, tmp2);
	node *tmp3 = make_list();
	term_append(tmp, tmp3);
	term_append(tmp3, make_const_atom("notify", 0));

	while (is_list(term1)) {
		node *n2 = term_firstarg(term1);

		if (is_atom(n2)) {
			if (strcmp(VAL_S(n2), "notify")) {
				node *tmp4 = make_list();
				term_append(tmp3, tmp4);
				term_append(tmp4, make_const_atom(VAL_S(n2), 1));
				tmp3 = tmp4;
			}
		}

		term1 = term_next(n2);
	}

	term_append(tmp3, make_const_atom("[]", 0));
	dir_dynamic(q->lex, tmp2);
	term_heapcheck(tmp);
	return 1;
}

void bifs_load_proc(void)
{
	DEFINE_BIF("proc:lput", 3, bif_proc_lput_3);
	DEFINE_BIF("proc:put", 3, bif_proc_put_3);
	DEFINE_BIF("proc:put", 2, bif_proc_put_2);
	DEFINE_BIF("proc:get_keys", 2, bif_proc_get_keys_2);
	DEFINE_BIF("proc:get", 2, bif_proc_get_2);
	DEFINE_BIF("proc:get", 1, bif_proc_get_1);
	DEFINE_BIF("proc:lget", 2, bif_proc_lget_2);
	DEFINE_BIF("proc:erase", 1, bif_proc_erase_1);
	DEFINE_BIF("proc:erase", 0, bif_proc_erase_0);

	DEFINE_BIF("proc:after", 0, bif_proc_after_0);
	DEFINE_BIF("proc:wait", 0, bif_proc_wait_0);
	DEFINE_BIF("proc:end_wait", 0, bif_proc_end_wait_0);
	DEFINE_BIF("proc:until", 1 + 1, bif_proc_until_2);
	DEFINE_BIF("proc:fork", 0, bif_proc_fork_0);
	DEFINE_BIF("proc:procinfo", 2, bif_proc_procinfo_2);
	DEFINE_BIF("proc:procinfo", 3, bif_proc_procinfo_3);
	DEFINE_BIF("proc:tmo", 1, bif_proc_tmo_1);
	DEFINE_BIF("proc:spawn", 2, bif_proc_spawn_2);
	DEFINE_BIF("proc:spawn", 1, bif_proc_spawn_1);
	DEFINE_BIF("proc:spawn_link", 2, bif_proc_spawn_link_2);
	DEFINE_BIF("proc:spawn_link", 1, bif_proc_spawn_link_1);
	DEFINE_BIF("proc:pid", 2, bif_proc_pid_2);
	DEFINE_BIF("proc:pid", 1, bif_proc_pid_1);
	DEFINE_BIF("proc:abort", 1, bif_proc_abort_1);
	DEFINE_BIF("proc:rsvp", 3, bif_proc_rsvp_3);
	DEFINE_BIF("proc:rsvp", 2, bif_proc_rsvp_2);
	DEFINE_BIF("proc:send", 2, bif_proc_send_2);
	DEFINE_BIF("proc:send", 1, bif_proc_send_1);
	DEFINE_BIF("proc:recv", 2, bif_proc_recv_2);
	DEFINE_BIF("proc:recv", 1, bif_proc_recv_1);
	DEFINE_BIF("proc:receive", 1, bif_proc_recv_1);
	DEFINE_BIF("proc:undo", 1, bif_proc_undo_1);
	DEFINE_BIF("proc:server", 3, bif_proc_server_3);
	DEFINE_BIF("proc:server", 2, bif_proc_server_2);
	DEFINE_BIF("proc:server", 5, bif_proc_server5);
	DEFINE_BIF("proc:server", 4, bif_proc_server4);

	DEFINE_BIF("linda:init", 0, bif_linda_init_0);
	DEFINE_BIF("linda:init", 1, bif_linda_init_1);
	DEFINE_BIF("linda:eval", 1, bif_proc_spawn_link_1);
	DEFINE_BIF("linda:fork", 0, bif_proc_fork_0);
	DEFINE_BIF("linda:wait", 0, bif_proc_wait_0);
	DEFINE_BIF("linda:end_wait", 0, bif_proc_end_wait_0);
}
