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
#define msleep Sleep
#define sleep _sleep
#define snprintf _snprintf
#else
#include <unistd.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#define msleep(ms) usleep((ms)*1000)
#endif

#ifndef USE_SSL
#define USE_SSL 0
#endif

#if USE_SSL
#include "openssl/sha.h"
#endif

#include "trealla.h"
#include "uncle.h"
#include "network.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

extern int g_tpool_size;

int configure_server(tpl_query *q, handler *h, node *term, int (*f)(session*, void *data), int *has_uncle)
{
	char binding[256];
	binding[0] = '\0';
	unsigned port = 0;

	if (term->val_s[0] == ':')
		sscanf(term->val_s, "%*c%u", &port);
	else if (strchr(term->val_s, ':'))
		sscanf(term->val_s, "%255[^:]:%u", binding, &port);

	binding[sizeof(binding)-1] = '\0';
	int uncle = 0, named = 0, mcast = 0;
	int pri = strstr(term->val_s, ";pri") ? 1 : 0;
	int tcp = strstr(term->val_s, ";udp") ? 0 : 1;
	int ssl = strstr(term->val_s, "+ssl") ? 1 : 0;
	ssl += strstr(term->val_s, "+tls") ? 1 : 0;
	int ws = strstr(term->val_s, "+ws") ? 1 : 0;
	if (ssl||ws) tcp = 1;
	char scope[256], name[256], maddr6[256], maddr4[256];
	scope[0] = name[0] = maddr6[0] = maddr4[0] = '\0';
	const char *ptr;

	if ((ptr = strstr(term->val_s, ";scope=")) != NULL)
	{
		uncle = 1;
		sscanf(ptr, ";%*[^=]=%255[^;]", scope);
		scope[sizeof(scope)-1] = '\0';
	}

	if ((ptr = strstr(term->val_s, ";name=")) != NULL)
	{
		named = 1;
		sscanf(ptr, ";%*[^=]=%255[^;]", name);
		name[sizeof(name)-1] = '\0';
	}

	if ((ptr = strstr(term->val_s, ";mcast6=")) != NULL)
	{
		mcast = 1;
		sscanf(ptr, ";%*[^=]=%255[^;]", maddr6);
		maddr6[sizeof(maddr6)-1] = '\0';
	}

	if ((ptr = strstr(term->val_s, ";mcast4=")) != NULL)
	{
		mcast = 1;
		sscanf(ptr, ";%*[^=]=%255[^;]", maddr4);
		maddr4[sizeof(maddr4)-1] = '\0';
	}

	if (uncle && !named)
	{
		*has_uncle = 1;
		if (!port) port = UNCLE_DEFAULT_PORT;
		if (!scope[0]) strcpy(scope, SCOPE_DEFAULT);

		printf("INFO: Add discovery: %s,port=%u,scope=%s\n",
			binding[0]?binding:"*", port, scope);

		if (!handler_add_uncle(h, binding[0]?binding:NULL, port, scope))
			{ QABORT(ABORT_SERVERCANTBIND); return 0; }

		return 1;
	}

	if (named && !*has_uncle)
	{
		*has_uncle = 1;
		unsigned uport = UNCLE_DEFAULT_PORT;
		strcpy(scope, SCOPE_DEFAULT);

		printf("INFO: Auto add discovery: %s,port=%u,scope=%s\n",
			binding[0]?binding:"*", uport, scope);

		if (!handler_add_uncle(h, binding[0]?binding:NULL, uport, scope))
			{ QABORT(ABORT_SERVERCANTBIND); return 0; }
	}

	printf("INFO: Add server: %s,port=%u,tcp=%d,tls=%d %s\n",
		binding[0]?binding:"*", port, tcp, ssl, name[0]?name:"");
	if (!name[0]) sprintf(name, "%u", port);

	if (!handler_add_server(h, f, q,
			binding[0]?binding:NULL, port, port, ws?2:tcp, ssl, pri, name[0]?name:NULL))
		{ QABORT(ABORT_SERVERCANTBIND); return 0; }

	if (mcast)
	{
		printf("INFO: Join mcast: addr6=%s, addr4=%s\n", maddr6, maddr4);

		if (!handler_add_multicast(h, f, q,
				binding[0]?binding:NULL, port, maddr6[0]?maddr6:NULL, maddr4[0]?maddr4:NULL, name[0]?name:""))
			{ QABORT(ABORT_SERVERCANTJOIN); return 0; }
	}

	return 1;
}

static int net_callback(session *s, void *data)
{
	tpl_query *q = (tpl_query*)data;
	node *args = get_args(q);
	node *term1 = get_list(term1);
	node *term2 = get_var(term2);
	node *goal = NLIST_NEXT(q->curr_term);

	if (session_get_udata_flag(s, BYE))
	{
		//printf("DEBUG: BYE\n");
		return 0;
	}

	if (session_on_connect(s))
	{
		//printf("DEBUG: CONNECT\n");
		session_set_udata_flag(s, HELLO);
		tpl_query *who = query_create_subquery(q, 0);
		if (!who) { QABORT(ABORT_OUTOFMEMORY); return 0; }
		begin_query(who, goal);
		stream *sp = CALLOC(stream);
		sp->sptr = s;
		node *n = make_socket(sp);
		put_env(who, who->curr_frame+term2->slot, n, -1);
		n->refcnt--;
		session_set_udata_ptr(s, who);
		process_start_handler(who);
	}
	else if (session_on_disconnect(s))
	{
		//printf("DEBUG: DISCONNECT\n");
		session_clr_udata_flag(s, HELLO);
		session_set_udata_flag(s, BYE);
		tpl_query *who = (tpl_query*)session_get_udata_ptr(s);
		process_restart_handler(who);
	}
	else
	{
		//printf("DEBUG: DATA\n");
		session_clr_udata_flag(s, HELLO);
		tpl_query *who = (tpl_query*)session_get_udata_ptr(s);
		process_restart_handler(who);
	}

	return 0;
}

static int bif_net_server2(tpl_query *q)
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
	if (!stat(keyfile, &st)) handler_set_tls(h, keyfile, certfile);
	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";
	if (!stat(cafile, &st)) handler_set_ca(h, cafile, capath);
	int has_uncle = 0;
	node *l = term1;

	while (is_list(l))
	{
		node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
		node *n = get_arg(q, head, q->latest_context);
		configure_server(q, h, n, &net_callback, &has_uncle);
		node *tail = NLIST_NEXT(head);
		l = get_arg(q, tail, q->latest_context);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int bif_net_server4(tpl_query *q)
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

	const char *keyfile = term3->val_s;
	const char *certfile = term4->val_s;
	struct stat st = {0};
	if (!stat(keyfile, &st)) handler_set_tls(h, keyfile, certfile);
	const char *cafile = CA_PEMFILE;
	const char *capath = "./ca";
	if (!stat(cafile, &st)) handler_set_ca(h, cafile, capath);
	int has_uncle = 0;

	while (is_list(term1))
	{
		term1 = NLIST_FRONT(&term1->val_l);
		term1 = NLIST_NEXT(term1);
		node *n = get_arg(q, term1, q->curr_frame);
		configure_server(q, h, n, &net_callback, &has_uncle);
		term1 = NLIST_NEXT(term1);
	}

	handler_wait(h);
	handler_destroy(h);
	return 1;
}

static int bif_net_handler(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_callable(term2);
	node *term3 = get_callable(term3);
	stream *sp = term1->val_str;

	if (session_get_udata_flag((session*)sp->sptr, BYE))
		q->curr_term = clone_term(q, term3);				// clone?
	else if (session_get_udata_flag((session*)sp->sptr, HELLO))
		q->curr_term = clone_term(q, term2);				// clone?
	else
		return 1;

	call(q);
	return 1;
}

static int bif_net_client(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	char host[256], userid[256], passwd[256];
	host[0] = userid[0] = passwd[0] = '\0';
	unsigned port = 0;
	const char *src = term1->val_s;
	int ssl = 0;

	if (!strncmp(src, "ws://", 5))
	{
		port = 80;
		src += 5;
	}
	else if (!strncmp(src, "wss://", 6))
	{
		port = 443;
		src += 6;
		ssl = 1;
	}
	else if (!strncmp(src, "http://", 7))
	{
		port = 80;
		src += 7;
	}
	else if (!strncmp(src, "https://", 8))
	{
		port = 443;
		src += 8;
		ssl = 1;
	}

	if (src[0] == ':')
		sscanf(src, "%*c%u", &port);
	else if (strchr(src, '@'))
		sscanf(src, "%255[^:]:%255[^@]@%255[^:/]%*c%u", userid, passwd, host, &port);
	else
		sscanf(src, "%255[^:/]%*c%u", host, &port);

	userid[sizeof(userid)-1] = passwd[sizeof(passwd)-1] = '\0';
	host[sizeof(host)-1] = '\0';
	int pri = strstr(src, ";pri") ? 1 : 0;
	int tcp = strstr(src, ";udp") ? 0 : 1;
	ssl += strstr(src, "+ssl") ? 1 : 0;
	ssl += strstr(src, "+tls") ? 1 : 0;
	int ws = strstr(src, "+ws") ? 1 : 0;
	if (ssl||ws) tcp = 1;
	int loop = 0, ttl = 0;
	char scope[256], name[256];
	scope[0] = name[0] = '\0';
	const char *ptr;

	if ((ptr = strstr(src, ";loop=")) != NULL)
		sscanf(ptr, "%*[^=]=%d", &loop);

	if ((ptr = strstr(src, ";ttl=")) != NULL)
		sscanf(ptr, "%*[^=]=%d", &ttl);

	if ((ptr = strstr(src, ";scope=")) != NULL)
	{
		sscanf(ptr, ";%*[^=]=%255[^;]", scope);
		scope[sizeof(scope)-1] = '\0';
	}

	if ((ptr = strstr(src, ";name=")) != NULL)
	{
		unsigned uport = port;
		if (!uport) uport = UNCLE_DEFAULT_PORT;
		if (!scope[0]) strcpy(scope, SCOPE_DEFAULT);
		sscanf(ptr, ";%*[^=]=%255[^;]", name);
		name[sizeof(name)-1] = '\0';

		//printf("DEBUG: Add discovery: %s,port=%u,scope=%s\n", "*", uport, scope);

		uncle *u = uncle_create(NULL, uport, scope, NULL, NULL);
		if (!u) return 0;
		int ms = 1, found = 0;
		unsigned tmp_port = 0;

		for (int i = 0; i < 10; i++, ms *= 2)
		{
			msleep(ms);

			if (!uncle_query(u, name, host, &tmp_port, &tcp, &ssl, &pri))
				continue;

			//printf("DEBUG: Discovery: scope=%s,name=%s,port=%u,tcp=%d,tls=%d,pri=%d\n", scope, name, tmp_port, tcp, ssl, pri);
			found = 1;
			break;
		}

		uncle_destroy(u);

		if (!found)
			{ QABORT(ABORT_NOTEXISTREMOTE); return 0; }

		port = tmp_port;
	}

	//printf("DEBUG: Client: userid=%s,passwd=%s,host=%s,port=%u,tcp=%d,tls=%d,pri=%d\n", userid, passwd, host, port, tcp, ssl, pri);

	session *s = session_open(host, port, tcp, ssl);
	if (!s) { QABORT(ABORT_CANTCONNECT); return 0; }
	stream *sp = CALLOC(stream);
	sp->sptr = s;
	sp->qptr = q;
	if (ws) session_set_websocket(s);
	if (loop||ttl) session_enable_multicast(s, loop, ttl);
	session_set_stash(s, "HOST", host);
	session_set_stash(s, "USER", userid);
	session_set_stash(s, "PASS", passwd);
	node *n = make_socket(sp);
	n->pid = q;
	put_env(q, q->curr_frame+term2->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_net_service(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	const char *name = session_get_name((session*)sp->sptr);
	put_atom(q, q->curr_frame+term2->slot, strdup(name), 1);
	return 1;
}

static int bif_net_local_port(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	unsigned port = session_get_local_port((session*)sp->sptr);
	put_int(q, q->curr_frame+term2->slot, port);
	return 1;
}

static int bif_net_remote_port(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	unsigned port = session_get_remote_port((session*)sp->sptr);
	put_int(q, q->curr_frame+term2->slot, port);
	return 1;
}

static int bif_net_local_addr(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	const char *name = session_get_local_addr((session*)sp->sptr, 0);
	put_atom(q, q->curr_frame+term2->slot, strdup(name), 1);
	return 1;
}

static int bif_net_remote_addr(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	const char *name = session_get_remote_addr((session*)sp->sptr, 0);
	put_atom(q, q->curr_frame+term2->slot, strdup(name), 1);
	return 1;
}

static int bif_net_local_host(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	const char *name = session_get_local_addr((session*)sp->sptr, 1);
	put_atom(q, q->curr_frame+term2->slot, strdup(name), 1);
	return 1;
}

static int bif_net_remote_host(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	const char *name = session_get_remote_addr((session*)sp->sptr, 1);
	put_atom(q, q->curr_frame+term2->slot, strdup(name), 1);
	return 1;
}

static int bif_net_ipv4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	int ok = session_is_ipv4((session*)sp->sptr);
	node *n = make_const_atom(ok?"true":"false", 0);
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_net_ipv6(tpl_query *q)
{
	return !bif_net_ipv4(q);
}

static int bif_net_udp(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	int ok = session_is_udp((session*)sp->sptr);
	node *n = make_const_atom(ok?"true":"false", 0);
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_net_tcp(tpl_query *q)
{
	return !bif_net_udp(q);
}

static int bif_net_tls(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	int ok = session_is_tls((session*)sp->sptr);
	node *n = make_const_atom(ok?"true":"false", 0);
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_net_stash_get4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	node *term4 = get_atom_or_var(term4);
	stream *sp = term1->val_str;
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term2);
	const char *s = session_get_stash((session*)sp->sptr, key);

	if (!*s)
		return unify_term(q, term3, term4, q->curr_frame);

	node *n = make_atom(strdup(s), 1);
	int ok = unify_term(q, term3, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_net_stash_get3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	stream *sp = term1->val_str;
	char tmpbuf[KEY_SIZE];
	const char *key = make_key(q->pl, tmpbuf, term2);
	const char *s = session_get_stash((session*)sp->sptr, key);
	node *n = make_atom(strdup(s), 1);
	int ok = unify_term(q, term3, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_net_stash_set4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	node *term4 = get_atom(term4);
	stream *sp = term1->val_str;
	const char *key = term2->val_s;

	if (UTF8LEN(term2) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	const char *s = session_get_stash((session*)sp->sptr, key);
	node *n = make_atom(strdup(s), 1);
	int ok = unify_term(q, term3, n, q->curr_frame);
	term_heapcheck(n);
	if (ok) session_set_stash((session*)sp->sptr, key, term4->val_s);
	return ok;
}

static int bif_net_stash_set3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	stream *sp = term1->val_str;
	const char *key = term2->val_s;

	if (UTF8LEN(term2) > FUNCTOR_LEN)
		{ QABORT(ABORT_ARGTOOBIG); return 0; }

	session_set_stash((session*)sp->sptr, key, term3->val_s);
	return 1;
}

static int bif_net_stash_clr3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	stream *sp = term1->val_str;
	const char *key = term2->val_s;
	const char *s = session_get_stash((session*)sp->sptr, key);
	node *n = make_atom(strdup(s), 1);
	int ok = unify_term(q, term3, n, q->curr_frame);
	term_heapcheck(n);

	if (ok)
	{
		const char *s = session_del_stash((session*)sp->sptr, key);
		if (s) free((void*)s);
	}

	return ok;
}

static int bif_net_start_tls(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	stream *sp = term1->val_str;
	return session_enable_tls((session*)sp->sptr, NULL, 0);
}

static int bif_net_readmsg(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char *line = NULL;

	if (!session_readmsg((session*)sp->sptr, &line))
	{
		q->is_yielded = 1;
		return 0;
	}

	if (session_on_disconnect((session*)sp->sptr))
		return 0;

	put_atom(q, q->curr_frame+term2->slot, line, 1);
	return 1;
}

static int bif_net_is_socket(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	return is_socket(term1);
}

void bifs_load_net(void)
{
	DEFINE_BIF("net:server", 4, bif_net_server4);
	DEFINE_BIF("net:server", 2, bif_net_server2);
	DEFINE_BIF("net:client", 2, bif_net_client);
	DEFINE_BIF("net:handler", 3, bif_net_handler);
	DEFINE_BIF("net:service", 2, bif_net_service);
	DEFINE_BIF("net:local_port", 2, bif_net_local_port);
	DEFINE_BIF("net:remote_port", 2, bif_net_remote_port);
	DEFINE_BIF("net:local_addr", 2, bif_net_local_addr);
	DEFINE_BIF("net:remote_addr", 2, bif_net_remote_addr);
	DEFINE_BIF("net:local_host", 2, bif_net_local_host);
	DEFINE_BIF("net:remote_host", 2, bif_net_remote_host);
	DEFINE_BIF("net:ipv4", 2, bif_net_ipv4);
	DEFINE_BIF("net:ipv6", 2, bif_net_ipv6);
	DEFINE_BIF("net:tcp", 2, bif_net_tcp);
	DEFINE_BIF("net:udp", 2, bif_net_udp);
	DEFINE_BIF("net:tls", 2, bif_net_tls);
	DEFINE_BIF("sys:is_socket", 1, bif_net_is_socket);
	DEFINE_BIF("net:readmsg", 2, bif_net_readmsg);
	DEFINE_BIF("net:stash_get", 4, bif_net_stash_get4);
	DEFINE_BIF("net:stash_get", 3, bif_net_stash_get3);
	DEFINE_BIF("net:stash_set", 4, bif_net_stash_set4);
	DEFINE_BIF("net:stash_set", 3, bif_net_stash_set3);
	DEFINE_BIF("net:stash_clr", 3, bif_net_stash_clr3);
	DEFINE_BIF("net:start_tls", 1, bif_net_start_tls);
};

