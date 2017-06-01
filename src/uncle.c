#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "jsonq.h"
#include "network.h"
#include "skipbuck.h"
#include "thread.h"
#include "uncle.h"

#ifdef _WIN32
#include <winsock2.h>
#define msleep Sleep
#else
#include <unistd.h>
#define msleep(ms) { struct timespec tv; tv.tv_sec = (ms)/1000; tv.tv_nsec = ((ms)%1000) * 1000 * 1000; nanosleep(&tv, &tv); }
#endif

static const int g_debug = 0;

struct uncle_ {
	handler *h;
	skipbuck *db;
	lock *strand;
	session *s; // used to broadcast on
	char scope[256];
	time_t unique;

	struct {
		const char *name;
		const char *key;
		char addr[256];
		unsigned port;
		int tcp, ssl, pri;
		session *s;
	} search;
};

static int uncle_iter(uncle *u, const char *key, const char *value)
{
	char name[256], addr[256];
	name[0] = addr[0] = '\0';
	unsigned port = 0;
	int tcp = 0, ssl = 0, local = 0, pri = 0;
	sscanf(key, "%255[^/]/%255[^/]/%d/%u/%d/%d/%d", name, addr, &local, &port, &tcp, &ssl, &pri);
	name[sizeof(name) - 1] = 0;
	addr[sizeof(addr) - 1] = 0;

	if (u->search.name[0] && strcmp(u->search.name, name))
		return 0;

	if ((u->search.tcp >= 0) && (u->search.tcp != tcp))
		return 1;

	if ((u->search.ssl >= 0) && (u->search.ssl != ssl))
		return 1;

	if ((u->search.pri >= 0) && (u->search.pri != pri))
		return 1;

	strcpy(u->search.addr, addr);
	u->search.port = port;
	u->search.tcp = tcp;
	u->search.ssl = ssl;
	u->search.key = key;
	return 0;
}

int uncle_query(uncle *u, const char *name, char *addr, unsigned *port, int *tcp, int *ssl, int *pri)
{
	if (!u)
		return 0;

	u->search.name = name;
	u->search.addr[0] = 0;
	u->search.tcp = *tcp;
	u->search.ssl = *ssl;
	u->search.pri = *pri;

	lock_lock(u->strand);

	if (name[0])
		sb_string_find(u->db, name, &uncle_iter, u);
	else
		sb_string_iter(u->db, &uncle_iter, u);

	lock_unlock(u->strand);

	if (!u->search.addr[0])
		return 0;

	strcpy(addr, u->search.addr);
	*port = u->search.port;
	*tcp = u->search.tcp;
	*ssl = u->search.ssl;
	*pri = u->search.pri;
	return 1;
}

static int uncle_db_add(uncle *u, const char *name, int local, const char *addr, unsigned port, int tcp,
                        int ssl, int pri)
{
	char tmpbuf[1024];
	sprintf(tmpbuf, "%s/%s/%d/%u/%d/%d/%d", name, addr, local, port, tcp, ssl, pri);
	if (g_debug)
		printf("DEBUG: DBADD %s\n", tmpbuf);
	lock_lock(u->strand);
	sb_string_set(u->db, tmpbuf, tmpbuf);
	lock_unlock(u->strand);
	return 1;
}

int uncle_add(uncle *u, const char *name, const char *addr, unsigned port, int tcp, int ssl, int pri)
{
	if (!u)
		return 0;

	if (strlen(name) > 255)
		return 0;
	if (strlen(addr) > 255)
		return 0;
	uncle_db_add(u, name, 1, addr, port, tcp, ssl, pri);

	char tmpbuf[1024];
	sprintf(tmpbuf, "{\"$scope\":\"%s\",\"$unique\":%llu,\"$cmd\":\"+\",\"$name\":\"%s\",\"$port\":%u,\"$tcp\":%"
	                "s,\"$ssl\":%s,\"$pri\":%s}\n",
	        u->scope, (unsigned long long)u->unique, name, port, tcp ? "true" : "false", ssl ? "true" : "false",
	        pri ? "true" : "false");
	if (g_debug)
		printf("DEBUG: ADD %s", tmpbuf);
	session_writemsg(u->s, tmpbuf);
	return 1;
}

static int uncle_db_rem(uncle *u, const char *name, int local, const char *addr, int tcp)
{
	u->search.name = name;
	strcpy(u->search.addr, addr);
	u->search.tcp = tcp;
	u->search.ssl = -1;
	u->search.addr[0] = 0;
	if (g_debug)
		printf("DEBUG: DBREM %s\n", u->search.key);

	lock_lock(u->strand);
	sb_string_find(u->db, name, &uncle_iter, u);

	if (!u->search.addr[0]) {
		lock_unlock(u->strand);
		return 0;
	}

	sb_string_del(u->db, u->search.key);
	lock_unlock(u->strand);
	return 1;
}

int uncle_rem(uncle *u, const char *name, const char *addr, int tcp)
{
	if (!u)
		return 0;

	while (uncle_db_rem(u, name, 1, addr, tcp))
		;

	char tmpbuf[1024];
	sprintf(tmpbuf, "{\"$scope\":\"%s\",\"$unique\":%llu,\"$cmd\":\"-\",\"$name\":\"%s\",\"$tcp\":%s}\n", u->scope,
	        (unsigned long long)u->unique, name, tcp ? "true" : "false");
	if (g_debug)
		printf("DEBUG: REM %s", tmpbuf);
	session_writemsg(u->s, tmpbuf);
	return 1;
}

static int uncle_iter2(uncle *u, const char *k, const char *v)
{
	char name[256], addr[256];
	name[0] = addr[0] = '\0';
	unsigned  port = 0;
	int tcp = 0, ssl = 0, local = 0, pri = 0;
	sscanf(v, "%255[^/]/%255[^/]/%d/%u/%d/%d/%d", name, addr, &local, &port, &tcp, &ssl, &pri);
	name[sizeof(name) - 1] = 0;
	addr[sizeof(addr) - 1] = 0;
	if (g_debug)
		printf("DEBUG: ??? %s, search name=%s, tcp=%d, ssl=%d\n", v, u->search.name, u->search.tcp, u->search.ssl);

	if (u->search.name[0] && strcmp(u->search.name, name))
		return 0;

	if ((u->search.tcp >= 0) && (u->search.tcp != tcp))
		return 1;

	if ((u->search.ssl >= 0) && (u->search.ssl != ssl))
		return 1;

	if ((u->search.pri >= 0) && (u->search.pri != pri))
		return 1;

	char tmpbuf[1024];
	sprintf(tmpbuf, "{\"$scope\":\"%s\",\"$unique\":%llu,\"$cmd\":\"+\",\"$name\":\"%s\",\"$port\":%u,\"$tcp\":%"
	                "s,\"$ssl\":%s,\"$pri\":%s}\n",
	        u->scope, (unsigned long long)u->unique, name, port, tcp ? "true" : "false", ssl ? "true" : "false",
	        pri ? "true" : "false");
	if (g_debug)
		printf("DEBUG: SND %s", tmpbuf);
	session_writemsg(u->search.s, tmpbuf);
	return 1;
}

static int uncle_handler(session *s, void *data)
{
	uncle *u = (uncle *)data;
	char *buf = 0;

	if (g_debug)
		printf("*** DEBUG: handler\n");

	if (!session_readmsg(s, &buf))
		return 0;

	const char *addr = session_get_remote_addr(s, 0);
	if (g_debug)
		printf("DEBUG: RCV %s: %s", addr, buf);
	char scope[256];
	jsonq(buf, "$scope", scope, sizeof(scope));
	time_t unique = jsonq_int(buf, "$unique");

	if (unique == u->unique)
		return 1;

	if (strcmp(scope, u->scope))
		return 1;

	char cmd[256], name[256];
	jsonq(buf, "$cmd", cmd, sizeof(cmd));
	jsonq(buf, "$name", name, sizeof(name));
	unsigned port = (unsigned)jsonq_int(buf, "$port");
	int tcp = -1, ssl = -1, pri = -1;

	if (!jsonq_null(buf, "$tcp"))
		tcp = jsonq_bool(buf, "$tcp");

	if (!jsonq_null(buf, "$ssl"))
		ssl = jsonq_bool(buf, "$ssl");

	if (!jsonq_null(buf, "$pri"))
		pri = jsonq_bool(buf, "$pri");

	if (!strcmp(cmd, "+")) {
		uncle_db_add(u, name, 0, addr, port, tcp, ssl, pri);
	}
	else if (!strcmp(cmd, "-")) {
		uncle_db_rem(u, name, 0, addr, tcp);
	}
	else if (!strcmp(cmd, "?")) {
		u->search.name = name;
		u->search.addr[0] = 0;
		u->search.tcp = tcp;
		u->search.ssl = ssl;
		u->search.pri = pri;
		u->search.s = s;

		lock_lock(u->strand);

		if (name[0])
			sb_string_find(u->db, name, &uncle_iter2, u);
		else
			sb_string_iter(u->db, &uncle_iter2, u);

		lock_unlock(u->strand);
	}

	return 1;
}

uncle *uncle_create2(handler *h, const char *binding, unsigned port, const char *scope, const char *maddr6, const char *maddr4)
{
	if (!h || !port)
		return NULL;

	if (strlen(scope) > 255)
		return NULL;

	session *s = session_open("255.255.255.255", port, 0, 0);
	if (!s)
		return NULL;
	session_enable_broadcast(s);

	uncle *u = (uncle *)calloc(1, sizeof(struct uncle_));
	u->db = sb_string_create2();
	u->h = h;
	u->strand = lock_create();
	u->unique = time(NULL);
	u->s = s;
	strcpy(u->scope, scope ? scope : SCOPE_DEFAULT);
	handler_add_server(u->h, &uncle_handler, u, binding, port, 0, 0, 0, NULL);
	handler_add_client(h, &uncle_handler, u, u->s);
	char tmpbuf[1024];
	sprintf(tmpbuf, "{\"$scope\":\"%s\",\"$unique\":%llu,\"$cmd\":\"?\"}\n", u->scope, (unsigned long long)u->unique);
	if (g_debug)
		printf("DEBUG: CREATE %s", tmpbuf);
	session_bcastmsg(u->s, tmpbuf);
	return u;
}

static int uncle_wait(void *data)
{
	uncle *u = (uncle *)data;
	handler_wait(u->h);
	return 1;
}

uncle *uncle_create(const char *binding, unsigned port, const char *scope, const char *maddr6, const char *maddr4)
{
	if (!port)
		return NULL;

	if (strlen(scope) > 255)
		return NULL;

	handler *h = handler_create(0);
	uncle *u = uncle_create2(h, binding, port, scope, maddr6, maddr4);
	thread_run(&uncle_wait, u);
	msleep(1000);
	return u;
}

const char *uncle_get_scope(uncle *u)
{
	if (!u)
		return "";

	return u->scope;
}

void uncle_destroy(uncle *u)
{
	if (!u)
		return;

	handler_destroy(u->h);
	sb_destroy(u->db);
	lock_destroy(u->strand);
	session_close(u->s);
	free(u);
}
