#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#ifdef _WIN32
#include <direct.h>
#include <io.h>
#define snprintf _snprintf
#define mkdir(a,b) _mkdir(a)
#define isatty _isatty
#else
#include <unistd.h>
#endif

#include "trealla.h"

#include "daemon.h"
#include "history.h"

#ifndef ISO_ONLY
#include "network.h"
#endif

#ifndef ISO_ONLY
extern int http_get10(session *s, const char *path, int keep, int *status, int head);
extern int http_get11(session *s, const char *path, int keep, int *status, int head);
#endif

static void sigfn(int s)
{
	signal(SIGINT, 0);
	g_abort = 1;
}

static int http11 = 1;

#ifndef ISO_ONLY
static void manifest_file(session *s, const char *branch, const char *appname, const char *filename)
{
	if (filename[0] == '-') {
		printf("INFO: DELETE %s/%s\n", appname, filename + 1);
		remove(filename + 1);
		return;
	}

	if (filename[0] == '+') {
		struct stat st;

		if (stat(filename + 1, &st) == 0)
			return;

		filename = filename + 1;
	}

	char tmpbuf[256];
	sprintf(tmpbuf, "/trealla-lang/apps/%s/%s/%s", branch, appname, filename);
	int status = 0, keep_alive = 1;

	if (http11)
		http_get11(s, tmpbuf, keep_alive, &status, 0);
	else
		http_get10(s, tmpbuf, keep_alive, &status, 0);

	if (status != 200) {
		printf("ERROR: http:get %s = %d\n", tmpbuf, status);
		return;
	}

	int len = (int)atoi(session_get_stash(s, "HTTP_CONTENT_LENGTH"));
	sprintf(tmpbuf, "%s/%s", appname, filename);

	char tmpbuf2[256];
	strcpy(tmpbuf2, tmpbuf);
	*strrchr(tmpbuf2, '/') = '\0';
	mkdir(tmpbuf2, 0777);

	FILE *fp = fopen(tmpbuf, "wb");

	if (!fp)
		printf("ERROR: Can't create '%s'\n", tmpbuf);
	else
		printf("INFO: GET %s/%s\n", appname, filename);

	char *bufptr[1024 * 4];

	while ((len > 0) && !session_on_disconnect(s)) {
		int rlen = session_read(s, &bufptr, len > sizeof(bufptr) ? sizeof(bufptr) : len);

		if (rlen <= 0)
			break;

		if (fp)
			fwrite(bufptr, 1, rlen, fp);
		len -= rlen;
	}

	if (fp)
		fclose(fp);
}

static void manifest(session *s, const char *branch, const char *appname, int len)
{
	char *lines = (char *)malloc(len * 2);
	*lines = '\0';

	while (!session_on_disconnect(s) && (len > 0)) {
		char *line = NULL;

		if (!session_readmsg(s, &line))
			break;

		strcat(lines, line);
		len -= strlen(line);
		free(line);
	}

	if (!strlen(lines))
		return;

	const char *src = lines;
	char *dstbuf = (char *)malloc(strlen(lines) + 1);
	char *dst = dstbuf;

	while (*src) {
		if (isspace(*src)) {
			*dst = '\0';
			const char *filename = dstbuf;
			manifest_file(s, branch, appname, filename);
			dst = dstbuf;
			src++;
		}
		else
			*dst++ = *src++;
	}

	if (dst != dstbuf) {
		*dst = '\0';
		const char *filename = dstbuf;
		manifest_file(s, branch, appname, filename);
	}

	free(dstbuf);
	free(lines);
}
#endif

int main(int ac, char *av[])
{
	int daemon = 0, quiet = 0, verbose = 0;

#ifndef ISO_ONLY
	int startapp = 0, testapp = 0;
#endif

	for (int i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--"))
			break;
		else if (!strcmp(av[i], "-d") || !strcmp(av[i], "--daemon"))
			daemon = 1;
#ifndef ISO_ONLY
		else if (!strcmp(av[i], "--startapp"))
			startapp = 1;
		else if (!strcmp(av[i], "--testapp"))
			testapp = 1;
#endif
		else if (!strcmp(av[i], "-q") || !strcmp(av[i], "--quiet"))
			quiet = 1;
		else if (!strcmp(av[i], "-v") || !strcmp(av[i], "--verbose"))
			verbose = 1;
	}

	if (daemon) {
		if (!daemonize(ac, av))
			return 0;
	}
	else
		signal(SIGINT, &sigfn);

	trealla *pl = trealla_create(NULL);

	g_trealla_memlimit_mb = 2048;

#ifndef ISO_ONLY
	g_tpool_size = 4;
#endif

	const char *homedir;

	if ((homedir = getenv("HOME")) == NULL)
		homedir = ".";

	char histfile[1024];
	snprintf(histfile, sizeof(histfile), "%s/%s", homedir, ".tpl_history");
	char *init = NULL, *p1 = NULL;
	int trace = 0, stats = 0, noquery = 0, get = 0, ns = 0;
	trealla_optimize(pl, 3);

	for (int i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--"))
			break;
		else if (!strcmp(av[i], "--trace"))
			trace = 1;
		else if (!strcmp(av[i], "--swi7"))
			g_list_cons = "[|]";
		else if (!strcmp(av[i], "--ns"))
			ns = 1;
		else if (!strcmp(av[i], "--stats"))
			stats = 1;
#ifndef ISO_ONLY
		else if (!strncmp(av[i], "--dbdir=", 8))
			g_dbdir = av[i] + 8;
#endif
#if USE_SSL
		else if (!strcmp(av[i], "--unbounded"))
			g_force_unbounded = 1;
#endif
		else if (!strcmp(av[i], "-O0"))
			trealla_optimize(pl, 0);
		else if (!strcmp(av[i], "-O1") || !strcmp(av[i], "-O"))
			trealla_optimize(pl, 1);
		else if (!strcmp(av[i], "-O2"))
			trealla_optimize(pl, 2);
		else if (!strcmp(av[i], "-O3"))
			trealla_optimize(pl, 3);
		else if (!strcmp(av[i], "-O4"))
			trealla_optimize(pl, 4);
		else if (!strcmp(av[i], "--http11"))
			http11 = 1;
		else if (!strcmp(av[i], "--http10"))
			http11 = 0;
		else if (!strncmp(av[i], "--memM=", 7))
			sscanf(av[i], "%*[^=]=%d", &g_trealla_memlimit_mb);
		else if (!strncmp(av[i], "--memG=", 7)) {
			sscanf(av[i], "%*[^=]=%d", &g_trealla_memlimit_mb);
			g_trealla_memlimit_mb *= 1024;
		}
#ifndef ISO_ONLY
		else if (!strcmp(av[i], "--merge"))
			g_dbs_merge = 1;
		else if (!strncmp(av[i], "--tpool", 7))
			sscanf(av[i], "--tpool=%d", &g_tpool_size);
#endif
	}

	trealla_trace(pl, trace);
	trealla_quiet(pl, quiet);

#ifndef ISO_ONLY
	char *p2 = NULL;
	char *branch = (char *)"master";
	int install = 0, appget = 0;

	for (int i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--"))
			break;
		else if (!get && !install && !appget) {
			if (!strcmp(av[i], "appget"))
				appget = 1;
			else if (!strcmp(av[i], "get"))
				get = 1;
			else if (!strcmp(av[i], "install"))
				get = install = 1;
		}
		else if (av[i][0] != '-') {
			if (p1 == NULL)
				p1 = strdup(av[i]);
			else if (p2 == NULL)
				p2 = strdup(av[i]);
		}
	}

	if (appget) {
		const char *host = "raw.githubusercontent.com";
		int port = 443, tcp = 1, ssl = 1;
		session *s = session_open(host, port, tcp, ssl);

		if (!s) {
			printf("ERROR: can't connect 'https://%s", host);
			return 1;
		}

		session_set_stash(s, "HOST", host);
		char tmpbuf[256];
		sprintf(tmpbuf, "/trealla-lang/apps/%s/%s/MANIFEST", branch, p1);
		int status = 0, keep_alive = 1;

		if (http11)
			http_get11(s, tmpbuf, keep_alive, &status, 0);
		else
			http_get10(s, tmpbuf, keep_alive, &status, 0);

		if (status != 200) {
			printf("ERROR: http_get %s = %d\n", tmpbuf, status);
			return 1;
		}

		int len = (int)atoi(session_get_stash(s, "HTTP_CONTENT_LENGTH"));
		const char *appname = p1;
		manifest(s, branch, appname, len);
		session_close(s);
		return 0;
	}

	if (get) {
		const char *name = "http_client";
		char *src = trealla_find_library(name);

		if (src != NULL) {
			trealla_consult_text(pl, src, name);
			free(src);
		}

		char scheme[10], tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		strcpy(scheme, "http");

		if (!strncmp(p1, "https:", 6) || !strncmp(p1, "http:", 5))
			sscanf(p1, "%9[^:]://%255[^/]%255s", scheme, tmpbuf1, tmpbuf2);
		else
			sscanf(p1, "%255[^/]%255s", tmpbuf1, tmpbuf2);

		scheme[sizeof(scheme) - 1] = '\0';
		tmpbuf1[sizeof(tmpbuf1) - 1] = '\0';
		tmpbuf2[sizeof(tmpbuf2) - 1] = '\0';

		if (!p2)
			p2 = strdup(tmpbuf2 + 1);

		char tmpbuf[sizeof(tmpbuf1) + (sizeof(tmpbuf2) * 2) + 100];

		if (http11)
			snprintf(tmpbuf, sizeof(tmpbuf), "http_client:get11_file('%s://%s','%s','%s').", scheme, tmpbuf1, tmpbuf2, p2);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "http_client:get10_file('%s://%s','%s','%s').", scheme, tmpbuf1, tmpbuf2, p2);

		init = strdup(tmpbuf);
	}

	if (startapp) {
		trealla_consult_file(pl, "app_server");
		init = strdup("app_server:start.");
	}
	else if (testapp) {
		trealla_consult_file(pl, "app_server");
		init = strdup("app_server:test.");
	}

#endif

	int error = 0;

	for (int i = 1; i < ac; i++) {
		if (!strcmp(av[i], "--"))
			break;
		else if ((av[i][0] != '-') && !get) {
			if (!trealla_consult_file(pl, av[i])) {
				error = -1;
				break;
			}
		}
		else if (!strcmp(av[i], "--consult")) {
			if (!trealla_consult_fp(pl, stdin)) {
				error = -1;
				break;
			}
		}
		else if (!strcmp(av[i], "--noquery"))
			noquery = 1;
		else if (!strcmp(av[i], "-g")) {
			i++;
			char tmpbuf[1024];
			sprintf(tmpbuf, "%s.", av[i]);
			init = strdup(tmpbuf);
		}
		else if (!strncmp(av[i], "--goal=", 7)) {
			char tmpbuf[1024];
			sprintf(tmpbuf, "%s.", av[i] + 7);
			init = strdup(tmpbuf);
		}
		else if (!strncmp(av[i], "--load=", 7)) {
			char tmpbuf[1024];
			sprintf(tmpbuf, "%s.", av[i] + 7);

			if (!trealla_consult_file(pl, tmpbuf)) {
				error = -1;
				break;
			}
		}
		else if (!strcmp(av[i], "--start"))
			init = strdup("start.");
		else if (!strcmp(av[i], "--test"))
			init = strdup("test.");
	}

	if (init && !error) {
		tpl_query *q = trealla_create_query(pl);
		int ok = query_parse(q, init);
		free(init);

		if (ok)
			ok = query_run(q);

		if (verbose)
			printf("(%.3lf) ", query_elapsed(q));

		if (verbose)
			printf("%s\n", ok ? "true" : "false");

		if (stats)
			query_stats(q);

		query_destroy(q);

		if (p1)
			free(p1);
	}
	else if (!error && !noquery && !ns && !trealla_is_halt(pl)) {
		if (!quiet)
			printf("Trealla v%s, %s\n", g_trealla_version, __DATE__);

		if (isatty(0))
			history_load(histfile);

		lexer *l = lexer_create(pl);
		char *line;

		while ((line = trealla_readline(l, stdin, 0)) != NULL) {
			tpl_query *q = trealla_create_query(pl);
			int ok = query_parse_file(q, line, stdin);
			free(line);

			if (ok)
				ok = query_run(q);

			if (ok)
				query_dump(q);

			if (query_is_halt(q)) {
				query_destroy(q);
				break;
			}

			while (ok && isatty(0) && query_choices(q)) {
				printf("%s", " (abort=a,trace=t,next=;): ");
				int ch = history_getch();
				printf("\n");

				if (ch == 'a')
					break;
				else if (ch == 't')
					query_trace(q);
				else if ((ch == ';') || (ch = 'n')) {
					ok = query_continue(q);

					if (ok)
						query_dump(q);
				}
			}

			if (verbose)
				printf("(%.3lf) ", query_elapsed(q));

			if (isatty(0))
				history_output("", ok ? "true." : "false.");
			else
				printf("%s\n", ok ? "true." : "false.");

			if (stats)
				query_stats(q);

			query_destroy(q);
		}

		lexer_destroy(l);

		if (isatty(0))
			history_save();
	}

	int halt = trealla_get_haltcode(pl);
	trealla_destroy(pl);

#ifndef ISO_ONLY
	if (get) {
		struct stat st = {0};
		stat(p2, &st);
		printf("INFO: Downloaded %s : %lu bytes\n", p2, (long unsigned)st.st_size);
	}

	if (install) {
		char *tmpbuf = strdup(p2) + 20;
		sprintf(tmpbuf, "tar -vxf %s", p2);
		return system(tmpbuf) != 0;
	}

	if (p2)
		free(p2);
#endif

	if (!halt && (error > 0))
		halt = error;

	return halt;
}
