#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
#include <io.h>
#include <direct.h>
#define snprintf _snprintf
#define mkdir(a,b) _mkdir(a)
#else
#include <unistd.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "jela.h"
#include "bifs.h"
#include "daemon.h"
#include "history.h"

#ifndef ISO_ONLY
extern int g_dbs_merge;
extern int g_tpool_size;
#endif

static void sigfn(int s)
{
	signal(SIGINT, 0);
	g_abort = 1;
}

#ifdef _WIN32
static char *strndup (const char *s, size_t n)
{
	size_t len = strlen(s);
	if (n < len) len = n;
	char *dstbuf = (char*)malloc(len+1);
	dstbuf[len] = '\0';
	return (char*)memcpy(dstbuf, s, len);
}
#endif

#ifndef ISO_ONLY
static void manifest_file(session *s, const char *branch, const char *appname, const char *filename)
{
	if (filename[0] == '-')
	{
		printf("INFO: DELETE %s/%s\n", appname, filename+1);
		remove(filename+1);
		return;
	}

	if (filename[0] == '+')
	{
		struct stat st;

		if (stat(filename+1, &st) == 0)
			return;

		filename = filename+1;
	}

	char tmpbuf[256];
	sprintf(tmpbuf, "/trealla-lang/apps/%s/%s/%s", branch, appname, filename);
	int status = 0;
	http_get10(s, tmpbuf, 1, &status);

	if (status != 200)
	{
		printf("ERROR: get10 %s = %d\n", tmpbuf, status);
		return;
	}

	int len = (int)atoi(session_get_stash(s, "HTTP_CONTENT_LENGTH"));
	sprintf(tmpbuf, "%s/%s", appname, filename);

	char tmpbuf2[256];
	strcpy(tmpbuf2, tmpbuf);
	*strrchr(tmpbuf2, '/') = '\0';
	mkdir(tmpbuf2, 0777);

	FILE *fp = fopen(tmpbuf, "wb");
	if (!fp) printf("ERROR: Can't create '%s'\n", tmpbuf);
	else printf("INFO: GET %s/%s\n", appname, filename);
	char *bufptr[1024*4];

	while ((len > 0) && !session_on_disconnect(s))
	{
		int rlen = session_read(s, &bufptr, len>sizeof(bufptr)?sizeof(bufptr):len);
		if (rlen <= 0) break;
		if (fp) fwrite(bufptr, 1, rlen, fp);
		len -= rlen;
	}

	if (fp) fclose(fp);
}

static void manifest(session *s, const char *branch, const char *appname, int len)
{
	char *lines = (char*)malloc(len*2);
	*lines = '\0';

	while (!session_on_disconnect(s) && (len > 0))
	{
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
	char *dstbuf = (char*)malloc(strlen(lines)+1);
	char *dst = dstbuf;

	while (*src)
	{
		if (isspace(*src))
		{
			*dst = '\0';
			const char *filename = dstbuf;
			manifest_file(s, branch, appname, filename);
			dst = dstbuf;
			src++;
		}
		else
			*dst++ = *src++;
	}

	free(dstbuf);
	free(lines);
}
#endif

int main(int ac, char *av[])
{
	int daemon = 0, quiet = 0;

#ifndef ISO_ONLY
	int startapp = 0, testapp = 0;
#endif

	for (int i = 1; i < ac; i++)
	{
		if (!strcmp(av[i], "--sizes"))
		{
			printf("sizeof int=%d\n", (int)sizeof(int));
			printf("sizeof long=%d\n", (int)sizeof(long));
			printf("sizeof long long=%d\n", (int)sizeof(long long));
			printf("sizeof void*=%d\n", (int)sizeof(void*));
		}

		if (!strcmp(av[i], "-d") || !strcmp(av[i], "--daemon"))
			daemon = 1;
#ifndef ISO_ONLY
		else if (!strcmp(av[i], "--startapp"))
			startapp = 1;
		else if (!strcmp(av[i], "--testapp"))
			testapp = 1;
#endif
		else if (!strcmp(av[i], "-q") || !strcmp(av[i], "--quiet"))
			quiet = 1;
	}

	if (daemon)
	{
		if (!daemonize(ac, av))
			return 0;
	}
	else
		signal(SIGINT, &sigfn);

	trealla *pl = trealla_create(NULL);

	if (!quiet)
		printf("Trealla v%s, %u-bits (int=%u-bits), %s\n", g_trealla_version, (unsigned)sizeof(void*)*8, (unsigned)sizeof(nbr_t)*8, __DATE__);

	g_trealla_memlimit_mb = 1024;

#ifndef ISO_ONLY
	g_tpool_size = 2;
#endif

	const char *homedir;

	if ((homedir = getenv("HOME")) == NULL)
		homedir = ".";

	char histfile[1024];
	snprintf(histfile, sizeof(histfile), "%s/%s", homedir, ".tpl_history");
	char *init = NULL, *p1 = NULL;
	int trace = 0, stats = 0, noquery = 0, get = 0;

	for (int i = 1; i < ac; i++)
	{
		if (!strcmp(av[i], "--trace"))
			trace = 1;
		else if (!strcmp(av[i], "--stats"))
			stats = 1;
		else if (!strcmp(av[i], "--noopt") || !strcmp(av[i], "-O0"))
			trealla_noopt(pl, 1);
		else if (!strncmp(av[i], "--memM=",7))
			sscanf(av[i], "%*[^=]=%d", &g_trealla_memlimit_mb);
		else if (!strncmp(av[i], "--memG=",7))
		{
			sscanf(av[i], "%*[^=]=%d", &g_trealla_memlimit_mb);
			g_trealla_memlimit_mb *= 1024;
		}
#ifndef ISO_ONLY
		else if (!strcmp(av[i], "--merge"))
			g_dbs_merge = 1;
		else if (!strncmp(av[i], "--tpool",7))
			sscanf(av[i], "--tpool=%d", &g_tpool_size);
#endif
	}

	trealla_trace(pl, trace);

#ifndef ISO_ONLY
	char *p2 = NULL;
	char *branch = (char*)"master";
	int install = 0, appget = 0;
	srand(time(NULL));

	for (int i = 1; i < ac; i++)
	{
		if (!get && !install && !appget)
		{
			if (!strcmp(av[i], "appget"))
				appget = 1;
			else if (!strcmp(av[i], "get"))
				get = 1;
			else if (!strcmp(av[i], "install"))
				get = install = 1;
		}
		else if (av[i][0] != '-')
		{
			if (p1 == NULL)
				p1 = strdup(av[i]);
			else if (p2 == NULL)
				p2 = strdup(av[i]);
		}
	}

	if (appget)
	{
		const char *host = "raw.githubusercontent.com";
		int port = 443, tcp = 1, ssl = 1;
		session *s = session_open(host, port, tcp, ssl);
		if (!s) { printf("ERROR: can't connect 'https://%s", host); return 1; }
		session_set_stash(s, "HOST", host);
		//printf("INFO: Connected https://%s\n", host);
		char tmpbuf[256];
		sprintf(tmpbuf, "/trealla-lang/apps/%s/%s/MANIFEST", branch, p1);
		int status = 0, keep_alive = 1;
		http_get10(s, tmpbuf, keep_alive, &status);

		if (status != 200)
		{
			printf("ERROR: get10 %s = %d\n", tmpbuf, status);
			return 1;
		}

		int len = (int)atoi(session_get_stash(s, "HTTP_CONTENT_LENGTH"));
		//printf("INFO: GET %s HTTP/1.0\n", tmpbuf);
		//printf("INFO: HTTP Status=%d, Content-Length=%d\n", status, len);
		const char *appname = p1;
		manifest(s, branch, appname, len);
		session_close(s);
		return 0;
	}

	if (get)
	{
		char scheme[10], tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		strcpy(scheme, "http");

		if (!strncmp(p1, "https:", 6) || !strncmp(p1, "http:", 5))
			sscanf(p1, "%9[^:]://%255[^/]%255s", scheme, tmpbuf1, tmpbuf2);
		else
			sscanf(p1, "%255[^/]%255s", tmpbuf1, tmpbuf2);

		scheme[sizeof(scheme)-1] = '\0';
		tmpbuf1[sizeof(tmpbuf1)-1] = '\0';
		tmpbuf2[sizeof(tmpbuf2)-1] = '\0';

		if (!p2)
			p2 = strdup(tmpbuf2+1);

		char tmpbuf[sizeof(tmpbuf1)+(sizeof(tmpbuf2)*2)+100];
		snprintf(tmpbuf, sizeof(tmpbuf), "http_client:get10_file('%s://%s','%s','%s')",
			scheme, tmpbuf1, tmpbuf2, p2);
		init = strdup(tmpbuf);
	}

	if (startapp)
	{
		trealla_consult_file(pl, "app");
		init = strdup("app_server:start");
	}
	else if (testapp)
	{
		trealla_consult_file(pl, "app");
		init = strdup("app_server:test");
	}

#endif

	for (int i = 1; i < ac; i++)
	{
		if ((av[i][0] != '-') && !get)
			trealla_consult_file(pl, av[i]);
		else if (!strcmp(av[i], "--consult"))
			trealla_consult_fp(pl, stdin);
		else if (!strcmp(av[i], "--noquery"))
			noquery = 1;
		else if (!strncmp(av[i], "--goal=", 7))
			init = strdup(av[i]+7);
		else if (!strncmp(av[i], "-g=", 3))
			init = strdup(av[i]+3);
		else if (!strcmp(av[i], "--start"))
			init = strdup("start");
		else if (!strcmp(av[i], "--test"))
			init = strdup("test");
		else if (!strcmp(av[i], "--listing"))
			init = strdup("listing");
		else if (!strcmp(av[i], "--listing_canonical"))
			init = strdup("listing_canonical");
	}

	if (init)
	{
		tpl_query *q = trealla_create_query(pl);
		if (!q) return 1;
		int ok = query_parse(q, init);
		if (ok) ok = query_run(q);
		if (!quiet) printf("(%.3lf s) %s\n", query_elapsed(q), ok?"yes":"no");
		if (stats) query_stats(q);
		query_destroy(q);
		free(init);
		if (p1) free(p1);
	}
	else if (!noquery && !pl->abort)
	{
		history_load(histfile);
		char *line;

		while ((line = (pl->tty ?
				history_readline_eol("?- ", '.') :
				trealla_readline(stdin))) != NULL)
		{
			const char *src = line;

			while (isspace(*src))
				src++;

			tpl_query *q = trealla_create_query(pl);
			if (!q) break;
			int ok = query_parse(q, src);
			if (ok) ok = query_run(q);
			if (ok) query_dump(q);
			free(line);

			if (pl->abort || (q->halt == ABORT_HALT))
			{
				query_destroy(q);
				break;
			}

			while (ok && query_choices(q) && pl->tty)
			{
				printf("%s", "(abort=a,trace=t,next=;): ");
				char ch = history_getch();
				printf("\n");

				if (ch == 'a')
					break;
				else if (ch == 't')
					query_trace(q);
				else if ((ch == ';') || (ch = 'n'))
				{
					ok = query_continue(q);
					if (ok) query_dump(q);
				}
			}

			printf("\n(%.3lf s) %s\n", query_elapsed(q), ok?"yes":"no");
			if (stats) query_stats(q);
			query_destroy(q);
		}

		history_save();
	}

	int halt_code = pl->halt_code;
	trealla_destroy(pl);

#ifndef ISO_ONLY
	if (get)
	{
		struct stat st = {0};
		stat(p2, &st);
		printf("INFO: Downloaded %s : %lu bytes\n", p2, (long unsigned)st.st_size);
	}

	if (install)
	{
		char *tmpbuf = strdup(p2)+20;
		sprintf(tmpbuf, "tar -vxf %s", p2);
		return system(tmpbuf) != 0;
	}

	if (p2) free(p2);
#endif

	if (halt_code != 0)
		exit(halt_code);

	return 1;
}
