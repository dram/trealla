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
#include "base64.h"
#include "network.h"
#include "internal.h"
#include "bifs.h"
#include "jela.h"

#define MAX_CHUNK_SIZE (1024LL*1024*1024)			// 1GB
#define MAX_FORM_SIZE (1024LL*1024*64)				// 64MB

static char *sanitize(const char *path, char *path2)
{
	const char *src = path;
	char *dst = path2;

	while (isspace(*src))
		src++;

	while (*src)
	{
		if ((src[0] == '\'') || (src[0] == '\"') || (src[0] == '~'))
			;
		else if ((src[0] != '.') || (src[1] != '.'))
			*dst++ = *src;

		src++;
	}

	while ((dst != path2) && isspace(*(dst-1)))
		dst--;

	*dst = '\0';
	return path2;
}

static char *http_cleanup(const char *path, char *path2)
{
	const char *src = path;
	char *dst = path2;

	while (isspace(*src))
		src++;

	while (*src)
		*dst++ = *src++;

	while ((dst != path2) && isspace(*(dst-1)))
		dst--;

	*dst = '\0';
	return path2;
}

static int bif_http_query3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	char tmpbuf[KEY_SIZE];
	snprintf(tmpbuf, KEY_SIZE-20, "QUERY:%s", term2->val_s);
	const char *s = session_get_stash((session*)sp->sptr, tmpbuf);
	put_atom(q, q->curr_frame+term3->slot, strdup(s), 1);
	return 1;
}

static int bif_http_form3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	char tmpbuf[KEY_SIZE];
	snprintf(tmpbuf, KEY_SIZE-20, "FORM:%s", term2->val_s);
	const char *s = session_get_stash((session*)sp->sptr, tmpbuf);
	put_atom(q, q->curr_frame+term3->slot, strdup(s), 1);
	return 1;
}

static int bif_http_www_form1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	stream *sp = term1->val_str;

	if (session_on_disconnect((session*)sp->sptr))
	{
		q->is_yielded = 1;
		return 0;
	}

	long long nbytes = session_get_stash_int((session*)sp->sptr, "Content-Length");

	if (nbytes > MAX_FORM_SIZE)
	{
		session_close((session*)sp->sptr);
		return 0;
	}

	long save_len = (long)nbytes;
	char *bufptr = (char*)malloc(nbytes+1);
	char *dst = bufptr;

	while (nbytes > 0)
	{
		int len = session_read((session*)sp->sptr, dst, nbytes);
		dst += len;
		*dst = '\0';
		nbytes -= len;

		if (!len)
			msleep(1);
	}

	const char *src = bufptr;
	char *name = (char*)malloc(save_len+1);
	char *value = (char*)malloc(save_len+1);
	char *tmpname = (char*)malloc(save_len+1);
	char *tmpvalue = (char*)malloc(save_len+1);
	dst = name;

	while (*src)
	{
		if (*src == '=')
		{
			*dst = '\0';
			dst = value;
			src++;
		}
		else if (*src == '&')
		{
			*dst = '\0';
			char tmpbuf[KEY_SIZE], tmpbuf1[KEY_SIZE], tmpbuf2[KEY_SIZE];
			snprintf(tmpbuf, KEY_SIZE-20, "FORM:%s", url_decode(name,tmpbuf1));
			session_set_stash((session*)sp->sptr, tmpbuf, url_decode(value, tmpbuf2));
			dst = name;
			src++;
		}
		else
			*dst++ = *src++;
	}

	*dst = '\0';

	if (name[0])
	{
		char tmpbuf[KEY_SIZE], tmpbuf1[KEY_SIZE], tmpbuf2[KEY_SIZE];
		snprintf(tmpbuf, KEY_SIZE-20, "FORM:%s", url_decode(name,tmpbuf1));
		session_set_stash((session*)sp->sptr, tmpbuf, url_decode(value, tmpbuf2));
	}

	free(tmpvalue);
	free(tmpname);
	free(value);
	free(name);
	free(bufptr);
	return 1;
}

static int bif_http_cookie3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	char tmpbuf[KEY_SIZE];
	snprintf(tmpbuf, KEY_SIZE-20, "COOKIE:%s", term2->val_s);
	const char *s = session_get_stash((session*)sp->sptr, tmpbuf);
	put_atom(q, q->curr_frame+term3->slot, strdup(s), 1);
	return 1;
}

static void parse_cookies(session *s, const char *src)
{
	size_t save_len = strlen(src);
	char *name = (char*)malloc(save_len+1);
	char *value = (char*)malloc(save_len+1);
	char *dst = name;
	name[0] = value[0] = '\0';
	int quoted = 0;

	while (*src)
	{
		char ch = *src++;

		if (ch == '=')
		{
			*dst = '\0';
			dst = value;
			*dst = '\0';

			if (*src == '"')
			{
				quoted = 1;
				src++;
			}

			continue;
		}
		else if (quoted && (ch == '"'))
		{
			quoted = 0;
			continue;
		}
		else if (ch == ';')
		{
			*dst = '\0';
			char tmpbuf[KEY_SIZE];
			snprintf(tmpbuf, KEY_SIZE-20, "COOKIE:%s", name);
			session_set_stash(s, tmpbuf, value);
			dst = name;
			*dst = '\0';
			value[0] = '\0';
			continue;
		}

		*dst++ = ch;
	}

	*dst = '\0';

	if (name[0])
	{
		char tmpbuf[KEY_SIZE];
		snprintf(tmpbuf, KEY_SIZE-20, "COOKIE:%s", name);
		session_set_stash(s, tmpbuf, value);
	}

	free(name);
	free(value);
}

static void parse_header(session *s, char *bufptr, int len)
{
	const char *src = strchr(bufptr, ':');
	if (!src) { free(bufptr); return; }
	src = bufptr;
	char *name = (char*)malloc(len+1);
	char *dst = name;

	while (isspace(*src))
		src++;

	while (*src && !isspace(*src) && (*src != ':'))
	{
		*dst++ = tolower(*src);
		src++;
	}

	*dst = '\0';

	while (isspace(*src))
		src++;

	src++;			// the ':'

	while (isspace(*src))
		src++;

	char *value = (char*)malloc(len+1);
	dst = value;

	while (*src && (*src != '\r') && (*src != '\n'))
		*dst++ = *src++;

	while ((dst != value) && isspace(*(dst-1)))
		dst--;

	*dst = '\0';
	free(bufptr);
	session_set_stash(s, name, value);

	if (!strcmp(name, "Cookie"))
		parse_cookies(s, value);

	free(name);
	free(value);
}

static int bif_http_parse4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	node *term4 = get_var(term4);
	stream *sp = term1->val_str;
	char *bufptr = NULL;

	if (session_on_disconnect((session*)sp->sptr))
	{
		q->is_yielded = 1;
		return 0;
	}

	int len;

	while ((len = session_readmsg((session*)sp->sptr, &bufptr)) > 0)
	{
		//printf("***> %s", bufptr);

		if (!session_get_udata_flag((session*)sp->sptr, CMD))
		{
			session_clr_stash((session*)sp->sptr);
			session_set_udata_flag((session*)sp->sptr, CMD);

			char cmd[256], ver[20], tmpbuf[256];
			char *path=(char*)malloc(len+1);
			char *path2=(char*)malloc(len+1);
			char *full_path=(char*)malloc(len+1);
			cmd[0] = path[0] = full_path[0] = path2[0] = ver[0] = '\0';
			sscanf(bufptr, "%255s %s HTTP/%19s", cmd, full_path, ver);
			free(bufptr);
			cmd[sizeof(cmd)-1] = ver[sizeof(ver)-1] = full_path[len] = '\0';
			sscanf(full_path, "%[^? \r\n]", path);
			url_decode(path, path2);
			sanitize(path2, path);
			free(path2);
			const char *ptr = strchr(full_path, '?');

			if (session_is_tls((session*)sp->sptr))
				session_set_stash((session*)sp->sptr, "SERVER_PROTOCOL", "https");
			else
				session_set_stash((session*)sp->sptr, "SERVER_PROTOCOL", "http");

			session_set_stash((session*)sp->sptr, "REQUEST_METHOD", cmd);
			session_set_stash((session*)sp->sptr, "HTTP", ver);
			session_set_stash((session*)sp->sptr, "PATH_INFO", path);
			session_set_stash((session*)sp->sptr, "PATH_TRANSLATED", path);
			session_set_stash((session*)sp->sptr, "QUERY_STRING", ptr?ptr:"");

			if (session_is_ipv6((session*)sp->sptr))
			{
				snprintf(tmpbuf, sizeof(tmpbuf), "[%s]", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "REMOTE_ADDR", tmpbuf);
				session_set_stash((session*)sp->sptr, "REMOTE_HOST", tmpbuf);
				snprintf(tmpbuf, sizeof(tmpbuf), "[%s]", session_get_local_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_ADDR", tmpbuf);
				session_set_stash((session*)sp->sptr, "LOCAL_HOST", tmpbuf);
			}
			else
			{
				session_set_stash((session*)sp->sptr, "REMOTE_ADDR", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "REMOTE_HOST", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_ADDR", session_get_local_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_HOST", session_get_local_addr((session*)sp->sptr, 0));
			}

			session_set_stash_int((session*)sp->sptr, "REMOTE_PORT", session_get_remote_port((session*)sp->sptr));
			session_set_stash_int((session*)sp->sptr, "LOCAL_PORT", session_get_local_port((session*)sp->sptr));

			if (ptr)
			{
				char *name =(char*)malloc(len+64);
				char *value =(char*)malloc(len+1);
				const char *src = ptr+1;
				char *dst = name;

				while (*src)
				{
					if (*src == '=')
					{
						*dst = '\0';
						dst = value;
						src++;
					}
					else if (*src == '&')
					{
						*dst = '\0';
						char tmpbuf[KEY_SIZE], tmpbuf1[KEY_SIZE], tmpbuf2[KEY_SIZE];
						snprintf(tmpbuf, KEY_SIZE-20, "QUERY:%s", url_decode(name,tmpbuf1));
						session_set_stash((session*)sp->sptr, tmpbuf, url_decode(value, tmpbuf2));
						dst = name;
						src++;
					}
					else
						*dst++ = *src++;
				}

				*dst = '\0';

				if (name[0])
				{
					char tmpbuf[KEY_SIZE], tmpbuf1[KEY_SIZE], tmpbuf2[KEY_SIZE];
					snprintf(tmpbuf, KEY_SIZE-20, "QUERY:%s", url_decode(name,tmpbuf1));
					session_set_stash((session*)sp->sptr, tmpbuf, url_decode(value, tmpbuf2));
				}

				free(name);
				free(value);
			}

			free(path);
			free(full_path);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag((session*)sp->sptr, CMD);
			char tmpbuf[1024], tmpbuf2[1024], tmpbuf3[256];
			tmpbuf2[0] = tmpbuf3[0] = '\0';
			const char *src = session_get_stash((session*)sp->sptr, "Host");
			if (*src) sscanf(src, "%1023[^: \r\n]:%255s", tmpbuf2, tmpbuf3);
			tmpbuf2[sizeof(tmpbuf2)-1] = tmpbuf3[sizeof(tmpbuf3)-1] = '\0';

			if (tmpbuf2[0])
				session_set_stash((session*)sp->sptr, "SERVER_NAME", http_cleanup(tmpbuf2, tmpbuf));
			else
				session_set_stash((session*)sp->sptr, "SERVER_NAME", session_get_stash((session*)sp->sptr, "LOCAL_ADDRESS"));

			if (tmpbuf3[0])
				session_set_stash((session*)sp->sptr, "SERVER_PORT", http_cleanup(tmpbuf3, tmpbuf));
			else
				session_set_stash((session*)sp->sptr, "SERVER_PORT", session_get_stash((session*)sp->sptr, "LOCAL_PORT"));

			if (strlen(session_get_stash((session*)sp->sptr, "Authorization")))
			{
				const char *hdr = session_get_stash((session*)sp->sptr, "Authorization");
				char auth[256], tmpbuf[256];
				auth[0] = tmpbuf[0] = '\0';
				sscanf(hdr, "%255s %255s", auth, tmpbuf);
				auth[sizeof(auth)-1] = tmpbuf[sizeof(tmpbuf)-1] = '\0';

				if (!strcmp(auth, "Basic"))
				{
					size_t nbytes = 64;
					char *dstbuf = (char*)malloc(nbytes);
					b64_decode(tmpbuf, strlen(tmpbuf), (char**)&dstbuf);
					char userid[256], passwd[256];
					userid[0] = passwd[0] = '\0';
					sscanf(dstbuf, "%255[^:]:%255s", userid, passwd);
					userid[sizeof(userid)-1] = passwd[sizeof(passwd)-1] = '\0';
					session_set_stash((session*)sp->sptr, "USER", userid);
					session_set_stash((session*)sp->sptr, "PASS", passwd);
					free(dstbuf);
				}
			}

			if (strlen(session_get_stash((session*)sp->sptr, "WWW-Authenticate")))
			{
				const char *hdr = session_get_stash((session*)sp->sptr, "WWW-Authenticate");
				char auth[256], tmpbuf[256];
				auth[0] = tmpbuf[0] = '\0';
				sscanf(hdr, "%255s %255s", auth, tmpbuf);
				auth[sizeof(auth)-1] = tmpbuf[sizeof(tmpbuf)-1] = '\0';

				if (!strcmp(auth, "Basic") && !strncmp(tmpbuf, "Realm", 5))
				{
					const char *src = tmpbuf;
					char *dst = auth;

					while (isalpha(*src) || (*src == '='))
						src++;

					if (*src == '"')
						src++;

					while (*src && (*src != '"') && ((dst-auth)<(sizeof(auth)-1)))
						*dst++ = *src++;

					*dst = '\0';
					session_set_stash((session*)sp->sptr, "REALM", auth);
				}
			}

			if (strlen(session_get_stash((session*)sp->sptr, "WWW-Authenticate"))) session_set_stash((session*)sp->sptr, "WWW_AUTHENTICATE", http_cleanup(session_get_stash((session*)sp->sptr, "WWW-Authenticate"), tmpbuf));
			if (strlen(session_get_stash((session*)sp->sptr, "Authorization"))) session_set_stash((session*)sp->sptr, "AUTHORIZATION", http_cleanup(session_get_stash((session*)sp->sptr, "Authorization"), tmpbuf));
			if (strlen(session_get_stash((session*)sp->sptr, "Referer"))) session_set_stash((session*)sp->sptr, "REFERER", http_cleanup(session_get_stash((session*)sp->sptr, "Referer"), tmpbuf));
			if (strlen(session_get_stash((session*)sp->sptr, "User-Agent"))) session_set_stash((session*)sp->sptr, "USER_AGENT", http_cleanup(session_get_stash((session*)sp->sptr, "User-Agent"), tmpbuf));
			if (strlen(session_get_stash((session*)sp->sptr, "Upgrade"))) session_set_stash((session*)sp->sptr, "UPGRADE", http_cleanup(session_get_stash((session*)sp->sptr, "Upgrade"), tmpbuf));
			if (strlen(session_get_stash((session*)sp->sptr, "Origin"))) session_set_stash((session*)sp->sptr, "ORIGIN", http_cleanup(session_get_stash((session*)sp->sptr, "Origin"), tmpbuf));

			if (strlen(session_get_stash((session*)sp->sptr, "Connection")))
				;
			else if (atof(session_get_stash((session*)sp->sptr, "HTTP")) < 1.1)
				session_set_stash((session*)sp->sptr, "Connection", "close");
			else
				session_set_stash((session*)sp->sptr, "Connection", "keep-alive");

			if (strlen(session_get_stash((session*)sp->sptr, "Content-Length"))) session_set_stash((session*)sp->sptr, "CONTENT_LENGTH", session_get_stash((session*)sp->sptr, "Content-Length"));
			if (strlen(session_get_stash((session*)sp->sptr, "Content-Type"))) session_set_stash((session*)sp->sptr, "CONTENT_TYPE", session_get_stash((session*)sp->sptr, "Content-Type"));

			put_float(q, q->curr_frame+term2->slot, (double)atof(session_get_stash((session*)sp->sptr, "HTTP")));
			put_atom(q, q->curr_frame+term3->slot, strdup(session_get_stash((session*)sp->sptr, "REQUEST_METHOD")), 1);
			put_atom(q, q->curr_frame+term4->slot, strdup(session_get_stash((session*)sp->sptr, "PATH_INFO")), 1);
			return 1;
		}

		parse_header((session*)sp->sptr, bufptr, len);
	}

	q->is_yielded = 1;
	return 0;
}

int http_get10(session *s, const char *path, int keep, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "GET %s HTTP/1.0\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\n");
	sprintf(dst, "Connection: %s\r\n\r\n", keep?"keep-alive":"close");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_get103(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_get10((session*)sp->sptr, term2->val_s, 1, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int http_head10(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "HOST");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "HEAD %s HTTP/1.0\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\nConnection: keep-alive\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_head103(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_head10((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int http_get11(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "HOST");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "GET %s HTTP/1.1\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\nAccept-Encoding: chunked\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_get113(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_get11((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int http_head11(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "HEAD %s HTTP/1.1\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_head113(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_head11((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int bif_http_get_chunk3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	char *line = NULL;

	// Read length line

	if (!session_readmsg((session*)sp->sptr, &line))
	{
		q->is_yielded = 1;
		return 0;
	}

	//printf("+$$$ %s", line);
	unsigned long long len = 0;
	sscanf(line, "%LX", &len);
	free(line);

	if (len > MAX_CHUNK_SIZE)
	{
		session_close((session*)sp->sptr);
		return 0;
	}

	put_int(q, q->curr_frame+term3->slot, len);
	int save_len = len;

	// Read for specified length

	char *bufptr = (char*)malloc(len+1);
	char *dst = bufptr;

	while (len > 0)
	{
		size_t rlen = 0;

		if (!(rlen = session_read((session*)sp->sptr, dst, len)))
		{
			if (session_on_disconnect((session*)sp->sptr))
			{
				free(bufptr);
				return 0;
			}
		}

		//printf("@@@ len=%d, rlen=%d\n", len, (int)rlen);
		dst += rlen;
		len -= rlen;

		if (len && !rlen)
			msleep(1);
	}

	*dst = '\0';
	node *tmp = make_blob(bufptr, save_len);
	put_env(q, q->curr_frame+term2->slot, tmp, -1);
	tmp->refcnt--;
	line = NULL;

	if (!session_readmsg((session*)sp->sptr, &line))
	{
		q->is_yielded = 1;
		return 0;
	}

	//printf("-$$$ %s", line);
	free(line);
	return 1;
}

static int bif_http_put_chunk3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_int(term3);
	stream *sp = term1->val_str;

	session_set_cork((session*)sp->sptr, 1);

	char tmpbuf[256];
	snprintf(tmpbuf, sizeof(tmpbuf), "%x\r\n", (unsigned)term3->val_i);

	if (!session_writemsg((session*)sp->sptr, tmpbuf))
	{
		session_set_cork((session*)sp->sptr, 0);
		return 0;
	}

	if (term3->val_i > 0)
	{
		if (!session_write((session*)sp->sptr, term2->val_s, term3->val_i))
		{
			session_set_cork((session*)sp->sptr, 0);
			return 0;
		}
	}

	snprintf(tmpbuf, sizeof(tmpbuf), "\r\n");

	if (!session_writemsg((session*)sp->sptr, tmpbuf))
	{
		session_set_cork((session*)sp->sptr, 0);
		return 0;
	}

	session_set_cork((session*)sp->sptr, 0);
	return 1;
}

static int bif_http_put_chunk2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;

	session_set_cork((session*)sp->sptr, 1);

	char tmpbuf[256];
	snprintf(tmpbuf, sizeof(tmpbuf), "%x\r\n", (unsigned)term2->val_len);

	if (!session_writemsg((session*)sp->sptr, tmpbuf))
	{
		session_set_cork((session*)sp->sptr, 0);
		return 0;
	}

	if (!session_write((session*)sp->sptr, term2->val_s, LEN(term2)))
	{
		session_set_cork((session*)sp->sptr, 0);
		return 0;
	}

	snprintf(tmpbuf, sizeof(tmpbuf), "\r\n");

	if (!session_writemsg((session*)sp->sptr, tmpbuf))
	{
		session_set_cork((session*)sp->sptr, 0);
		return 0;
	}

	session_set_cork((session*)sp->sptr, 0);
	return 1;
}

static int bif_http_put_file2(tpl_query *q)
{
	node *args = get_args(q);
	node *var;								// FLAG_HIDDEN
	FILE *fp;

	if (!q->retry)
	{
		var = get_var(var);
	}
	else
	{
		var = get_stream(var);
	}

	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str, *sp2;

	if (!q->retry)
	{
		fp = fopen(term2->val_s, "rb");

		if (fp == NULL)
			{ QABORT(ABORT_NOTEXISTFILE); return 0; }

		sp2 = CALLOC(stream);
		sp2->fptr = fp;
		node *n = make_stream(sp2);
		put_env(q, q->curr_frame+var->slot, n, -1);
		n->refcnt--;
		allocate_frame(q);
	}
	else
	{
		sp2 = var->val_str;
		fp = sp2->fptr;
	}

	char tmpbuf[1024*8];

	if (!q->retry)
	{
		struct stat st;
		stat(term2->val_s, &st);
		snprintf(tmpbuf, sizeof(tmpbuf), "%x\r\n", (unsigned)st.st_size);

		if (!session_writemsg((session*)sp->sptr, tmpbuf))
			return 0;
	}

	size_t rlen;

	while ((rlen = fread(tmpbuf, 1, sizeof(tmpbuf), fp)) > 0)
	{
		const char *src = tmpbuf;

		while (rlen > 0)
		{
			int wlen = session_rawwrite((session*)sp->sptr, src, rlen);

			if (wlen < 0)
				return 0;

			if (!wlen)
			{
				fseek(fp, -rlen, SEEK_CUR);
				process_yield_unlocked(q);
				return 0;
			}

			rlen -= wlen;
			src += wlen;
		}
	}

	snprintf(tmpbuf, sizeof(tmpbuf), "\r\n0\r\n\r\n");

	if (!session_writemsg((session*)sp->sptr, tmpbuf))
		return 0;

	fclose(fp);
	sp2->fptr = NULL;
	return 1;
}

static int http_put10(session *s, const char *path, const char *cttype, int64_t ctlen, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "PUT %s HTTP/1.0\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	dst += snprintf(dst, 256, "Content-Type: %s\r\n", cttype);

	if (ctlen >= 0)
		dst += sprintf(dst, "Content-Length: %lu\r\n", (long unsigned)ctlen);

	sprintf(dst, "User-Agent: Trealla\r\nConnection: keep-alive\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_put105(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	node *term4 = get_int(term4);
	node *term5 = get_var(term5);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_put10((session*)sp->sptr, term2->val_s, term3->val_s, term4->val_i, &status);
	put_int(q, q->curr_frame+term5->slot, status);
	return ok;
}

static int http_put11(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "PUT %s HTTP/1.1\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	dst += sprintf(dst, "Transfer-Encoding: chunked\r\n");
	sprintf(dst, "User-Agent: Trealla\r\nConnection: keep-alive\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_put113(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_put11((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int http_delete10(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "DELETE %s HTTP/1.0\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\nConnection: keep-alive\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_delete105(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_delete10((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int http_delete11(session *s, const char *path, int *status)
{
	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "DELETE %s HTTP/1.1\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	sprintf(dst, "User-Agent: Trealla\r\nConnection: keep-alive\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);
	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "%d", *status);
			session_set_stash(s, "STATUS", tmpbuf);
			session_set_stash(s, "HTTP", ver);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (strlen(session_get_stash(s, "Content-Length"))) session_set_stash(s, "CONTENT_LENGTH", session_get_stash(s, "Content-Length"));
			if (strlen(session_get_stash(s, "Content-Type"))) session_set_stash(s, "CONTENT_TYPE", session_get_stash(s, "Content-Type"));
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int bif_http_delete113(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	int status = 0;
	int ok = http_delete11((session*)sp->sptr, term2->val_s, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return ok;
}

static int bif_ws_msg3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	stream *sp = term1->val_str;
	const char *op = term2->val_s;
	int bin = is_blob(term3);
	const char *src = term3->val_s;
	const uint64_t len = LEN(term3);

	if (!strcmp(op, "ping"))
	{
		if (!session_ws_ping((session*)sp->sptr, src, len))
			return 0;
	}
	else if (!strcmp(op, "close"))
	{
		if (!session_ws_close((session*)sp->sptr, src, len))
			return 0;
	}
	else if (!strcmp(op, "data"))
	{
		if (!session_ws_data((session*)sp->sptr, 1, bin, src, len))
			return 0;
	}
	else if (!strcmp(op, "more"))
	{
		if (!session_ws_data((session*)sp->sptr, 0, bin, src, len))
			return 0;
	}
	else
		return 0;

	return 1;
}

#if USE_SSL
static int ws_request(session *s, const char *path, const char *prots, int *status)
{
	char tmpbuf[256];
	snprintf(tmpbuf, sizeof(tmpbuf), "%lld-%d", (long long)time(NULL), rand());
	char key[256];
	key[0] = '\0';
	char *ptr = key;
	b64_encode(tmpbuf, strlen(tmpbuf), &ptr, 0, 0);

	const char *host = session_get_stash(s, "SERVER_NAME");
	const char *user = session_get_stash(s, "USER");
	const char *pass = session_get_stash(s, "PASS");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024*4, "GET %s HTTP/1.1\r\n", path);
	dst += snprintf(dst, 256, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	//dst += snprintf(dst, 256, "Origin: http://%s\r\n", host);
	dst += sprintf(dst, "Sec-WebSocket-Key: %s\r\n", key);
	if (prots) dst += snprintf(dst, 256, "Sec-WebSocket-Protocol: %s\r\n", prots);
	dst += sprintf(dst, "Sec-WebSocket-Version: %d\r\n", 13);
	sprintf(dst, "Upgrade: websocket\r\nConnection: Upgrade\r\nUser-Agent: Trealla\r\n\r\n");
	session_writemsg(s, dstbuf);
	//printf("%s", dstbuf);

	strcpy(dstbuf, key);
	strcat(dstbuf, WS_GUID);
	unsigned char digest[SHA_DIGEST_LENGTH];
	SHA1((unsigned char*)dstbuf, strlen(dstbuf), digest);
	ptr = key;
	b64_encode((const char*)digest, sizeof(digest), &ptr, 0, 0);

	char *bufptr = NULL;
	int len;

	while ((len = session_readmsg(s, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag(s, CMD))
		{
			session_set_udata_flag(s, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag(s, CMD);
			if (*status != 101) return 1;
			const char *tmpkey = session_get_stash(s, "Sec-WebSocket-Accept");

			if (strcmp(key, tmpkey))
			{
				printf("*** WARN security missmatch, %s != %s\n", key, tmpkey);
				return 0;
			}

			session_set_websocket(s);
			return 1;
		}

		parse_header(s, bufptr, len);
	}

	return 0;
}

static int ws_upgrade(session *s, const char *prot)
{
	const char *sec_key = session_get_stash(s, "Sec-WebSocket-Key");
	char dstbuf[1024*8];
	char *dst = dstbuf;
	dst += snprintf(dst, 1024, "%s", sec_key);
	dst += sprintf(dst, "%s", WS_GUID);
	unsigned char digest[SHA_DIGEST_LENGTH];
	SHA1((unsigned char*)dstbuf, dst-dstbuf, digest);
	char key[256];
	char *ptr = key;
	b64_encode((const char*)digest, sizeof(digest), &ptr, 0, 0);

	dst = dstbuf;
	dst += sprintf(dst, "HTTP/1.1 101 Switching Protocols\r\n");
	dst += snprintf(dst, 256, "Sec-WebSocket-Protocol: %s\r\n", prot);
	dst += sprintf(dst, "Sec-WebSocket-Accept: %s\r\n", key);
	strcat(dst, "Upgrade: websocket\r\nConnection: Upgrade\r\nServer: Trealla\r\n\r\n");
	session_writemsg(s, dstbuf);
	session_set_websocket(s);
	//printf("%s", dstbuf);
	return 1;
}

static int bif_ws_request5(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	node *term4 = get_atom_or_list(term4);
	node *term5 = get_var(term5);
	stream *sp = term1->val_str;
	const char *path = term2->val_s;

	char prots[256];
	prots[0] = '\0';

	if (is_atom(term4) && strcmp(term4->val_s, "[]"))
		strcat(prots, term4->val_s);

	node *l = term4;

	while (is_list(l))
	{
		node *head = NLIST_NEXT(NLIST_FRONT(&l->val_l));
		node *n = get_arg(q, head, q->curr_frame);

		if (is_atom(n))
		{
			if (prots[0])
				strcat(prots, ",");

			strcat(prots, n->val_s);
		}

		node *tail = NLIST_NEXT(head);
		l = get_arg(q, tail, q->latest_context);
	}

	int status;

	int ok = ws_request((session*)sp->sptr, path, prots, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	if (ok) put_atom(q, q->curr_frame+term5->slot, strdup(session_get_stash((session*)sp->sptr, "Sec-WebSocket-Protocol")), 0);
	return 1;
}

static int bif_ws_request3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	const char *path = term2->val_s;
	int status;
	ws_request((session*)sp->sptr, path, NULL, &status);
	put_int(q, q->curr_frame+term3->slot, status);
	return 1;
}

static int bif_ws_upgrade2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;
	const char *prot = term2->val_s;
	return ws_upgrade((session*)sp->sptr, prot);
}
#endif

static int bif_ws_parse3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;

	if (session_on_disconnect((session*)sp->sptr))
	{
		q->is_yielded = 1;
		return 0;
	}

	int fin;
	unsigned opcode = 0;
	size_t dstlen = 0;
	char *dstbuf = NULL;

	if (!session_ws_parse((session*)sp->sptr, &fin, &opcode, &dstbuf, &dstlen))
	{
		q->is_yielded = 1;
		return 0;
	}

	node *n = NULL;

	if (opcode == WS_OP_BINARY)
		n = make_blob(dstbuf, dstlen);
	else
		n = make_atom(dstbuf, 1);

	const char *op = "";

	if (opcode == WS_OP_CLOSE)
		op = "close";
	else if (opcode == WS_OP_PONG)
		op = "pong";
	else if (opcode == WS_OP_PING)
		op = "ping";
	else if (fin)
		op = "data";
	else
		op = "more";

	put_const_atom(q, q->curr_frame+term2->slot, op, 0);
	put_env(q, q->curr_frame+term3->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_ws_is_ws(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	int ok = session_is_websocket((session*)sp->sptr);
	node *n = make_const_atom(ok?"true":"false", 0);
	n->refcnt++;
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_h2_request3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	const char *host = session_get_stash((session*)sp->sptr, "SERVER_NAME");
	const char *user = session_get_stash((session*)sp->sptr, "USER");
	const char *pass = session_get_stash((session*)sp->sptr, "PASS");
	size_t mlen = strlen(term2->val_s)+strlen(host)+1024;
	char *dstbuf = (char*)malloc(mlen);
	char *dst = dstbuf;
	dst += sprintf(dst, "GET %s HTTP/1.1\r\n", term2->val_s);
	dst += sprintf(dst, "Host: %s\r\n", host);

	if (user[0])
	{
		dst += snprintf(dst, 256, "Authorization: Basic ");
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s", user, pass);
		dst += b64_encode(tmpbuf, strlen(tmpbuf), &dst, 0, 0);
		dst += sprintf(dst, "\r\n");
	}

	dst += sprintf(dst, "HTTP2-Settings: %s\r\n", "???");
	sprintf(dst, "Upgrade: h2c\r\nConnection: Upgrade,HTTP2-Settings\r\nUser-Agent: Trealla\r\n\r\n");
	session_writemsg((session*)sp->sptr, dstbuf);
	//printf("%s", dstbuf);

	char *bufptr = NULL;
	int status = 0;
	int len;

	while ((len = session_readmsg((session*)sp->sptr, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag((session*)sp->sptr, CMD))
		{
			session_set_udata_flag((session*)sp->sptr, CMD);
			char ver[20];
			ver[0] = '\0';
			sscanf(bufptr, "HTTP/%19s %d", ver, &status);
			free(bufptr);
			ver[sizeof(ver)-1] = '\0';
			put_int(q, q->curr_frame+term3->slot, status);
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag((session*)sp->sptr, CMD);
			if (status != 101) return 1;
			session_set_udata_flag((session*)sp->sptr, HTTP2);
			return 1;
		}

		parse_header((session*)sp->sptr, bufptr, len);
	}

	return 0;
}

static int bif_h2_upgrade2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;
	//const char *sec_set = session_get_stash((session*)sp->sptr, "HTTP2-Settings");
	char dstbuf[1024*4];
	char *dst = dstbuf;
	dst += sprintf(dst, "HTTP/1.1 101 Switching Protocols\r\n");
	//dst += snprintf(dst, 1024, "%s\r\n", sec_set);
	strcat(dst, "Upgrade: h2c\r\nConnection: Upgrade\r\nServer: Trealla\r\n\r\n");
	session_writemsg((session*)sp->sptr, dstbuf);
	session_set_udata_flag((session*)sp->sptr, HTTP2);
	//printf("%s", dstbuf);
	return 1;
}

static int bif_h2_msg3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_int(term2);
	node *term3 = get_atom(term3);
	//stream *sp = term1->val_str;
	//const uint64_t len = LEN(term3);
	return 0;
}

static int bif_h2_parse3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;

	if (session_on_disconnect((session*)sp->sptr))
		return 0;

	return 0;
}

static int bif_h2_is_h2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_term(term2);
	stream *sp = term1->val_str;
	int ok = session_get_udata_flag((session*)sp->sptr, HTTP2);
	node *n = make_const_atom(ok?"true":"false", 0);
	n->refcnt++;
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

#if 0
static char *stomp_escape(const char *path, char *path2)
{
	const char *src = path;
	char *dst = path2;

	while (*src)
	{
		if (*src == ':')
		{
			*dst++ = '\\';
			*dst++ = 'c';
		}
		else if (*src == '\\')
		{
			*dst++ = '\\';
			*dst++ = '\\';
		}
		else
			*dst++ = *src;

		src++;
	}

	*dst = '\0';
	return path2;
}
#endif

static char *stomp_deescape(const char *path, char *path2)
{
	const char *src = path;
	char *dst = path2;

	while (*src)
	{
		if (*src == '\\')
		{
			src++;

			if (*src == 'r')
				*dst++ = '\r';
			else if (*src == 'n')
				*dst++ = '\n';
			else if (*src == 'c')
				*dst++ = ':';
			else if (*src == '\\')
				*dst++ = '\\';
		}
		else
			*dst++ = *src;

		src++;
	}

	*dst = '\0';
	return path2;
}

static int bif_stomp_parse3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	char *bufptr = NULL;

	if (session_on_disconnect((session*)sp->sptr))
	{
		q->is_yielded = 1;
		return 0;
	}

	int len;

	while ((len = session_readmsg((session*)sp->sptr, &bufptr)) > 0)
	{
		//printf("> %s", bufptr);

		if (!session_get_udata_flag((session*)sp->sptr, CMD))
		{
			session_clr_stash((session*)sp->sptr);
			session_set_udata_flag((session*)sp->sptr, CMD);

			char cmd[256];
			cmd[0] = '\0';
			sscanf(bufptr, "%255s", cmd);
			free(bufptr);
			cmd[sizeof(cmd)-1] = '\0';
			put_atom(q, q->curr_frame+term3->slot, strdup(cmd), 1);
			session_set_stash((session*)sp->sptr, "REQUEST_METHOD", cmd);
			char tmpbuf[256];

			if (session_is_ipv6((session*)sp->sptr))
			{
				snprintf(tmpbuf, sizeof(tmpbuf), "[%s]", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "REMOTE_ADDR", tmpbuf);
				session_set_stash((session*)sp->sptr, "REMOTE_HOST", tmpbuf);
				snprintf(tmpbuf, sizeof(tmpbuf), "[%s]", session_get_local_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_ADDR", tmpbuf);
				session_set_stash((session*)sp->sptr, "LOCAL_HOST", tmpbuf);
			}
			else
			{
				session_set_stash((session*)sp->sptr, "REMOTE_ADDR", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "REMOTE_HOST", session_get_remote_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_ADDR", session_get_local_addr((session*)sp->sptr, 0));
				session_set_stash((session*)sp->sptr, "LOCAL_HOST", session_get_local_addr((session*)sp->sptr, 0));
			}

			session_set_stash_int((session*)sp->sptr, "REMOTE_PORT", session_get_remote_port((session*)sp->sptr));
			session_set_stash_int((session*)sp->sptr, "LOCAL_PORT", session_get_local_port((session*)sp->sptr));
			continue;
		}

		if ((bufptr[0] == '\r') || (bufptr[0] == '\n'))
		{
			free(bufptr);
			session_clr_udata_flag((session*)sp->sptr, CMD);
			char tmpbuf[1024], tmpbuf2[1024], tmpbuf3[256];
			tmpbuf2[0] = tmpbuf3[0] = '\0';
			const char *src = session_get_stash((session*)sp->sptr, "Host");
			sscanf(src, "%1023[^: \r\n]:%255s", tmpbuf2, tmpbuf3);
			tmpbuf2[sizeof(tmpbuf2)-1] = tmpbuf3[sizeof(tmpbuf3)-1] = '\0';

			if (tmpbuf2[0])
				session_set_stash((session*)sp->sptr, "SERVER_NAME", stomp_deescape(tmpbuf2, tmpbuf));
			else
				session_set_stash((session*)sp->sptr, "SERVER_NAME", session_get_stash((session*)sp->sptr, "LOCAL_ADDRESS"));

			if (tmpbuf3[0])
				session_set_stash((session*)sp->sptr, "SERVER_PORT", stomp_deescape(tmpbuf3, tmpbuf));
			else
				session_set_stash((session*)sp->sptr, "SERVER_PORT", session_get_stash((session*)sp->sptr, "LOCAL_PORT"));

			if (strlen(session_get_stash((session*)sp->sptr, "Authorization")))
			{
				const char *hdr = session_get_stash((session*)sp->sptr, "Authorization");
				char auth[256], tmpbuf[256];
				auth[0] = tmpbuf[0] = '\0';
				sscanf(hdr, "%255s %255s", auth, tmpbuf);
				auth[sizeof(auth)-1] = tmpbuf[sizeof(tmpbuf)-1] = '\0';

				if (!strcmp(auth, "Basic"))
				{
					size_t nbytes = 64;
					char *dstbuf = (char*)malloc(nbytes);
					b64_decode(tmpbuf, strlen(tmpbuf), (char**)&dstbuf);
					char userid[256], passwd[256];
					userid[0] = passwd[0] = '\0';
					sscanf(dstbuf, "%255[^:]:%255s", userid, passwd);
					userid[sizeof(userid)-1] = passwd[sizeof(passwd)-1] = '\0';
					session_set_stash((session*)sp->sptr, "USER", userid);
					session_set_stash((session*)sp->sptr, "PASS", passwd);
					free(dstbuf);
				}
			}

			if (strlen(session_get_stash((session*)sp->sptr, "WWW-Authenticate")))
			{
				const char *hdr = session_get_stash((session*)sp->sptr, "WWW-Authenticate");
				char auth[256], tmpbuf[256];
				auth[0] = tmpbuf[0] = '\0';
				sscanf(hdr, "%255s %255s", auth, tmpbuf);
				auth[sizeof(auth)-1] = tmpbuf[sizeof(tmpbuf)-1] = '\0';

				if (!strcmp(auth, "Basic") && !strncmp(tmpbuf, "Realm", 5))
				{
					const char *src = tmpbuf;
					char *dst = auth;

					while (isalpha(*src) || (*src == '='))
						src++;

					if (*src == '"')
						src++;

					while (*src && (*src != '"') && ((dst-auth)<(sizeof(auth)-1)))
						*dst++ = *src++;

					*dst = '\0';
					session_set_stash((session*)sp->sptr, "REALM", auth);
				}
			}

			if (strlen(session_get_stash((session*)sp->sptr, "Content-Length"))) session_set_stash((session*)sp->sptr, "CONTENT_LENGTH", session_get_stash((session*)sp->sptr, "Content-Length"));
			if (strlen(session_get_stash((session*)sp->sptr, "Content-Type"))) session_set_stash((session*)sp->sptr, "CONTENT_TYPE", session_get_stash((session*)sp->sptr, "Content-Type"));

			if (strlen(session_get_stash((session*)sp->sptr, "Version")))
			{
				put_float(q, q->curr_frame+term2->slot, atof(session_get_stash((session*)sp->sptr, "Version")));
				session_set_stash((session*)sp->sptr, "STOMP", session_get_stash((session*)sp->sptr, "Version"));
			}
			else if (strlen(session_get_stash((session*)sp->sptr, "Accept-Version")))
			{
				const char *verstr = session_get_stash((session*)sp->sptr, "Accept-Version");
				double ver;
				if (strstr(verstr, "1.2")) put_float(q, q->curr_frame+term2->slot, ver=1.2);
				else if (strstr(verstr, "1.1")) put_float(q, q->curr_frame+term2->slot, ver=1.1);
				else put_float(q, q->curr_frame+term2->slot, ver=1.0);
				char tmpbuf[256];
				snprintf(tmpbuf, sizeof(tmpbuf), "%.1f", ver);
				session_set_stash((session*)sp->sptr, "STOMP", tmpbuf);
			}

			return 1;
		}

		parse_header((session*)sp->sptr, bufptr, len);
	}

	q->is_yielded = 1;
	return 0;
}

static int bif_stomp_msg4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_socket(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	node *term4 = get_atom(term4);
	stream *sp = term1->val_str;

	session_set_cork((session*)sp->sptr, 1);
	session_writemsg((session*)sp->sptr, term2->val_s);
	session_writemsg((session*)sp->sptr, "\n");
	size_t len = LEN(term3);

	if (len)
	{
		const char *src = term3->val_s;
		char *tmpbuf = (char*)malloc((len*2)+1);
		char *dst = tmpbuf;

		while (*src)
		{
			while (*src != ':')
				*dst++ = *src++;

			*dst++ = *src++;		// the ':'

			while ((*src != 'r') && (*src != '\n'))
			{
				if (*src == ':')
				{
					*dst++ = '\\';
					*dst++ = 'c';
				}
				else if (*src == '\\')
				{
					*dst++ = '\\';
					*dst++ = '\\';
				}
				else
					*dst++ = *src;

				src++;
			}

			while ((*src == 'r') || (*src == '\n'))
				*dst++ = *src++;
		}

		*dst = '\0';
		session_writemsg((session*)sp->sptr, tmpbuf);
		free(tmpbuf);
	}

	len = LEN(term4);
	char tmpbuf[256];
	snprintf(tmpbuf, sizeof(tmpbuf), "content-length:%u\n\n", (unsigned)len);
	session_writemsg((session*)sp->sptr, tmpbuf);
	session_write((session*)sp->sptr, term4->val_s, len+1); // Include the NULL
	session_set_cork((session*)sp->sptr, 0);
	return 1;
}

void bifs_load_http(void)
{
	DEFINE_BIF("http:parse", 4, bif_http_parse4);
	DEFINE_BIF("http:www_form", 1, bif_http_www_form1);
	DEFINE_BIF("http:query", 3, bif_http_query3);
	DEFINE_BIF("http:form", 3, bif_http_form3);
	DEFINE_BIF("http:cookie", 3, bif_http_cookie3);
	DEFINE_BIF("http:get10", 3, bif_http_get103);
	DEFINE_BIF("http:head10", 3, bif_http_head103);
	DEFINE_BIF("http:delete10", 5, bif_http_delete105);
	DEFINE_BIF("http:put10", 5, bif_http_put105);

	DEFINE_BIF("http:get11", 3, bif_http_get113);
	DEFINE_BIF("http:head11", 3, bif_http_head113);
	DEFINE_BIF("http:delete11", 3, bif_http_delete113);
	DEFINE_BIF("http:put11", 3, bif_http_put113);
	DEFINE_BIF("http:get_chunk", 3, bif_http_get_chunk3);
	DEFINE_BIF("http:put_chunk", 3, bif_http_put_chunk3);
	DEFINE_BIF("http:put_chunk", 2, bif_http_put_chunk2);
	DEFINE_BIF("http:put_file", 1+2, bif_http_put_file2);

	DEFINE_BIF("h2:is_h2", 2, bif_h2_is_h2);
	DEFINE_BIF("h2:request", 3, bif_h2_request3);
	DEFINE_BIF("h2:upgrade", 2, bif_h2_upgrade2);
	DEFINE_BIF("h2:parse", 3, bif_h2_parse3);
	DEFINE_BIF("h2:msg", 3, bif_h2_msg3);
};

void bifs_load_ws(void)
{
#if USE_SSL
	DEFINE_BIF("ws:request", 5, bif_ws_request5);
	DEFINE_BIF("ws:request", 3, bif_ws_request3);
	DEFINE_BIF("ws:upgrade", 2, bif_ws_upgrade2);
#endif

	DEFINE_BIF("ws:parse", 3, bif_ws_parse3);
	DEFINE_BIF("ws:msg", 3, bif_ws_msg3);
	DEFINE_BIF("ws:is_ws", 2, bif_ws_is_ws);
};

void bifs_load_stomp(void)
{
	DEFINE_BIF("stomp:parse", 3, bif_stomp_parse3);
	DEFINE_BIF("stomp:msg", 4, bif_stomp_msg4);
};

