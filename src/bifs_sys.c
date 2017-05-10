#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
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
#define fseeko _fseeki64
#else
#include <sys/time.h>
#include <unistd.h>
#define msleep(ms) { struct timespec tv; tv.tv_sec = (ms)/1000; tv.tv_nsec = ((ms)%1000) * 1000 * 1000; nanosleep(&tv, &tv); }
#endif

#ifndef USE_SSL
#define USE_SSL 0
#endif

#if USE_SSL
#include "openssl/sha.h"
#endif

#include "trealla.h"

#include "base64.h"
#include "bifs.h"
#include "internal.h"
#include "jela.h"
#include "jsonq.h"
#include "uuid.h"
#include "xmlq.h"

#define END_OF_FILE "end_of_file"

#ifdef _WIN32
static char *strndup(const char *s, size_t n)
{
	size_t len = strlen(s);

	if (n < len)
		len = n;

	char *dstbuf = (char *)malloc(len + 1);
	dstbuf[len] = '\0';
	return (char *)memcpy(dstbuf, s, len);
}
#endif

static int bif_sys_exists_file1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) != 0)
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return 0;

	return 1;
}

static int bif_sys_exists_dir1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) != 0)
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return 0;

	return 1;
}

static int bif_sys_make_dir1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) == 0)
		return 0;

	return !mkdir(filename, 0777);
}

static int bif_sys_make_dir2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int(term2);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) == 0)
		return 0;

	return !mkdir(filename, term2->val_u);
}

static int bif_sys_exists_file3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	node *term3 = get_var(term3);
	const char *filename = VAL_S(term1);
	struct stat st = {0};

	if (stat(filename, &st) != 0)
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return 0;

	put_int(q, q->curr_frame + term2->slot, (nbr_t)st.st_size);
	put_int(q, q->curr_frame + term3->slot, (nbr_t)st.st_mtime);
	return 1;
}

static int bif_sys_write_file2(tpl_query *q)
{
	node *args = get_args(q);
	node *var; // FLAG_HIDDEN
	FILE *fp;

	if (!q->retry) {
		var = get_var(var);
	}
	else {
		var = get_stream(var);
	}

	node *term1 = get_stream(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str, *sp2;

	if (!q->retry) {
		fp = fopen(VAL_S(term2), "rb");

		if (!fp) {
			QABORT(ABORT_NOFILEACCESS);
			return 0;
		}

		sp2 = calloc(1, sizeof(stream));
		sp2->fptr = fp;
		node *n = make_stream(sp2);
		put_env(q, q->curr_frame + var->slot, n, -1);
		n->refcnt--;
		allocate_frame(q);
	}
	else {
		sp2 = var->val_str;
		fp = sp2->fptr;
	}

	size_t buflen = 1024 * 64;
	char *tmpbuf = malloc(buflen);
	size_t rlen;

	while ((rlen = fread(tmpbuf, 1, buflen, fp)) > 0) {
		if (is_socket(term1)) {
			const char *src = tmpbuf;

			while (rlen > 0) {
				int wlen = session_rawwrite((session *)sp->sptr, src, rlen);

				if (wlen < 0) {
					free(tmpbuf);
					fclose(fp);
					sp2->fptr = NULL;
					QABORT(ABORT_STREAMCLOSED);
					return 0;
				}

				if (!wlen) {
					free(tmpbuf);
					fseeko(fp, -rlen, SEEK_CUR);
					process_yield_unlocked(q);
					return 0;
				}

				rlen -= wlen;
				src += wlen;
			}
		}
		else {
			if (fwrite(tmpbuf, 1, rlen, sp->fptr) != rlen) {
				free(tmpbuf);
				fclose(fp);
				sp2->fptr = NULL;
				QABORT(ABORT_STREAMCLOSED);
				return 0;
			}
		}
	}

	free(tmpbuf);
	fclose(fp);
	sp2->fptr = NULL;
	return 1;
}

static int bif_sys_remove_file1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	remove(VAL_S(term1));
	return 1;
}

static int bif_sys_save_file2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	FILE *fp = fopen(VAL_S(term1), "wb");

	if (!fp) {
		QABORT(ABORT_NOFILEACCESS);
		return 0;
	}

	size_t len = fwrite(term2->val_ptr, 1, LEN(term2), fp);
	fclose(fp);
	return len == LEN(term2);
}

static int bif_sys_append_file2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	FILE *fp = fopen(VAL_S(term1), "ab");

	if (!fp) {
		QABORT(ABORT_NOFILEACCESS);
		return 0;
	}

	size_t len = fwrite(term2->val_ptr, 1, LEN(term2), fp);
	fclose(fp);
	return len == LEN(term2);
}

static int bif_sys_load_file2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	struct stat st = {0};

	if (stat(VAL_S(term1), &st) != 0) {
		QABORT(ABORT_NOTEXISTFILE);
		return 0;
	}

	FILE *fp = fopen(VAL_S(term1), "rb");

	if (!fp) {
		QABORT(ABORT_NOTEXISTFILE);
		return 0;
	}

	char *dstbuf = (char *)malloc(st.st_size + 1);
	size_t len = fread(dstbuf, 1, st.st_size, fp);
	dstbuf[len] = '\0';
	fclose(fp);
	node *n = make_blob(dstbuf, len);
	put_env(q, q->curr_frame + term2->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_sys_concat(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_next_arg(q, &args);
	node *save_args = args;
	node *term = term1;
	int any_blobs = 0;

	while (term) {
		any_blobs += is_blob(term);
		term = get_next_arg(q, &args);
	}

	args = save_args;
	term = term1;
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	node *var = NULL;

	while (term) {
		if (is_var(term))
			var = term;
		else if (is_atomic(term))
			dst += term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term, 0);
		else {
			free(tmpbuf);
			return 0;
		}

		term = get_next_arg(q, &args);
	}

	*dst = '\0';

	if (!var) {
		free(tmpbuf);
		return 0;
	}

	node *n;

	if (any_blobs)
		n = make_blob(tmpbuf, dst - tmpbuf);
	else
		n = make_atom(tmpbuf, 1);

	put_env(q, q->curr_frame + var->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_sys_getline2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_file(term1);
	node *term2 = get_var(term2);
	stream *sp = term1->val_str;
	char *line;

	if (!(line = trealla_readline(q->lex, sp->fptr, 0)))
		line = strdup(END_OF_FILE);
	else {
		size_t len = strlen(line);

		if (len) {
			if (line[len - 1] == '\n')
				len--;
			if (line[len - 1] == '\r')
				len--;

			line[len] = '\0';
		}
	}

	put_atom(q, q->curr_frame + term2->slot, line, 1);
	return 1;
}

static int bif_sys_getline1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	char *line;

	if (!(line = trealla_readline(q->lex, q->curr_stdin, 0)))
		line = strdup(END_OF_FILE);
	else {
		size_t len = strlen(line);

		if (len) {
			if (line[len - 1] == '\n')
				len--;
			if (line[len - 1] == '\r')
				len--;

			line[len] = '\0';
		}
	}

	put_atom(q, q->curr_frame + term1->slot, line, 1);
	return 1;
}

static int bif_sys_bwrite2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	node *term2 = get_atom(term2);
	stream *sp = term1->val_str;
	size_t len = LEN(term2);

	if (is_socket(term1)) {
		if (!session_write((session *)sp->sptr, VAL_S(term2), len)) {
			QABORT(ABORT_STREAMCLOSED);
			return 0;
		}
	}
	else {
		if (fwrite(VAL_S(term2), 1, len, sp->fptr) == 0) {
			QABORT(ABORT_STREAMCLOSED);
			return 0;
		}
	}

	return 1;
}

static int bif_sys_bread3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_stream(term1);
	node *term2 = get_int_or_var(term2);
	node *term3 = get_var(term3);
	stream *sp = term1->val_str;
	size_t len = is_var(term2) ? 0 : term2->val_i;
	char *bufptr;
	size_t rlen;

	if (is_var(term2)) // just return what's available
	{
		len = (1024 * 8);
		bufptr = (char *)malloc(len + 1);

		if (is_socket(term1)) {
			if (!(rlen = session_read((session *)sp->sptr, bufptr, len))) {
				if (session_on_disconnect((session *)sp->sptr)) {
					free(bufptr);
					return 0;
				}
			}
		}
		else {
			if (!(rlen = fread(bufptr, 1, len, sp->fptr))) {
				free(bufptr);
				return 0;
			}
		}

		bufptr[len = rlen] = '\0';
		put_int(q, q->curr_frame + term2->slot, len);
	}
	else // return specific length
	{
		bufptr = (char *)malloc(len + 1);
		char *dst = bufptr;
		size_t nbytes = len;

		if (is_socket(term1)) {
			size_t chunk_size = 1024 * 8;

			// Allow for blocking & non-blocking sockets

			while (nbytes > 0) {
				size_t blk_size = nbytes < chunk_size ? nbytes : chunk_size;

				if (!(rlen = session_read((session *)sp->sptr, dst, blk_size))) {
					if (session_on_disconnect((session *)sp->sptr)) {
						free(bufptr);
						return 0;
					}

					if (!rlen) {
						q->is_yielded = 1;
						free(bufptr);
						return 0;
					}
				}

				dst += rlen;
				nbytes -= rlen;
			}
		}
		else {
			if (!(rlen = fread(bufptr, 1, len, sp->fptr))) {
				free(bufptr);
				return 0;
			}

			len = rlen;
		}

		bufptr[len] = '\0';
	}

	node *n = make_blob(bufptr, len);
	put_env(q, q->curr_frame + term3->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_sys_exit1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	q->halt = 1;
	q->halt_s = strdup(VAL_S(term1));
	return 1;
}

static int bif_sys_now0(tpl_query *q)
{
	q->nv.val_i = time(NULL);
	q->nv.flags = TYPE_INTEGER;
	return 0;
}

static int bif_sys_now1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	time_t secs = time(NULL);
	put_int(q, q->curr_frame + term1->slot, secs);
	return 1;
}

static int bif_sys_timestamp1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	put_int(q, q->curr_frame + term1->slot, gettimeofday_usec());
	return 1;
}

static int bif_sys_hsleep1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	msleep((int)term1->val_i * 1000);
	return 1;
}

static int bif_sys_hdelay1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	msleep((int)term1->val_i);
	return 1;
}

static int bif_sys_sleep1(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);
	else
		return 1;

	node *args = get_args(q);
	node *term1 = get_int(term1);
	int msecs = (int)term1->val_i * 1000;

	if (!q->is_forked) {
		while (!g_abort && (msecs > 0)) {
			msleep(10);
			msecs -= 10;
		}

		return 1;
	}

	q->tmo_msecs = msecs;
	PIDLOCK(q->pl);

	if (q->tmo_msecs > 0) {
		q->tmo_when_msecs = gettimeofday_usec() / 1000;
		q->tmo_when_msecs += q->tmo_msecs;
		q->is_idle = 1;
		sl_set(&q->pl->idle, (const char *)q, NULL);
	}

	return process_yield_locked(q);
}

static int bif_sys_delay1(tpl_query *q)
{
	if (!q->retry)
		allocate_frame(q);
	else
		return 1;

	node *args = get_args(q);
	node *term1 = get_int(term1);
	int msecs = (int)term1->val_i;

	if (!q->is_forked) {
		while (!g_abort && (msecs > 0)) {
			msleep(10);
			msecs -= 10;
		}

		return 1;
	}

	q->tmo_msecs = msecs;
	PIDLOCK(q->pl);

	if (q->tmo_msecs > 0) {
		q->tmo_when_msecs = gettimeofday_usec() / 1000;
		q->tmo_when_msecs += q->tmo_msecs;
		q->is_idle = 1;
		sl_set(&q->pl->idle, (const char *)q, NULL);
	}

	return process_yield_locked(q);
}

static int bif_sys_jsonq3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	const size_t len = LEN(term1);
	char *tmpbuf = (char *)malloc(len + 1);

	if (!jsonq(VAL_S(term1), VAL_S(term2), tmpbuf, len)) {
		free(tmpbuf);
		return 0;
	}

	put_atom(q, q->curr_frame + term3->slot, strdup(tmpbuf), 1);
	free(tmpbuf);
	return 1;
}

static int bif_sys_jsonq4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	node *term4 = get_atom(term3);
	const size_t len = LEN(term1);
	char *tmpbuf = (char *)malloc(len + 1);

	if (!jsonq(VAL_S(term1), VAL_S(term2), tmpbuf, len))
		put_atom(q, q->curr_frame + term3->slot, strdup(VAL_S(term4)), 1);
	else
		put_atom(q, q->curr_frame + term3->slot, strdup(tmpbuf), 1);


	free(tmpbuf);
	return 1;
}

static int bif_sys_xmlq4(tpl_query *q)
{
	node *args = get_args(q);
	node *var = get_var(var); // FLAG_HIDDEN
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_int(term3);
	node *term4 = get_var(term4);
	const size_t len = LEN(term1);
	char *tmpbuf = (char *)malloc(len + 1);

	if (!xmlq(VAL_S(term1), term3->val_i, VAL_S(term2), tmpbuf, len)) {
		free(tmpbuf);
		return 0;
	}

	put_atom(q, q->curr_frame + term4->slot, strdup(tmpbuf), 1);
	free(tmpbuf);
	return 1;
}

static int bif_sys_xmlq3(tpl_query *q)
{
	node *args = get_args(q);
	node *orig_var = term_next(args);
	node *var = get_term(var); // FLAG_HIDDEN
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	const size_t len = LEN(term1);
	char *tmpbuf = (char *)malloc(len + 1);

	if (!q->retry) {
		if (!xmlq(VAL_S(term1), 0, VAL_S(term2), tmpbuf, len)) {
			free(tmpbuf);
			return 0;
		}

		put_int(q, q->curr_frame + var->slot, 0);
		allocate_frame(q);
	}
	else {
		int i = var->val_i + 1;

		if (!xmlq(VAL_S(term1), i, VAL_S(term2), tmpbuf, len)) {
			free(tmpbuf);
			return 0;
		}

		reset_arg(q, orig_var, q->curr_frame);
		put_int(q, q->curr_frame + orig_var->slot, i);
	}

	try_me(q);
	put_atom(q, q->curr_frame + term3->slot, strdup(tmpbuf), 1);
	free(tmpbuf);
	return 1;
}

static int bif_sys_upper2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	const size_t len = LEN(term1) + 1;
	char *dstbuf = (char *)malloc(len + 1);
	char *dst = dstbuf;
	const char *src = VAL_S(term1);

	while (*src) {
		char ch = *src++;
		*dst++ = toupper(ch);
	}

	*dst = '\0';
	put_atom(q, q->curr_frame + term2->slot, dstbuf, 1);
	return 1;
}

static int bif_sys_lower2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	const size_t len = LEN(term1);
	char *dstbuf = (char *)malloc(len + 1);
	char *dst = dstbuf;
	const char *src = VAL_S(term1);

	while (*src) {
		char ch = *src++;
		*dst++ = tolower(ch);
	}

	*dst = '\0';
	put_atom(q, q->curr_frame + term2->slot, dstbuf, 1);
	return 1;
}

static const char *month_names[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

static char *format_rfcdate(char *dst, time_t when)
{
	static const char *day_names[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	struct tm tm = *gmtime(&when);
	sprintf(dst, "%s, %02d %s %d %02d:%02d:%02d GMT", day_names[tm.tm_wday], (int)tm.tm_mday, month_names[tm.tm_mon],
	        tm.tm_year + 1900, tm.tm_hour, tm.tm_min, tm.tm_sec);
	return dst;
}

static int bif_sys_format_rfcdate2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_var(term2);
	char tmpbuf[80];
	format_rfcdate(tmpbuf, term1->val_i);
	put_atom(q, q->curr_frame + term2->slot, strdup(tmpbuf), 1);
	return 1;
}

static int bif_sys_parse_rfcdate2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	char tmp_month[20];
	tmp_month[0] = '\0';
	int year = 0, month = 0, mday = 0, hour = 0, min = 0, sec = 0;
	sscanf(VAL_S(term1), "%*[^,], %d %19s %d %d:%d:%d", &mday, tmp_month, &year, &hour, &min, &sec);
	tmp_month[sizeof(tmp_month) - 1] = '\0';

	for (int i = 0; i < 12; i++) {
		if (!strcmp(tmp_month, month_names[i])) {
			month = i;
			break;
		}
	}

	//printf("### %02d %s(%02d) %04d %02d:%02d:%02d\n", mday, tmp_month, month, year, hour, min, sec);
	struct tm tm = {0};
	tm.tm_year = year - 1900;
	tm.tm_mon = month;
	tm.tm_mday = mday;
	tm.tm_hour = hour;
	tm.tm_min = min;
	tm.tm_sec = sec;
	nbr_t v = (nbr_t)mktime(&tm);
	put_int(q, q->curr_frame + term2->slot, v);
	return 1;
}

static int bif_sys_atom_timestamp2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	nbr_t v = 0;
	int year = 0, month = 0, mday = 0, hour = 0, min = 0, sec = 0, ms = 0;
	sscanf(VAL_S(term1), "%d%*c%d%*c%d%*c%d%*c%d%*c%d.%d", &year, &month, &mday, &hour, &min, &sec, &ms);
	struct tm tm = {0};
	tm.tm_year = year - 1900;
	tm.tm_mon = month - 1;
	tm.tm_mday = mday;
	tm.tm_hour = hour;
	tm.tm_min = min;
	tm.tm_sec = sec;
	v = mktime(&tm);
	v *= 1000;
	v += ms;
	v *= 1000;
	put_int(q, q->curr_frame + term2->slot, v);
	return 1;
}

static int bif_sys_atom_number2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_nbr_or_var(term2);
	const char *src = VAL_S(term1);
	char ch = *src++;
	nbr_t v = 0;
	int numeric = 0;
	parse_number(ch, src, &v, &numeric);
	node *n;

	if (numeric > 1)
		n = make_quick_int(v);
	else
		n = make_float(strtod(VAL_S(term1), NULL));

	if (numeric == 5)
		n->flags |= FLAG_HEX;
	else if (numeric == 4)
		n->flags |= FLAG_OCTAL;
	else if (numeric == 3)
		n->flags |= FLAG_BINARY;

	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_sys_read_term_from_atom3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_var(term2);
	node *term3 = get_atom_or_list(term3);
	char *src = VAL_S(term1);
	int len = LEN(term1);

	if (!len)
		return 0;

	lexer l;
	lexer_init(&l, q->pl);
	lexer_parse(&l, l.r, src, NULL);
	xref_clause(&l, l.r);
	node *term = term_first(l.r);
	term = copy_term(q, term);
	term_heapcheck(l.r);
	lexer_done(&l);
	int ok = unify_term(q, term2, term, q->curr_frame);
	term_heapcheck(term);
	return ok;
}

static int bif_sys_splitl4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	node *term4 = get_atom_or_var(term4);
	const char *src = VAL_S(term1);
	char *dstbuf = (char *)malloc(LEN(term1) + 1);
	char *dst = dstbuf;

	while (isspace(*src))
		src++;

	for (;;) {
		while (*src && strncmp(src, VAL_S(term2), LEN(term2)))
			*dst++ = *src++;

		if (!strstr(src+2, VAL_S(term2)))
			break;

		strncpy(dst, src, LEN(term2));
		dst += LEN(term2);
		src += LEN(term2);
	}

	while (isspace(*src))
		src++;

	*dst = '\0';
	node *tmp = make_atom(strdup(dstbuf), 1);
	int ok = unify_term(q, term3, tmp, q->curr_frame);
	term_heapcheck(tmp);

	if (!ok) {
		free(dstbuf);
		return 0;
	}

	if (strncmp(src, VAL_S(term2), LEN(term2))) {
		free(dstbuf);
		return 1;
	}

	src += LEN(term2);
	dst = dstbuf;

	while (isspace(*src))
		src++;

	while (*src && (*src != '\r') && (*src != '\n'))
		*dst++ = *src++;

	*dst = '\0';
	tmp = make_atom(strdup(dstbuf), 1);
	free(dstbuf);
	ok = unify_term(q, term4, tmp, q->curr_frame);
	term_heapcheck(tmp);
	return ok;
}

static int bif_sys_split4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom_or_var(term3);
	node *term4 = get_atom_or_var(term4);
	const char *src = VAL_S(term1);
	char *dstbuf = (char *)malloc(LEN(term1) + 1);
	char *dst = dstbuf;

	while (isspace(*src))
		src++;

	while (*src && strncmp(src, VAL_S(term2), LEN(term2)))
		*dst++ = *src++;

	while (isspace(*src))
		src++;

	*dst = '\0';
	node *tmp = make_atom(strdup(dstbuf), 1);
	int ok = unify_term(q, term3, tmp, q->curr_frame);
	term_heapcheck(tmp);

	if (!ok) {
		free(dstbuf);
		return 0;
	}

	if (strncmp(src, VAL_S(term2), LEN(term2))) {
		free(dstbuf);
		return 1;
	}

	src += LEN(term2);
	dst = dstbuf;

	while (isspace(*src))
		src++;

	while (*src && (*src != '\r') && (*src != '\n'))
		*dst++ = *src++;

	*dst = '\0';
	tmp = make_atom(strdup(dstbuf), 1);
	free(dstbuf);
	ok = unify_term(q, term4, tmp, q->curr_frame);
	term_heapcheck(tmp);
	return ok;
}

static int bif_sys_split3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);
	const char *src = VAL_S(term1);
	node *l = make_list();
	node *save_l = l;
	char *dstbuf = (char *)malloc(LEN(term1) + 1);

	while (isspace(*src))
		src++;

	while (*src) {
		char *dst = dstbuf;

		while (*src && strncmp(src, VAL_S(term2), LEN(term2)))
			*dst++ = *src++;

		if (*src) {
			src += LEN(term2);

			while (isspace(*src))
				src++;
		}

		*dst = '\0';
		node *tmp = make_atom(strdup(dstbuf), 1);
		term_append(l, tmp);

		if (!*src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
	}

	free(dstbuf);
	term_append(l, make_const_atom("[]", 0));
	put_env(q, q->curr_frame + term3->slot, save_l, q->curr_frame);
	save_l->refcnt--;
	return 1;
}

static int bif_sys_replace4(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_atom(term3);
	node *term4 = get_var(term4);
	int srclen = LEN(term1);
	int dstlen = srclen * 2;
	const char *src = VAL_S(term1);
	const char *s1 = VAL_S(term2);
	const char *s2 = VAL_S(term3);
	int s1len = LEN(term2);
	int s2len = LEN(term3);
	char *dstbuf = (char *)malloc(dstlen + 1);
	char *dst = dstbuf;

	while (srclen > 0) {
		if (!strncmp(src, s1, s1len)) {
			if (dstlen < s2len) {
				size_t save_len = dst - dstbuf;
				dstlen = ((save_len)*2) + s2len;
				dstbuf = (char *)realloc(dstbuf, dstlen + 1);
				dst = dstbuf + save_len;
			}

			strcpy(dst, s2);
			dst += s2len;
			dstlen -= s2len;
			src += s1len;
			srclen -= s1len;
		}
		else {
			if (dstlen < 1) {
				size_t max_len = dst - dstbuf;
				dstlen = max_len *= 2;
				dstbuf = (char *)realloc(dstbuf, dstlen + 1);
				dst = dstbuf + max_len;
			}

			*dst++ = *src++;
			dstlen--;
			srclen--;
		}
	}

	*dst = '\0';
	put_atom(q, q->curr_frame + term4->slot, strdup(dstbuf), 1);
	free(dstbuf);
	return 1;
}

static int bif_sys_begins2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_list(term2);
	node *l = term2;

	while (is_list(l)) {
		node *head = term_firstarg(l);
		node *n = get_arg(q, head, q->latest_context);

		if (is_atom(n)) {
			if (!strncmp(VAL_S(term1), VAL_S(n), strlen(VAL_S(n))))
				return 1;
		}

		node *tail = term_next(head);
		l = get_arg(q, tail, q->latest_context);
	}

	return 0;
}

static int bif_sys_left3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int_or_var(term2);
	node *term3 = get_atom_or_var(term3);
	int len;

	if (is_integer(term2) && (term2->val_i > LEN(term1)))
		return 0;

	if (is_integer(term2))
		len = term2->val_i;
	else if (is_atom(term3))
		len = LEN(term3);
	else
		return 0;

	const char *src = VAL_S(term1);

	if (is_var(term2))
		put_int(q, q->curr_frame + term2->slot, len);

	if (is_atom(term3))
		return !strncmp(src, VAL_S(term3), len);

	put_atom(q, q->curr_frame + term3->slot, strndup(src, len), 1);
	return 1;
}

static int bif_sys_right3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int_or_var(term2);
	node *term3 = get_atom_or_var(term3);
	int len;

	if (is_integer(term2) && (term2->val_i > LEN(term1)))
		return 0;

	if (is_integer(term2))
		len = term2->val_i;
	else if (is_atom(term3))
		len = LEN(term3);
	else
		return 0;

	const char *src = VAL_S(term1) + LEN(term1) - len;

	if (is_var(term2))
		put_int(q, q->curr_frame + term2->slot, len);

	if (is_atom(term3))
		return !strcmp(src, VAL_S(term3));

	put_atom(q, q->curr_frame + term3->slot, strdup(src), 1);
	return 1;
}

static int bif_sys_rand1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	nbr_t v = rand();
	put_int(q, q->curr_frame + term1->slot, v);
	return 1;
}

static int bif_sys_munge2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);

	if (is_blob(term1)) {
		QABORT(ABORT_INVALIDARGNOTATOM);
		return 0;
	}

	const char *src = VAL_S(term1);
	size_t len = LEN(term1);
	char *tmpbuf = (char *)malloc(len + 1);
	char *dst = tmpbuf;

	while (len-- > 0) {
		*dst++ = isalnum(*src) ? *src : '_';
		src++;
	}

	*dst = '\0';
	put_atom(q, q->curr_frame + term2->slot, tmpbuf, 1);
	return 1;
}

static int bif_sys_getenv2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_var(term2);
	const char *value = getenv(VAL_S(term1));

	if (!value)
		return 0;

	node *n = make_atom(strdup(value), 1);
	int ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_sys_setenv2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom_or_int(term2);

	if (is_atom(term2)) {
		setenv(VAL_S(term1), VAL_S(term2), 1);
	}
	else {
		char tmpbuf[40];
		sprintf(tmpbuf, "%lld", (long long)term2->val_i);
		setenv(VAL_S(term1), tmpbuf, 1);
	}

	return 1;
}

static int bif_sys_unsetenv1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	unsetenv(VAL_S(term1));
	return 1;
}

static int bif_sys_system2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_int_or_var(term2);
	int ok = system(VAL_S(term1));
	node *n = make_int(ok);
	ok = unify_term(q, term2, n, q->curr_frame);
	term_heapcheck(n);
	return ok;
}

static int bif_sys_system1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	int ok = system(VAL_S(term1));
	return ok < 0 ? 0 : 1;
}

static int bif_sys_uuid1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);
	uuid u;
	uuid_gen(&u);
	char tmpbuf[256];
	uuid_to_string(&u, tmpbuf);
	put_atom(q, q->curr_frame + term1->slot, strdup(tmpbuf), 1);
	return 1;
}

#if USE_SSL
static int bif_sys_sha1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	unsigned char digest[SHA_DIGEST_LENGTH];
	SHA1((unsigned char *)VAL_S(term1), LEN(term1), digest);
	char tmpbuf[1024];
	char *dst = tmpbuf;

	for (int i = 0; i < SHA_DIGEST_LENGTH; i++)
		dst += snprintf(dst, sizeof(tmpbuf), "%02X", digest[i]);

	put_atom(q, q->curr_frame + term2->slot, strdup(tmpbuf), 1);
	return 1;
}

static int bif_sys_sha256(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	unsigned char digest[SHA256_DIGEST_LENGTH];
	SHA256((unsigned char *)VAL_S(term1), LEN(term1), digest);
	char tmpbuf[1024];
	char *dst = tmpbuf;

	for (int i = 0; i < SHA256_DIGEST_LENGTH; i++)
		dst += snprintf(dst, sizeof(tmpbuf), "%02X", digest[i]);

	put_atom(q, q->curr_frame + term2->slot, strdup(tmpbuf), 1);
	return 1;
}

static int bif_sys_sha512(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	unsigned char digest[SHA512_DIGEST_LENGTH];
	SHA512((unsigned char *)VAL_S(term1), LEN(term1), digest);
	char tmpbuf[1024];
	char *dst = tmpbuf;

	for (int i = 0; i < SHA512_DIGEST_LENGTH; i++)
		dst += snprintf(dst, sizeof(tmpbuf), "%02X", digest[i]);

	put_atom(q, q->curr_frame + term2->slot, strdup(tmpbuf), 1);
	return 1;
}
#endif

static uint32_t jenkins_one_at_a_time_hash(char *key)
{
	uint32_t hash = 0;

	while (*key != 0) {
		hash += *key++;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);
	return hash;
}

static int bif_sys_hash2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	put_int(q, q->curr_frame + term2->slot, jenkins_one_at_a_time_hash(VAL_S(term1)));
	return 1;
}

static char *url_encode(const char *src, int len, char *dstbuf)
{
	char *dst = dstbuf;

	while (len-- > 0) {
		if (*src == ' ') {
			*dst++ = '+';
			src++;
		}
		else if (!isalnum(*src))
			dst += sprintf(dst, "%%%02X", (unsigned)*src++);
		else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

char *url_decode(const char *src, char *dstbuf)
{
	char *dst = dstbuf;

	while (*src) {
		if (*src == '%') {
			src++;
			unsigned ch = 0;
			sscanf(src, "%02X", &ch);
			src += 2;
			*dst++ = (unsigned char)ch;
		}
		else if (*src == '+') {
			*dst++ = ' ';
			src++;
		}
		else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

static int bif_sys_url_encode2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	size_t len = LEN(term1);
	char *dstbuf = (char *)malloc((len * 3) + 1);
	url_encode(VAL_S(term1), len, dstbuf);
	put_atom(q, q->curr_frame + term2->slot, strdup(dstbuf), 1);
	free(dstbuf);
	return 1;
}

static int bif_sys_url_decode2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	size_t len = LEN(term1);
	char *dstbuf = (char *)malloc(len + 1);
	url_decode(VAL_S(term1), dstbuf);
	put_atom(q, q->curr_frame + term2->slot, strdup(dstbuf), 1);
	free(dstbuf);
	return 1;
}

static int bif_sys_b64_encode2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	size_t len = LEN(term1);
	char *dstbuf = (char *)malloc((len * 3) + 1);
	b64_encode(VAL_S(term1), len, &dstbuf, 0, 0);
	put_atom(q, q->curr_frame + term2->slot, strdup(dstbuf), 1);
	free(dstbuf);
	return 1;
}

static int bif_sys_b64_decode2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	size_t len = LEN(term1);
	char *dstbuf = (char *)malloc(len + 1);
	size_t nbytes = b64_decode(VAL_S(term1), len, &dstbuf);
	node *n = make_blob(dstbuf, nbytes);
	put_env(q, q->curr_frame + term2->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_sys_parse_csv(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	const char *src = VAL_S(term1);
	node *l = make_list();
	node *save_l = l;
	char *dstbuf = (char *)malloc(LEN(term1) + 1);
	char *dst = dstbuf;
	int quoted = 0, was_quoted = 0;

	while (isspace(*src))
		src++;

	while (*src) {
		char ch = *src++;

		if (!quoted && (ch == '"')) {
			was_quoted = quoted = 1;
			continue;
		}

		if (quoted && (ch == ',')) {
			*dst++ = ch;
			continue;
		}

		if (quoted && (ch == '"')) {
			quoted = 0;
			ch = *src++;
		}

		if ((ch != ',') && ch) {
			*dst++ = ch;

			if (*src)
				continue;
		}

		if (!ch)
			src--;

		*dst = '\0';

		// We allow for simple [-]integers and decimals,
		// the rest are treated as quoted atoms.

		node *tmp;
		const char *tmp_src = dstbuf;
		int dots = 0, bad = 0;

		if (*tmp_src == '-')
			tmp_src++;

		while (*tmp_src) {
			if (*tmp_src == '.')
				dots++;
			else if (!isdigit(*tmp_src))
				bad++;

			tmp_src++;
		}

		if (was_quoted || bad || (dots > 1))
			tmp = make_atom(strdup(dstbuf), 1);
		else {
			nbr_t v = 0;
			int numeric = 0;
			parse_number(ch = dstbuf[0], dstbuf + 1, &v, &numeric);

			if (numeric > 1)
				tmp = make_int(v);
			else
				tmp = make_float(strtod(dstbuf, NULL));
		}

		term_append(l, tmp);

		if (!*src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
		dst = dstbuf;
		*dst = '\0';
		quoted = was_quoted = 0;

		while (isspace(*src))
			src++;
	}

	free(dstbuf);
	term_append(l, make_const_atom("[]", 0));
	put_env(q, q->curr_frame + term2->slot, save_l, q->curr_frame);
	save_l->refcnt--;
	return 1;
}

static int bif_sys_parse_tab(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	const char *src = VAL_S(term1);
	node *l = make_list();
	node *save_l = l;
	char *dstbuf = (char *)malloc(LEN(term1) + 1);
	char *dst = dstbuf;
	int quoted = 0, was_quoted = 0;

	while (isspace(*src))
		src++;

	while (*src) {
		char ch = *src++;

		if (!quoted && (ch == '"')) {
			was_quoted = quoted = 1;
			continue;
		}

		if (quoted && (ch == '\t')) {
			*dst++ = ch;
			continue;
		}

		if (quoted && (ch == '"')) {
			quoted = 0;
			ch = *src++;
		}

		if ((ch != '\t') && ch) {
			*dst++ = ch;

			if (*src)
				continue;
		}

		if (!ch)
			src--;

		*dst = '\0';

		// We allow for simple [-]integers and decimals,
		// the rest are treated as quoted atoms.

		node *tmp;
		const char *tmp_src = dstbuf;
		int dots = 0, bad = 0;

		if (*tmp_src == '-')
			tmp_src++;

		while (*tmp_src) {
			if (*tmp_src == '.')
				dots++;
			else if (!isdigit(*tmp_src))
				bad++;

			tmp_src++;
		}

		if (was_quoted || bad || (dots > 1))
			tmp = make_atom(strdup(dstbuf), 1);
		else {
			nbr_t v = 0;
			int numeric = 0;
			parse_number(ch = dstbuf[0], dstbuf + 1, &v, &numeric);

			if (numeric > 1)
				tmp = make_int(v);
			else
				tmp = make_float(strtod(dstbuf, NULL));
		}

		term_append(l, tmp);

		if (!*src)
			break;

		tmp = make_list();
		term_append(l, tmp);
		l = tmp;
		dst = dstbuf;
		*dst = '\0';
		quoted = was_quoted = 0;

		while (isspace(*src))
			src++;
	}

	free(dstbuf);
	term_append(l, make_const_atom("[]", 0));
	put_env(q, q->curr_frame + term2->slot, save_l, q->curr_frame);
	save_l->refcnt--;
	return 1;
}

static int bif_sys_stream1(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_var(term0);
	stream *sp = calloc(1, sizeof(stream));
	node *n = make_stream(sp);
	put_env(q, q->curr_frame + term0->slot, n, -1);
	n->refcnt--;
	return 1;
}

static int bif_sys_erase1(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	stream *sp = term0->val_str;
	SYSLOCK(q->pl);

	if (sp->kvs) {
		sl_clear(sp->kvs, (void (*)(void *)) & term_heapcheck);
	}

	SYSUNLOCK(q->pl);
	return 1;
}

static int bif_sys_erase2(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	stream *sp = term0->val_str;
	SYSLOCK(q->pl);

	if (sp->kvs) {
		node *n = NULL;

		if (sl_del(sp->kvs, VAL_S(term1), (void **)&n))
			term_heapcheck(n);
	}

	SYSUNLOCK(q->pl);
	return 1;
}

static int bif_sys_lput4(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	node *term3 = get_nonvar(term3);
	stream *sp = term0->val_str;
	node *value = NULL;
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	SYSLOCK(q->pl);

	if (!sp->kvs)
		value = make_const_atom("[]", 0);
	else if (!sl_get(sp->kvs, VAL_S(term1), (void **)&value))
		value = make_const_atom("[]", 0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);

	if (!ok) {
		SYSUNLOCK(q->pl);
		return 0;
	}

	if (!sp->kvs) {
		sp->kvs = malloc(sizeof(skiplist));
		sl_init(sp->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(sp->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(sp->kvs, strdup(VAL_S(term1)), term3);
	term3->refcnt++;
	SYSUNLOCK(q->pl);
	return 1;
}

static int bif_sys_put4(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	node *term2 = get_var(term2);
	node *term3 = get_nonvar(term3);
	stream *sp = term0->val_str;
	node *value = NULL;
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	SYSLOCK(q->pl);

	if (!sp->kvs)
		value = make_quick_int(0);
	else if (!sl_get(sp->kvs, VAL_S(term1), (void **)&value))
		value = make_quick_int(0);

	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);

	if (!ok) {
		SYSUNLOCK(q->pl);
		return 0;
	}

	if (!sp->kvs) {
		sp->kvs = malloc(sizeof(skiplist));
		sl_init(sp->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(sp->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(sp->kvs, strdup(VAL_S(term1)), term3);
	term3->refcnt++;
	SYSUNLOCK(q->pl);
	return 1;
}

static int bif_sys_put3(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	node *term2 = get_nonvar(term2);
	stream *sp = term0->val_str;
	node *n = NULL;

	if (UTF8LEN(term1) > FUNCTOR_LEN) {
		QABORT(ABORT_ARGTOOBIG);
		return 0;
	}

	SYSLOCK(q->pl);

	if (!sp->kvs) {
		sp->kvs = malloc(sizeof(skiplist));
		sl_init(sp->kvs, 0, &strcmp, &free);
	}
	else if (sl_del(sp->kvs, VAL_S(term1), (void **)&n))
		term_heapcheck(n);

	sl_set(sp->kvs, strdup(VAL_S(term1)), term2);
	term2->refcnt++;
	SYSUNLOCK(q->pl);
	return 1;
}

static int bif_sys_get3(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	stream *sp = term0->val_str;
	node *value = NULL;
	SYSLOCK(q->pl);

	if (!sp->kvs)
		value = make_quick_int(0);
	else if (!sl_get(sp->kvs, VAL_S(term1), (void **)&value))
		value = make_quick_int(0);

	SYSUNLOCK(q->pl);
	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	return ok;
}

static int bif_sys_lget3(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_atom(term1);
	node *term2 = get_term(term2);
	stream *sp = term0->val_str;
	node *value = NULL;
	SYSLOCK(q->pl);

	if (!sp->kvs)
		value = make_const_atom("[]", 0);
	else if (!sl_get(sp->kvs, VAL_S(term1), (void **)&value))
		value = make_const_atom("[]", 0);

	SYSUNLOCK(q->pl);
	int ok = unify_term(q, term2, value, q->curr_frame);
	term_heapcheck(value);
	return ok;
}

static int bif_sys_get_keys3(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_term(term1);
	node *term2 = get_term(term2);
	stream *sp = term0->val_str;
	node *value, *l = make_list();
	node *save_l = l;
	int cnt = 0, ok;
	SYSLOCK(q->pl);

	if (sp->kvs) {
		sl_start(sp->kvs);
		const char *key;

		while ((key = sl_next(sp->kvs, (void **)&value)) != NULL) {
			if (!unify_term(q, value, term1, q->curr_frame)) {
				reallocate_frame(q);
				continue;
			}

			node *tmp = make_atom(strdup(key), 1);
			term_append(l, tmp);
			cnt++;

			if (!sp->kvs->iter)
				break;

			tmp = make_list();
			term_append(l, tmp);
			l = tmp;
		}
	}

	SYSUNLOCK(q->pl);

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

static int bif_sys_get2(tpl_query *q)
{
	node *args = get_args(q);
	node *term0 = get_stream(term0);
	node *term1 = get_term(term1);
	stream *sp = term0->val_str;
	node *value, *l = make_list();
	node *save_l = l;
	const char *key;
	int cnt = 0, ok;
	SYSLOCK(q->pl);

	if (sp->kvs) {
		sl_start(sp->kvs);

		while ((key = sl_next(sp->kvs, (void **)&value)) != NULL) {
			node *n = make_tuple();
			node *n2 = make_compound();
			term_append(n2, make_and());
			term_append(n2, make_atom(strdup(key), 1));
			term_append(n2, clone_term(q, value));
			term_append(n, n2);
			term_append(l, n);
			cnt++;

			if (!sp->kvs->iter)
				break;

			node *tmp;
			term_append(l, tmp = make_list());
			l = tmp;
		}
	}

	SYSUNLOCK(q->pl);

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

void bifs_load_sys(void)
{
	DEFINE_BIF("sys:concat", -1, bif_sys_concat);
	DEFINE_BIF("sys:system", 2, bif_sys_system2);
	DEFINE_BIF("sys:system", 1, bif_sys_system1);
	DEFINE_BIF("sys:load_file", 2, bif_sys_load_file2);
	DEFINE_BIF("sys:save_file", 2, bif_sys_save_file2);
	DEFINE_BIF("sys:append_file", 2, bif_sys_append_file2);
	DEFINE_BIF("sys:remove_file", 1, bif_sys_remove_file1);
	DEFINE_BIF("sys:make_dir", 1, bif_sys_make_dir1);
	DEFINE_BIF("sys:make_dir", 2, bif_sys_make_dir2);
	DEFINE_BIF("sys:exists_dir", 1, bif_sys_exists_dir1);
	DEFINE_BIF("sys:exists_file", 1, bif_sys_exists_file1);
	DEFINE_BIF("sys:exists_file", 3, bif_sys_exists_file3);
	DEFINE_BIF("sys:write_file", 1 + 2, bif_sys_write_file2);
	DEFINE_BIF("sys:getline", 1, bif_sys_getline1);
	DEFINE_BIF("sys:getline", 2, bif_sys_getline2);
	DEFINE_BIF("sys:now", 0, bif_sys_now0);
	DEFINE_BIF("sys:now", 1, bif_sys_now1);
	DEFINE_BIF("sys:exit", 1, bif_sys_exit1);
	DEFINE_BIF("sys:timestamp", 1, bif_sys_timestamp1);
	DEFINE_BIF("sys:jsonq", 3, bif_sys_jsonq3);
	DEFINE_BIF("sys:jsonq", 4, bif_sys_jsonq4);
	DEFINE_BIF("sys:xmlq", 1 + 4, bif_sys_xmlq4);
	DEFINE_BIF("sys:xmlq", 1 + 3, bif_sys_xmlq3);
	DEFINE_BIF("sys:sleep", 1, bif_sys_sleep1);
	DEFINE_BIF("sys:delay", 1, bif_sys_delay1);
	DEFINE_BIF("sys:hsleep", 1, bif_sys_hsleep1);
	DEFINE_BIF("sys:hdelay", 1, bif_sys_hdelay1);
	DEFINE_BIF("sys:rand", 1, bif_sys_rand1);
	DEFINE_BIF("sys:uuid", 1, bif_sys_uuid1);
	DEFINE_BIF("sys:getenv", 2, bif_sys_getenv2);
	DEFINE_BIF("sys:setenv", 2, bif_sys_setenv2);
	DEFINE_BIF("sys:unsetenv", 1, bif_sys_unsetenv1);
	DEFINE_BIF("sys:stream", 1, bif_sys_stream1);

	DEFINE_BIF("sys:lput", 4, bif_sys_lput4);
	DEFINE_BIF("sys:put", 4, bif_sys_put4);
	DEFINE_BIF("sys:put", 3, bif_sys_put3);
	DEFINE_BIF("sys:get", 3, bif_sys_get3);
	DEFINE_BIF("sys:get", 2, bif_sys_get2);
	DEFINE_BIF("sys:get_keys", 3, bif_sys_get_keys3);
	DEFINE_BIF("sys:lget", 3, bif_sys_lget3);
	DEFINE_BIF("sys:erase", 2, bif_sys_erase2);
	DEFINE_BIF("sys:erase", 1, bif_sys_erase1);

#if USE_SSL
	DEFINE_BIF("sys:sha1", 2, bif_sys_sha1);
	DEFINE_BIF("sys:sha256", 2, bif_sys_sha256);
	DEFINE_BIF("sys:sha512", 2, bif_sys_sha512);
#endif

	DEFINE_BIF("sys:hash", 2, bif_sys_hash2);
	DEFINE_BIF("sys:bread", 3, bif_sys_bread3);
	DEFINE_BIF("sys:bwrite", 2, bif_sys_bwrite2);
	DEFINE_BIF("sys:munge", 2, bif_sys_munge2);
	DEFINE_BIF("sys:upper", 2, bif_sys_upper2);
	DEFINE_BIF("sys:lower", 2, bif_sys_lower2);
	DEFINE_BIF("sys:splitl", 4, bif_sys_splitl4);
	DEFINE_BIF("sys:split", 4, bif_sys_split4);
	DEFINE_BIF("sys:split", 3, bif_sys_split3);
	DEFINE_BIF("sys:replace", 4, bif_sys_replace4);
	DEFINE_BIF("sys:begins", 2, bif_sys_begins2);
	DEFINE_BIF("sys:left", 3, bif_sys_left3);
	DEFINE_BIF("sys:right", 3, bif_sys_right3);
	DEFINE_BIF("sys:read_term_from_atom", 3, bif_sys_read_term_from_atom3);
	DEFINE_BIF("sys:atom_number", 2, bif_sys_atom_number2);
	DEFINE_BIF("sys:atom_timestamp", 2, bif_sys_atom_timestamp2);
	DEFINE_BIF("sys:format_rfcdate", 2, bif_sys_format_rfcdate2);
	DEFINE_BIF("sys:parse_rfcdate", 2, bif_sys_parse_rfcdate2);
	DEFINE_BIF("sys:url_encode", 2, bif_sys_url_encode2);
	DEFINE_BIF("sys:url_decode", 2, bif_sys_url_decode2);
	DEFINE_BIF("sys:b64_encode", 2, bif_sys_b64_encode2);
	DEFINE_BIF("sys:b64_decode", 2, bif_sys_b64_decode2);
	DEFINE_BIF("sys:parse_csv", 2, bif_sys_parse_csv);
	DEFINE_BIF("sys:parse_tab", 2, bif_sys_parse_tab);
}
