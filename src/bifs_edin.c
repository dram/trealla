#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <locale.h>

#ifdef _WIN32
#include <io.h>
#define snprintf _snprintf
#define fseeko _fseeki64
#define ftello _ftelli64
#define isatty _isatty
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

#ifndef ISO_ONLY

static int bif_edin_display_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_term(term2);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	q->ignore_ops = 1;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term2, 0);
	q->ignore_ops = 0;

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	int ok;

#ifndef ISO_ONLY
	stream *sp = term1->val_str;

	if (is_socket(term1))
		ok = session_write((session *)sp->sptr, tmpbuf, len);
	else
#endif
		ok = fwrite(tmpbuf, 1, len, get_output_stream(term1));

	free(tmpbuf);
	return ok > 0;
}

static int bif_edin_display_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_term(term1);
	size_t max_len = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(max_len + 1);
	char *dst = tmpbuf;
	q->ignore_ops = 1;
	size_t len = term_sprint2(&tmpbuf, &max_len, &dst, q->pl, q, term1, 0);
	q->ignore_ops = 0;

	if (q->halt) {
		free(tmpbuf);
		return 0;
	}

	fwrite(tmpbuf, 1, len, q->curr_stdout);
	free(tmpbuf);
	return 1;
}

static int bif_edin_see_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);

		if (!strcmp(q->curr_stdin_name, filename))
			return 1;

		if (!strcmp(filename, "user") || !strcmp(filename, "user_input")) {
			free(q->curr_stdin_name);
			stream *sp = (stream*)calloc(1, sizeof(stream));
			sp->filename = strdup("user");
			sp->fptr = stdin;
			q->curr_stdin_stream = sp;
			q->curr_stdin = stdin;
			q->curr_stdin_name = strdup("user");
			return 1;
		}

		FILE *fp = fopen(filename, "r");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			return 0;
		}

		free(q->curr_stdin_name);
		stream *sp = (stream*)calloc(1, sizeof(stream));
		sp->filename = strdup(filename);
		sp->fptr = fp;
		q->curr_stdin_stream = sp;
		q->curr_stdin = fp;
		q->curr_stdin_name = strdup(filename);
	}
	else {
		free(q->curr_stdin_name);
		stream *sp = term1->val_str;
		q->curr_stdin_stream = sp;
		q->curr_stdin = sp->fptr;
		q->curr_stdin_name = strdup(sp->filename);
	}

	return 1;
}

static int bif_edin_seeing_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);

	if (q->curr_stdin_stream) {
		node *n = make_stream(q->curr_stdin_stream);
		n->flags |= FLAG_FILE | FLAG_CONST;
		int ok = unify(q, term1, term1_ctx, n, -1);
		term_heapcheck(n);
		return ok;
	}
	else
		return unify_atom(q, term1, q->latest_context, strdup(q->curr_stdin_name));
}

static int bif_edin_seen_0(tpl_query *q)
{
	if (q->curr_stdin != stdin) {
		free(q->curr_stdin_name);
		fclose(q->curr_stdin);
		q->curr_stdin_stream->fptr = stdin;
		q->curr_stdin_stream = NULL;
		q->curr_stdin_name = strdup("user");
		q->curr_stdin = stdin;
	}

	return 1;
}

static int bif_edin_tell_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);

		if (!strcmp(filename, q->curr_stdout_name))
			return 1;

		if (!strcmp(filename, "user") || !strcmp(filename, "user_output")) {
			free(q->curr_stdout_name);
			stream *sp = (stream*)calloc(1, sizeof(stream));
			sp->filename = strdup("user");
			sp->fptr = stdout;
			q->curr_stdout_stream = sp;
			q->curr_stdout = stdout;
			q->curr_stdout_name = strdup("user");
			return 1;
		}

		FILE *fp = fopen(filename, "w");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			q->curr_stdout_name = strdup("user");
			q->curr_stdout = stdout;
			return 0;
		}

		free(q->curr_stdout_name);
		stream *sp = (stream*)calloc(1, sizeof(stream));
		sp->filename = strdup(filename);
		sp->fptr = fp;
		q->curr_stdout_stream = sp;
		q->curr_stdout = fp;
		q->curr_stdout_name = strdup(filename);
	}
	else {
		free(q->curr_stdout_name);
		stream *sp = term1->val_str;
		q->curr_stdout_stream = sp;
		q->curr_stdout = sp->fptr;
		q->curr_stdout_name = strdup(sp->filename);
	}

	return 1;
}

static int bif_edin_append_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);

	if (q->curr_stdout_name)
		free(q->curr_stdout_name);

	if (is_atom(term1)) {
		const char *filename = VAL_S(term1);
		FILE *fp = fopen(filename, "a");

		if (!fp) {
			QABORT(ABORT_NOTEXISTFILE);
			q->curr_stdout_name = strdup("user");
			q->curr_stdout = stdout;
			return 0;
		}

		q->curr_stdout_name = strdup(filename);
		q->curr_stdout = fp;
	}
	else {
		stream *sp = term1->val_str;
		q->curr_stdout_name = strdup(sp->filename);
		q->curr_stdout_stream = sp;
		q->curr_stdout = sp->fptr;
	}

	return 1;
}

static int bif_edin_telling_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_var(term1);

	if (q->curr_stdout_stream) {
		node *n = make_stream(q->curr_stdout_stream);
		n->flags |= FLAG_FILE | FLAG_CONST;
		int ok = unify(q, term1, term1_ctx, n, -1);
		term_heapcheck(n);
		return ok;
	}
	else
		return unify_atom(q, term1, q->latest_context, strdup(q->curr_stdout_name));
}

static int bif_edin_told_0(tpl_query *q)
{
	if (q->curr_stdout != stdout) {
		free(q->curr_stdout_name);
		fclose(q->curr_stdout);
		q->curr_stdout_stream->fptr = stdout;
		q->curr_stdout_stream = NULL;
		q->curr_stdout_name = strdup("user");
		q->curr_stdout = stdout;
	}

	return 1;
}

static int bif_edin_get_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int_or_var(term1);

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

LOOP:

	q->did_getc = 1;
	int ch = getc_utf8(q->curr_stdin);

	if (ch == EOF)
		q->did_getc = 0;
	else {
		if (ch == '\n')
			q->did_getc = 0;
		else if (isspace(ch))
			goto LOOP;
	}

	return unify_int(q, term1, q->latest_context, ch);
}

static int bif_edin_get_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int_or_var(term2);
	int ch;

LOOP:

	ch = getc_utf8(get_input_stream(term1));

	if (ch != EOF) {
		if (isspace(ch))
			goto LOOP;
	}

	return unify_int(q, term2, q->latest_context, ch);
}

static int bif_edin_skip_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);

	if (q->pl->tty && !q->curr_stdin_name && !q->did_getc) {
		printf("| ");
		fflush(q->curr_stdout);
	}

LOOP:

	q->did_getc = 1;
	int ch = getc_utf8(q->curr_stdin);

	if (ch == EOF)
		q->did_getc = 0;
	else {
		if (ch == '\n')
			q->did_getc = 0;

		if (ch != VAL_INT(term1))
			goto LOOP;
	}

	return 1;
}

static int bif_edin_skip_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int(term2);
	int ch;

LOOP:

	ch = getc_utf8(get_input_stream(term1));

	if (ch != EOF) {
		if (ch != VAL_INT(term2))
			goto LOOP;
	}

	return 1;
}

static int bif_edin_tab_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom_or_stream(term1);
	node *term2 = get_int(term2);
	stream *sp = term1->val_str;
	int n = VAL_INT(term2);
	int ok = 1;

	for (int i = 0; ok && (i < n); i++) {
#ifndef ISO_ONLY
		if (is_socket(term1))
			ok = session_write((session *)sp->sptr, "\n", 1);
		else
#endif
			ok = fwrite(" ", 1, 1, get_output_stream(term1));
	}

	return ok > 0;
}

static int bif_edin_tab_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	int n = VAL_INT(term1);
	int ok = 1;

	for (int i = 0; ok && (i < n); i++)
		ok = fwrite(" ", 1, 1, q->curr_stdout);

	return ok > 0;
}

#endif

void bifs_load_edin(void)
{
#ifndef ISO_ONLY
	DEFINE_BIF("see", 1, bif_edin_see_1);
	DEFINE_BIF("seeing", 1, bif_edin_seeing_1);
	DEFINE_BIF("seen", 0, bif_edin_seen_0);
	DEFINE_BIF("tell", 1, bif_edin_tell_1);
	DEFINE_BIF("append", 1, bif_edin_append_1);
	DEFINE_BIF("telling", 1, bif_edin_telling_1);
	DEFINE_BIF("told", 0, bif_edin_told_0);
	DEFINE_BIF("tab", 1, bif_edin_tab_1);
	DEFINE_BIF("tab", 2, bif_edin_tab_2);
	DEFINE_BIF("get0", 1, bif_iso_get_code);
	DEFINE_BIF("get0", 2, bif_iso_get_code);
	DEFINE_BIF("get", 1, bif_edin_get_1);
	DEFINE_BIF("get", 2, bif_edin_get_2);
	DEFINE_BIF("skip", 1, bif_edin_skip_1);
	DEFINE_BIF("skip", 2, bif_edin_skip_2);
	DEFINE_BIF("display", 1, bif_edin_display_1);
	DEFINE_BIF("display", 2, bif_edin_display_2);
#endif
}
