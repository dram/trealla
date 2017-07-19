#define _XOPEN_SOURCE
#include <time.h>

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

static int bif_posix_format_time_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_compound(term2);
	node *term3 = get_var(term3);

	if (strcmp(term_functor(term2), "time")) {
		return 0;
	}

	const char *format = VAL_S(term1);
	size_t length = strlen(format);

	// XXX: Is this check reasonable? May strftime() return non-empty
	// result for empty format?
	if (length == 0) {
		return unify_atom(q, term3, q->latest_context, strdup(""));
	}

	struct tm tm;
	node *n = term_firstarg(term2); tm.tm_sec = get_word(n);
	n = term_next(n); tm.tm_min = get_word(n);
	n = term_next(n); tm.tm_hour = get_word(n);
	n = term_next(n); tm.tm_mday = get_word(n);
	n = term_next(n); tm.tm_mon = get_word(n);
	n = term_next(n); tm.tm_year = get_word(n);
	n = term_next(n); tm.tm_wday = get_word(n);
	n = term_next(n); tm.tm_yday = get_word(n);
	n = term_next(n); tm.tm_isdst = get_word(n);

	char *buffer = NULL;
	int tries = 0;
	const int max_tries = 5;
	while (++tries <= max_tries) {
		// make enough space for some long formats, e.g. `%c'
		length = 128 + length * 2;
		char *buffer = realloc(buffer, length);

		// FIXME: `0' returned by strftime() does not always indicate
		// an error, seems there is no easy way to check that.
		if (strftime(buffer, length, format, &tm) > 0) {
			return unify_atom(q, term3, q->latest_context, buffer);
		}
	}

	free(buffer);

	// XXX: maybe raise an error?
	return 0;
}

static int bif_posix_gmt_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_var(term2);

	time_t t = (time_t)term1->val_i;
	struct tm tm;

	if (gmtime_r(&t, &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term2, term2_ctx, tmp, -1);

		return 1;
	}
}

static int bif_posix_local_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_int(term1);
	node *term2 = get_var(term2);

	time_t t = (time_t)term1->val_i;
	struct tm tm;

	if (localtime_r(&t, &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term2, term2_ctx, tmp, -1);

		return 1;
	}
}

// XXX: Is it desirable to also return corrected `tm'?
static int bif_posix_make_time_2(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_compound(term1);
	node *term2 = get_var(term2);

	if (strcmp(term_functor(term1), "time")) {
		return 0;
	}

	struct tm tm;
	node *n = term_firstarg(term1); tm.tm_sec = get_word(n);
	n = term_next(n); tm.tm_min = get_word(n);
	n = term_next(n); tm.tm_hour = get_word(n);
	n = term_next(n); tm.tm_mday = get_word(n);
	n = term_next(n); tm.tm_mon = get_word(n);
	n = term_next(n); tm.tm_year = get_word(n);
	n = term_next(n); tm.tm_wday = get_word(n);
	n = term_next(n); tm.tm_yday = get_word(n);
	n = term_next(n); tm.tm_isdst = get_word(n);

	time_t t = mktime(&tm);

	if (t == -1) {
		return 0;
	} else {
		return unify_int(q, term2, q->latest_context, t);
	}
}

// XXX: To be consistent with posix:format_time/3, order by arguments
// is different from strptime(), is this choice reasonable?
static int bif_posix_parse_time_3(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_atom(term1);
	node *term2 = get_atom(term2);
	node *term3 = get_var(term3);

	struct tm tm;

	memset(&tm, 0, sizeof(struct tm));

	if (strptime(VAL_S(term2), VAL_S(term1), &tm) == NULL) {
		return 0;
	} else {
		node *tmp = make_compound();

		term_append(tmp, make_const_atom("time"));
		term_append(tmp, make_int(tm.tm_sec));
		term_append(tmp, make_int(tm.tm_min));
		term_append(tmp, make_int(tm.tm_hour));
		term_append(tmp, make_int(tm.tm_mday));
		term_append(tmp, make_int(tm.tm_mon));
		term_append(tmp, make_int(tm.tm_year));
		term_append(tmp, make_int(tm.tm_wday));
		term_append(tmp, make_int(tm.tm_yday));
		term_append(tmp, make_int(tm.tm_isdst));

		unify(q, term3, term3_ctx, tmp, -1);

		return 1;
	}
}

static int bif_posix_time_1(tpl_query *q)
{
	node *args = get_args(q);
	node *term1 = get_var(term1);

	time_t t = time(NULL);

	if (t == -1) {
		return 0;
	} else {
		return unify_int(q, term1, q->latest_context, t);
	}
}

void bifs_load_posix(void)
{
	DEFINE_BIF("posix:format_time", 3, bif_posix_format_time_3);
	DEFINE_BIF("posix:gmt_time", 2, bif_posix_gmt_time_2);
	DEFINE_BIF("posix:local_time", 2, bif_posix_local_time_2);
	DEFINE_BIF("posix:make_time", 2, bif_posix_make_time_2);
	DEFINE_BIF("posix:parse_time", 3, bif_posix_parse_time_3);
	DEFINE_BIF("posix:time", 1, bif_posix_time_1);
}
