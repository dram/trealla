#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "trealla.h"

#include "bifs.h"
#include "jela.h"

#ifndef ISO_ONLY
#include "base64.h"
#endif

extern const char *g_escapes;
extern const char *g_anti_escapes;

char *deescape(char *dst, const char *src, char quote)
{
	char *save = dst;
	int ch;

	while ((ch = get_char_utf8(&src)) != '\0') {
		const char *ptr = strrchr(g_escapes, ch);

		if (ptr) {
			*dst++ = '\\';
			*dst++ = g_anti_escapes[ptr - g_escapes];
		}
		else if (quote == ch) {
			*dst++ = '\\';
			*dst++ = ch;
		}
		else if (iscntrl(ch)) {
			int v = ch;
			*dst++ = '\\';
			dst += sprintf(dst, "%02o", v);
			*dst++ = '\\';
		}
		else
			dst += put_char_utf8(dst, ch);
	}

	*dst = 0;
	return save;
}

static size_t _sprint_int(char *dst, size_t size, nbr_t n, int base)
{
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += _sprint_int(dst, size, n / base, base);

	int n2 = n % base;
	n2 += '0';

	*dst++ = (char)n2;
	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t size, nbr_t n)
{
	const char *save_dst = dst;
	const int base = 10;

	if (n < 0) {
		*dst++ = '-';
		n = -n;
	}

	if (n == 0) {
		*dst++ = '0';
		*dst = '\0';
		return dst - save_dst;
	}

	dst += _sprint_int(dst, size, n, base);
	*dst = '\0';
	return dst - save_dst;
}

static size_t _sprint_uint(char *dst, size_t size, unbr_t n, int base)
{
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += _sprint_uint(dst, size, n / base, base);

	int n2 = n % base;

	if (base == 16) {
		if (n2 < 10)
			n2 += '0';
		else
			n2 = 'A' + (n2 - 10);
	}
	else
		n2 += '0';

	*dst++ = (char)n2;
	return dst - save_dst;
}

size_t sprint_uint(char *dst, size_t size, unbr_t n, int base)
{
	const char *save_dst = dst;

	if (base == 16) {
		*dst++ = '0';
		*dst++ = 'x';
	}
	else if (base == 8) {
		*dst++ = '0';
		*dst++ = 'o';
	}
	else if (base == 2) {
		*dst++ = '0';
		*dst++ = 'b';
	}

	if (n == 0) {
		*dst++ = '0';
		*dst = '\0';
		return dst - save_dst;
	}

	dst += _sprint_uint(dst, size, n, base);
	*dst = '\0';
	return dst - save_dst;
}

static size_t sprint2_term(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *_n, int listing);

static size_t sprint2_list(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) {
		if (depth > MAX_UNIFY_DEPTH) {
			QABORT2(ABORT_MAXDEPTH, "CYCLIC TERM");
			return 0;
		}
	}

	int save_context = q ? q->latest_context : -1;
	char *dst = *_dst;
	dst += snprintf(dst, *bufsize - (dst - *dstbuf), "[");
	int inner = 0;

	for (;;) {
		if (inner)
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), ",");

		int this_context = q ? q->latest_context : -1;
		node *head = term_firstarg(n);
		node *tail = term_next(head);
		node *term = q ? subst(q, head, this_context) : head;
		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, listing ? listing : 1);
		term = q ? subst(q, tail, this_context) : tail;

		if (is_list(term)) {
			inner = 1;
			n = term;
			continue;
		}

		if (!is_atom(term) || strcmp(VAL_S(term), "[]")) {
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "|");
			dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, listing ? listing : 1);
		}

		break;
	}

	dst += snprintf(dst, *bufsize - (dst - *dstbuf), "]");

	if (q)
		q->latest_context = save_context;

	return dst - *_dst;
}

static size_t sprint2_compound(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) {
		if (depth > MAX_UNIFY_DEPTH) {
			QABORT2(ABORT_MAXDEPTH, "CYCLIC TERM");
			return 0;
		}
	}

	char *dst = *_dst;
	int save_context = q ? q->latest_context : -1;
	char tmpbuf[KEY_SIZE];
	tmpbuf[0] = 0;
	node *nf = term_first(n);
	int arity = term_arity(n);

	if (is_atom(nf)) {
		const char *src = VAL_S(nf);
		strcpy(tmpbuf, src);
	}

	tmpbuf[sizeof(tmpbuf) - 1] = 0;
	const char *functor = tmpbuf;
	int isop = pl ? is_op(&pl->db, functor) : 0;
	int ignore_ops = q ? q->ignore_ops : 0;

	if (is_tuple(n)) {
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "{");
		n = term_next(nf);
		node *term = q ? subst(q, n, save_context) : n;
		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, -listing ? listing : 1);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "}");
	}
	else if ((listing < 2) && pl && is_infix(&pl->db, functor) && (arity > 1) && !ignore_ops) {
		int parens = 0;

		if (((listing == 1) || (depth > 2)) && strcmp(functor, ":")) {
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "(");
			parens = 1;
		}

		node *head = term_next(nf);

		if (is_hidden(head))
			head = term_next(head);

		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, head, listing ? listing : 1);

		if (!is_fact(n)) {
			if ((listing <= 1) && strcmp(functor, "is"))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", tmpbuf);
			else
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), " %s ", tmpbuf);

			for (node *n = term_next(head); n != NULL; n = term_next(n)) {
				if (is_hidden(n))
					continue;

				node *term = q ? subst(q, n, save_context) : n;
				dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, listing ? listing : 1);

				if (term_next(n))
					if (!is_hidden(term_next(n)))
						dst += snprintf(dst, *bufsize - (dst - *dstbuf), ",\n\t");
			}
		}

		if (parens)
			if (listing == 1)
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), ")");
	}
	else if ((listing < 2) && pl && is_postfix(&pl->db, functor) && !ignore_ops) {
		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term_next(nf), listing ? listing : 1);
		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, nf, listing ? listing : 1);
	}
	else if ((listing < 2) && pl && is_prefix(&pl->db, functor) && !ignore_ops) {
		node *n = term_next(nf);

		if (is_hidden(n))
			n = term_next(n);

		if (!strcmp(functor, "]-["))
			functor = "-";
		else if (!strcmp(functor, "]+["))
			functor = "+";

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s%s", functor, isalpha(functor[0])?" ":"");
		dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, n, listing ? listing : 1);
	}
	else if (!isop || (listing == 2) || ignore_ops) {
		if (!(n->flags & FLAG_CONSING) || (listing >= 2)) {
			if (!strcmp(functor, "]-["))
				functor = " -";
			else if (!strcmp(functor, "]+["))
				functor = " +";

			const char *src = functor;

			if (((nf->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"");
			else if ((needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'");

			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", src);

			if (((nf->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"");
			else if ((needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'");

			n = term_next(nf);
		}
		else
			n = nf;

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "(");

		for (node *n1 = n; n1 != NULL; n1 = term_next(n1)) {
			if (is_hidden(n1))
				continue;

			node *term = q ? subst(q, n1, save_context) : n1;
			dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, listing ? listing : 1);
			node *n2 = term_next(n1);

			while (n2 && is_hidden(n2))
				n2 = term_next(n2);

			if (n2)
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), ",");
		}

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), ")");
	}
	else {
		for (node *n = nf; n != NULL; n = term_next(n)) {
			node *term = q ? subst(q, n, save_context) : n;
			dst += sprint2_term(depth+1, dstbuf, bufsize, &dst, pl, q, term, listing ? listing : 1);

			if (term_next(n))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), " ");
		}
	}

	if (q)
		q->latest_context = save_context;

	return dst - *_dst;
}

static size_t sprint2_term(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *_n, int listing)
{
	if (q) {
		if (depth > MAX_UNIFY_DEPTH) {
			QABORT2(ABORT_MAXDEPTH, "CYCLIC TERM");
			return 0;
		}
	}

	char *dst = *_dst;
	int save_context = q ? q->latest_context : -1;
	node *n = q ? subst(q, _n, q->latest_context) : _n;
	size_t xlen = (is_atom(n) ? LEN(n) : 64) + 1024;
	size_t rem = (*bufsize - (dst - *dstbuf));

	if (rem < xlen) {
		size_t offset = dst - *dstbuf;
		*bufsize *= 2;

		if (*bufsize < xlen)
			*bufsize += xlen;

		*dstbuf = (char *)realloc(*dstbuf, *bufsize);
		*_dst = *dstbuf + offset;
		dst = *_dst;
	}

	int flag_character_escapes = pl ? pl->flag_character_escapes : 1;

	if (is_list(n) && (listing < 2))
		dst += sprint2_list(depth+1, dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_compound(n))
		dst += sprint2_compound(depth+1, dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_file(n) && needs_quoting(n->val_str->filename) && listing)
		dst += sprintf(dst, "'%s'", n->val_str->filename);
	else if (is_file(n))
		dst += sprintf(dst, "%s", n->val_str->filename);
	else if (is_ptr(n) && listing) {
		*dst++ = '@';
		dst += sprint_uint(dst, *bufsize - (dst - *dstbuf), n->val_i, 10);
	}
#if USE_SSL
	else if (is_bignum(n) && listing) {
		char *src = BN_bn2dec(n->val_bn);
		xlen = strlen(src) + 1024;
		rem = (*bufsize - (dst - *dstbuf));

		if (rem < xlen) {
			size_t offset = dst - *dstbuf;
			*bufsize *= 2;

			if (*bufsize < xlen)
				*bufsize += xlen;

			*dstbuf = (char *)realloc(*dstbuf, *bufsize);
			*_dst = *dstbuf + offset;
			dst = *_dst;
		}

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", src);
		OPENSSL_free(src);

		if ((BN_num_bits(n->val_bn) < 64) && !g_force_unbounded)
		{
			*dst++ = 'B';
			*dst = '\0';
		}
	}
	else if (is_bignum(n)) {
		char *src = BN_bn2dec(n->val_bn);
		xlen = strlen(src) + 1024;
		rem = (*bufsize - (dst - *dstbuf));

		if (rem < xlen) {
			size_t offset = dst - *dstbuf;
			*bufsize *= 2;

			if (*bufsize < xlen)
				*bufsize += xlen;

			*dstbuf = (char *)realloc(*dstbuf, *bufsize);
			*_dst = *dstbuf + offset;
			dst = *_dst;
		}

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", src);
		OPENSSL_free(src);
	}
#endif
	else if (is_integer(n) && (n->flags & FLAG_BINARY) && listing)
		dst += sprint_uint(dst, *bufsize - (dst - *dstbuf), n->val_u, 2);
	else if (is_integer(n) && (n->flags & FLAG_OCTAL) && listing)
		dst += sprint_uint(dst, *bufsize - (dst - *dstbuf), n->val_u, 8);
	else if (is_integer(n) && (n->flags & FLAG_HEX) && listing)
		dst += sprint_uint(dst, *bufsize - (dst - *dstbuf), n->val_u, 16);
	else if (is_integer(n)) {
		static const char *s_chars = "*/+-=.";
		if ((n->val_i < 0) && (dst != *dstbuf) && strchr(s_chars, *(dst-1)))
			*dst++ = ' ';

		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_i);
	}
	else if (is_float(n)) {
		if (n->val_f < 0)
			*dst++ = ' ';

		const char *save_dst = dst;
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%.*g", DBL_DIG, (double)n->val_f);

		if (!strchr(save_dst, '.'))
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), ".0");
	}
	else if (is_var(n) && !listing)
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "_%d", n->slot);
	else if (listing && is_var(n) && (n->flags & FLAG_ANON))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "_");
	else if (is_var(n))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", VAL_S(n));
#ifndef ISO_ONLY
	else if (listing && is_blob(n)) {
		char *tmpbuf = (char *)malloc((LEN(n) * 3) + 1);
		b64_encode(n->val_s, n->val_len, &tmpbuf, 0, 0);
		xlen = strlen(tmpbuf) + 1024;
		rem = (*bufsize - (dst - *dstbuf));

		if (rem < xlen) {
			size_t offset = dst - *dstbuf;
			*bufsize *= 2;

			if (*bufsize < xlen)
				*bufsize += xlen;

			*dstbuf = (char *)realloc(*dstbuf, *bufsize);
			*_dst = *dstbuf + offset;
			dst = *_dst;
		}

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "`%s`", tmpbuf);
		free(tmpbuf);
	}
	else if (is_blob(n)) {
		size_t len = (n->val_len < (*bufsize - (dst - *dstbuf)) ? n->val_len : (*bufsize - (dst - *dstbuf)));
		xlen = len + 1024;
		rem = (*bufsize - (dst - *dstbuf));

		if (rem < xlen) {
			size_t offset = dst - *dstbuf;
			*bufsize *= 2;

			if (*bufsize < xlen)
				*bufsize += xlen;

			*dstbuf = (char *)realloc(*dstbuf, *bufsize);
			*_dst = *dstbuf + offset;
			dst = *_dst;
		}

		memcpy(dst, n->val_s, len);
		dst += len;
	}
#endif
	else if (listing && flag_character_escapes && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(VAL_S(n))) {
		char *tmp = (char *)malloc((strlen(VAL_S(n)) * 2) + 1);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"%s\"", deescape(tmp, VAL_S(n), '"'));
		free(tmp);
	}
	else if (listing && flag_character_escapes && is_atom(n) && needs_quoting(VAL_S(n)) && !(n->flags & FLAG_NOOP)) {
		char *tmp = (char *)malloc((strlen(VAL_S(n)) * 2) + 1);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'%s'", deescape(tmp, VAL_S(n), '\''));
		free(tmp);
	}
	else if (listing && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(VAL_S(n)))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"%s\"", VAL_S(n));
	else if (listing && is_atom(n) && needs_quoting(VAL_S(n)) && !(n->flags & FLAG_NOOP))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'%s'", VAL_S(n));
	else if ((listing == 2) && is_atom(n) && needs_quoting(VAL_S(n)))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'%s'", VAL_S(n));
	else if (is_atom(n))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", VAL_S(n));

	if (q)
		q->latest_context = save_context;

	return dst - *_dst;
}

size_t term_sprint2(char **dstbuf, size_t *bufsize, char **dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	return sprint2_term(0, dstbuf, bufsize, dst, pl, q, n, listing);
}

size_t term_sprint(char *dstbuf, size_t bufsize, trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(size + 1);
	char *dst = tmpbuf;
	dst += sprint2_term(0, &tmpbuf, &size, &dst, pl, q, n, listing);
	size_t len = dst - tmpbuf;
	strncpy(dstbuf, tmpbuf, bufsize);
	dstbuf[bufsize - 1] = '\0';
	free(tmpbuf);
	return len;
}

void term_print(trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *tmpbuf = (char *)malloc(size + 1);
	char *dst = tmpbuf;
	dst += sprint2_term(0, &tmpbuf, &size, &dst, pl, q, n, listing);
	size_t len = dst - tmpbuf;

	if (q)
		fwrite(tmpbuf, 1, len, q->curr_stdout);
	else
		fwrite(tmpbuf, 1, len, stdout);

	free(tmpbuf);
}
