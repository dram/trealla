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
#include "internal.h"
#include "jela.h"

#ifndef ISO_ONLY
#include "base64.h"
#endif

extern const char *g_escapes;
extern const char *g_anti_escapes;

char *deescape(char *dst, const char *src, char quote)
{
	char *save = dst;

	while (*src) {
		const char *ptr = strrchr(g_escapes, *src);

		if (ptr) {
			*dst++ = '\\';
			*dst++ = g_anti_escapes[ptr - g_escapes];
		}
		else if (quote == *src) {
			*dst++ = '\\';
			*dst++ = *src;
		}
		else
			*dst++ = *src;

		src++;
	}

	*dst = 0;
	return save;
}

static int needs_quoting(const char *s)
{
	if (!*s)
		return 1;

	if (isupper(*s))
		return 1;

	while (*s) {
		unsigned char ch = *s++;

		if (!isalpha(ch) && !isdigit(ch) && (ch != '_') && (ch != ':') && (ch < 0x7F))
			return 1;
	}

	return 0;
}

static size_t _sprint_int(char *dst, size_t size, nbr_t n, int base)
{
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += _sprint_int(dst, size, n / base, base);

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

size_t sprint_int(char *dst, size_t size, nbr_t n, int base)
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
	else if (n < 0) {
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

static size_t sprint2_term(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *_n, int listing);

static size_t sprint2_list(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) {
		if (++q->print_depth > MAX_UNIFY_DEPTH) {
			QABORT2(ABORT_MAXDEPTH, "CYCLIC TERM");
			return 0;
		}
	}

	int save_context = q ? q->latest_context : -1;
	char *dst = *_dst;
	int inner = 0;

	for (;;) {
		if (inner)
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), ",");
		else
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "[");

		int this_context = q ? q->latest_context : -1;
		node *head = term_firstarg(n);
		node *tail = term_next(head);
		node *term = q ? get_arg(q, head, this_context) : head;
		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, listing);
		term = q ? get_arg(q, tail, this_context) : tail;

		if (is_list(term)) {
			inner = 1;
			n = term;
			continue;
		}

		if (strcmp(VAL_S(term), "[]")) {
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "|");
			dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, listing);
		}

		break;
	}

	dst += snprintf(dst, *bufsize - (dst - *dstbuf), "]");

	if (q) {
		q->latest_context = save_context;
		q->print_depth--;
	}

	return dst - *_dst;
}

static size_t sprint2_compound(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) {
		if (++q->print_depth > MAX_UNIFY_DEPTH) {
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
		node *term = q ? get_arg(q, n, save_context) : n;
		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, -listing);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "}");
	}
	else if ((listing < 2) && pl && is_infix(&pl->db, functor) && (arity > 1) && !ignore_ops) {
		if (!strcmp(functor, ",") || !strcmp(functor, ";"))
			if (listing == 1)
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "(");

		node *head = term_next(nf);

		if (is_hidden(head))
			head = term_next(head);

		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, head, listing);

		if (!is_fact(n)) {
			if (isalpha(functor[0]) || !strcmp(functor, ":-") || !strcmp(functor, ";") || !strcmp(functor, "-->") || !strcmp(functor, "->"))
				if (listing == 1)
					dst += snprintf(dst, *bufsize - (dst - *dstbuf), " ");

			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", tmpbuf);

			if (isalpha(functor[0]) || !strcmp(functor, ":-") || !strcmp(functor, ";") || !strcmp(functor, "-->") || !strcmp(functor, "->"))
				if (listing == 1)
					dst += snprintf(dst, *bufsize - (dst - *dstbuf), " ");

			if (!strcmp(functor, ":-") && (depth <= 2) && (listing == 1))
				dst += sprintf(dst, "\n\t");

			if (!strcmp(functor, "->") && (listing == 1))
				dst += sprintf(dst, "\n\t\t");

			if (!strcmp(functor, ";") && (listing == 1))
				dst += sprintf(dst, "\n\t\t");

			for (node *n = term_next(head); n != NULL; n = term_next(n)) {
				if (is_hidden(n))
					continue;

				node *term = q ? get_arg(q, n, save_context) : n;
				dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, listing);

				if (term_next(n))
					if (!is_hidden(term_next(n)))
						dst += snprintf(dst, *bufsize - (dst - *dstbuf), ",\n\t");
			}
		}

		if (!strcmp(functor, ",") || !strcmp(functor, ";"))
			if (listing == 1)
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), ")");
	}
	else if ((listing < 2) && pl && is_postfix(&pl->db, functor) && !ignore_ops) {
		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term_next(nf), listing);
		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, nf, listing);
	}
	else if ((listing < 2) && pl && is_prefix(&pl->db, functor) && !ignore_ops) {
		node *n = term_next(nf);

		if (is_hidden(n))
			n = term_next(n);

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", strcmp(functor, "--") ? functor : "-");

		if (strcmp(functor, "--"))
			dst += snprintf(dst, *bufsize - (dst - *dstbuf), " ");

		dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, n, listing);
	}
	else if (!isop || (nf->flags & FLAG_QUOTED) || (listing == 2) || ignore_ops) {
		if (!(n->flags & FLAG_CONSING) || (listing >= 2)) {
			const char *src = functor;

			if (!src || (isop && !ignore_ops))
				src = functor;

			if (((nf->flags & FLAG_QUOTED) || needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'");
			else if (((nf->flags & FLAG_DOUBLE_QUOTE) || needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"");

			dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", src);

			if (((nf->flags & FLAG_QUOTED) || needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'");
			else if (((nf->flags & FLAG_DOUBLE_QUOTE) || needs_quoting(src)) && (!isop || (listing == 2)))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"");

			n = term_next(nf);
		}
		else
			n = nf;

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "(");

		for (node *n1 = n; n1 != NULL; n1 = term_next(n1)) {
			if (is_hidden(n1))
				continue;

			node *term = q ? get_arg(q, n1, save_context) : n1;
			dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, listing);
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
			node *term = q ? get_arg(q, n, save_context) : n;
			dst += sprint2_term(++depth, dstbuf, bufsize, &dst, pl, q, term, listing);

			if (term_next(n))
				dst += snprintf(dst, *bufsize - (dst - *dstbuf), " ");
		}
	}

	if (q) {
		q->latest_context = save_context;
		q->print_depth--;
	}

	return dst - *_dst;
}

static size_t sprint2_term(int depth, char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *_n, int listing)
{
	if (q) {
		if (++q->print_depth > MAX_UNIFY_DEPTH) {
			QABORT2(ABORT_MAXDEPTH, "CYCLIC TERM");
			return 0;
		}
	}

	char *dst = *_dst;
	int save_context = q ? q->latest_context : -1;
	node *n = q ? get_arg(q, _n, q->latest_context) : _n;
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
		dst += sprint2_list(++depth, dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_compound(n))
		dst += sprint2_compound(++depth, dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_ptr(n) && listing) {
		*dst++ = '@';
		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_i, 10);
	}
#if USE_SSL
	else if (is_bignum(n) && listing) {
		dst += sprintf(dst, "%s", BN_bn2dec(n->val_bn));

		//if (BN_num_bits(n->val_bn) > 64)
		{
			*dst++ = 'B';
			*dst = '\0';
		}
	}
	else if (is_bignum(n)) {
		dst += sprintf(dst, "%s", BN_bn2dec(n->val_bn));
	}
#endif
	else if (is_integer(n) && (n->flags & FLAG_BINARY) && listing)
		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_u, 2);
	else if (is_integer(n) && (n->flags & FLAG_OCTAL) && listing)
		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_u, 8);
	else if (is_integer(n) && (n->flags & FLAG_HEX) && listing)
		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_u, 16);
	else if (is_integer(n))
		dst += sprint_int(dst, *bufsize - (dst - *dstbuf), n->val_i, 10);
	else if (is_float(n)) {
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
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "`%s`", tmpbuf);
		free(tmpbuf);
	}
	else if (is_blob(n)) {
		size_t len = (n->val_len < (*bufsize - (dst - *dstbuf)) ? n->val_len : (*bufsize - (dst - *dstbuf)));
		memcpy(dst, n->val_s, len);
		dst += len;
	}
#endif
	else if (listing && flag_character_escapes && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(VAL_S(n))) {
		char *tmp = (char *)malloc((strlen(VAL_S(n)) * 2) + 1);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"%s\"", deescape(tmp, VAL_S(n), '"'));
		free(tmp);
	}
	else if (listing && flag_character_escapes && is_atom(n) && (n->flags & FLAG_QUOTED) && !(n->flags & FLAG_NOOP) && needs_quoting(VAL_S(n))) {
		char *tmp = (char *)malloc((strlen(VAL_S(n)) * 2) + 1);
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'%s'", deescape(tmp, VAL_S(n), '\''));
		free(tmp);
	}
	else if (listing && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(VAL_S(n)))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "\"%s\"", VAL_S(n));
	else if (listing && is_atom(n) && (n->flags & FLAG_QUOTED) && needs_quoting(VAL_S(n)) && !(n->flags & FLAG_NOOP) )
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "'%s'", VAL_S(n));
	else if (listing && is_atom(n) && !(n->flags & FLAG_QUOTED) && strchr(VAL_S(n), '/') && 0) {
		const char *src = VAL_S(n);
		char tmpbuf[KEY_SIZE];
		strcpy(tmpbuf, src);
		char *end = strrchr(tmpbuf, '/');

		if (end)
			*end = '\0';

		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", tmpbuf);
	}
	else if (is_atom(n))
		dst += snprintf(dst, *bufsize - (dst - *dstbuf), "%s", VAL_S(n));

	if (q)
		q->print_depth--;

	if (q)
		q->latest_context = save_context;

	return dst - *_dst;
}

size_t term_sprint2(char **dstbuf, size_t *bufsize, char **dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	return sprint2_term(0, dstbuf, bufsize, dst, pl, q, n, listing);
}

size_t term_sprint(char *dstbuf_, size_t bufsize, trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *dstbuf = (char *)malloc(size + 1);
	char *dst = dstbuf;
	dst += sprint2_term(0, &dstbuf, &size, &dst, pl, q, n, listing);
	strncpy(dstbuf_, dstbuf, bufsize);
	dstbuf_[bufsize - 1] = '\0';
	free(dstbuf);
	return strlen(dstbuf_);
}

void term_print(trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *dstbuf = (char *)malloc(size + 1);
	char *dst = dstbuf;
	dst += sprint2_term(0, &dstbuf, &size, &dst, pl, q, n, listing);

	if (q)
		fwrite(dstbuf, 1, dst-dstbuf, q->curr_stdout);
	else
		fwrite(dstbuf, 1, dst - dstbuf, stdout);

	free(dstbuf);
}
