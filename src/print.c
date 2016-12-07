#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <float.h>

#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "list.h"
#include "trealla.h"
#include "internal.h"
#include "jela.h"
#include "bifs.h"

#ifndef ISO_ONLY
#include "base64.h"
#endif

extern const char *g_escapes;
extern const char *g_anti_escapes;

char *deescape(char *dst, const char *src, char quote)
{
	char *save = dst;

	while (*src)
	{
		const char *ptr = strrchr(g_escapes, *src);

		if (ptr)
		{
			*dst++ = '\\';
			*dst++ = g_anti_escapes[ptr-g_escapes];
		}
		else if (quote == *src)
		{
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
	if (!*s || isupper(*s) || !isalpha(*s))
		return 1;

	if (*s == '_')
		s++;

	while (*s)
	{
		if (!isalnum(*s) && (*s != '_') && (*s != ':'))
			return 1;

		s++;
	}

	return 0;
}

nbr_t dec_to_int(const char *src)
{
	nbr_t v = 0;
	int neg = 0;

	if (*src == '-')
	{
		neg = 1;
		src++;
	}

	while (*src)
	{
		v *= 10;
		v += *src - '0';
		src++;
	}

	return neg ? -v : v;
}

static size_t _sprint_int(char *dst, size_t size, nbr_t n, int base)
{
	const char *save_dst = dst;

	if ((n/base) > 0)
		dst += _sprint_int(dst, size, n/base, base);

	char n2 = n % base;

	if (base == 16)
	{
		if (n2 < 10)
			n2 += '0';
		else
			n2 = 'A' + (n2-10);
	}
	else
		n2 += '0';

	*dst++ = n2;
	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t size, nbr_t n, int base)
{
	const char *save_dst = dst;

	if (base == 16)
	{
		*dst++ = '0';
		*dst++ = 'x';
	}
	else if (base == 8)
	{
		*dst++ = '0';
		*dst++ = 'o';
	}
	else if (base == 2)
	{
		*dst++ = '0';
		*dst++ = 'b';
	}
	else if (n < 0)
	{
		*dst++ = '-';
		n = -n;
	}

	if (n == 0)
	{
		*dst++ = '0';
		*dst = '\0';
		return dst - save_dst;
	}

	dst += _sprint_int(dst, size, n, base);
	*dst = '\0';
	return dst - save_dst;
}

static size_t sprint2_list(char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) if (++q->print_depth > MAX_UNIFY_DEPTH)
		{ QABORT2(ABORT_MAXDEPTH,"CYCLIC TERM"); return 0; }

	int this_context = q?q->latest_context:-1;
	char *dst = *_dst;
	int inner = 0;

	while (n)
	{
		if (inner) dst += snprintf(dst, *bufsize-(dst-*dstbuf), ",");
		else dst += snprintf(dst, *bufsize-(dst-*dstbuf), "[");

		int this_context = q?q->latest_context:-1;
		node *head = NLIST_NEXT(NLIST_FRONT(&n->val_l));
		node *term = q?get_arg(q, head, this_context):head;
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, term, listing>0?listing:1);
		node *tail = NLIST_NEXT(head);
		term = q?get_arg(q, tail, this_context):tail;

		if (is_list(term))
		{
			inner = 1;
			n = term;
			continue;
		}

		if (strcmp(term->val_s, "[]"))
		{
			dst += snprintf(dst, *bufsize-(dst-*dstbuf), "|");
			dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, term, listing>0?listing:1);
		}

		break;
	}

	dst += snprintf(dst, *bufsize-(dst-*dstbuf), "]");
	if (q) q->latest_context = this_context;
	if (q) q->print_depth--;
	return dst - *_dst;
}

static size_t sprint2_compound(char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *n, int listing)
{
	if (q) if (++q->print_depth > MAX_UNIFY_DEPTH)
		{ QABORT2(ABORT_MAXDEPTH,"CYCLIC TERM"); return 0; }

	char *dst = *_dst;
	int this_context = q?q->latest_context:-1;
	char tmpbuf[KEY_SIZE];
	tmpbuf[0] = 0;
	node *nf = NLIST_FRONT(&n->val_l);

	if (is_atom(nf) && !(is_op(&pl->db, nf->val_s)))
	{
		const char *src = nf->val_s;
		strcpy(tmpbuf, src);
		char *end = strrchr(tmpbuf, ARITY_CHAR);
		if (end) *end = '\0';
	}
	else if (is_atom(nf))
	{
		const char *src = nf->val_s;
		strcpy(tmpbuf, src);
		char *end = strrchr(tmpbuf, ARITY_CHAR);
		if (end) *end = '\0';
	}

	tmpbuf[sizeof(tmpbuf)-1] = 0;
	const char *functor = tmpbuf;
	int op = is_op(&pl->db, functor);

	if (is_tuple(n))
	{
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "{");
		n = NLIST_NEXT(nf);
		node *term = q?get_arg(q, n, this_context):n;
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, term, listing>0?listing:1);
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "}");
	}
	else if ((listing < 2) && is_infix(&pl->db, functor))
	{
		if (!strcmp(functor,",")||!strcmp(functor,";"))
			dst += snprintf(dst, *bufsize-(dst-*dstbuf), "(");

		node *head = NLIST_NEXT(nf);
		if (head->flags & FLAG_HIDDEN) head = NLIST_NEXT(head);
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, head, listing>0?listing:1);

		if (!(n->flags & FLAG_FACT))
		{
			if (isalpha(functor[0]) || !strcmp(functor, ":-") || !strcmp(functor, ";") || !strcmp(functor, "->")) dst += snprintf(dst, *bufsize-(dst-*dstbuf), " ");
			dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", tmpbuf);
			if (isalpha(functor[0]) || !strcmp(functor, ":-") || !strcmp(functor, ";") || !strcmp(functor, "->")) dst += snprintf(dst, *bufsize-(dst-*dstbuf), " ");
			if (!strcmp(functor, ":-")) dst += sprintf(dst, "\n\t");
			if (!strcmp(functor, "->")) dst += sprintf(dst, "\n\t\t");
			if (!strcmp(functor, ";")) dst += sprintf(dst, "\n\t\t");

			for (n = NLIST_NEXT(head); n; n = NLIST_NEXT(n))
			{
				if (n->flags & FLAG_HIDDEN) continue;
				dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, n, listing>0?listing:1);
				if (NLIST_NEXT(n)) if (!(NLIST_NEXT(n)->flags & FLAG_HIDDEN)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), ",\n\t");
			}
		}

		if (!strcmp(functor,",")||!strcmp(functor,";"))
			dst += snprintf(dst, *bufsize-(dst-*dstbuf), ")");
	}
	else if ((listing < 2) && is_postfix(&pl->db, functor))
	{
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, NLIST_NEXT(nf), listing>0?listing:1);
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, nf, listing>0?listing:1);
	}
	else if ((listing < 2) && is_prefix(&pl->db, functor))
	{
		node *n = NLIST_NEXT(nf);
		if (n->flags & FLAG_HIDDEN) n = NLIST_NEXT(n);
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", strcmp(functor, "--")?functor:"-");
		if (strcmp(functor, "--")) dst += snprintf(dst, *bufsize-(dst-*dstbuf), " ");
		dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, n, listing>0?listing:1);
	}
	else if (!op || (nf->flags & FLAG_QUOTED) || (listing == 2))
	{
		if (!(n->flags & FLAG_CONSING))
		{
			const char *src = op?functor:functor;
			if (!src||op) src = functor;
			if ((nf->flags&FLAG_QUOTED)||needs_quoting(src)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), "'");
			else if ((nf->flags&FLAG_DOUBLE_QUOTE)||needs_quoting(src)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), "\"");
			dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", src);
			if ((nf->flags&FLAG_QUOTED)||needs_quoting(src)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), "'");
			else if ((nf->flags&FLAG_DOUBLE_QUOTE)||needs_quoting(src)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), "\"");
			n = NLIST_NEXT(nf);
		}
		else
			n = nf;

		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "(");

		for (; n; n = NLIST_NEXT(n))
		{
			if (n->flags & FLAG_HIDDEN) continue;
			node *term = q?get_arg(q, n, this_context):n;
			dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, term, listing>0?listing:1);
			node *n2 = NLIST_NEXT(n);

			if (n2) if (!(n2->flags & FLAG_HIDDEN))
				dst += snprintf(dst, *bufsize-(dst-*dstbuf), ",");
		}

		dst += snprintf(dst, *bufsize-(dst-*dstbuf), ")");
	}
	else
	{
		for (n = nf; n; n = NLIST_NEXT(n))
		{
			node *term = q?get_arg(q, n, this_context):n;
			dst += sprint2_term(dstbuf, bufsize, &dst, pl, q, term, listing>0?listing:1);
			if (NLIST_NEXT(n)) dst += snprintf(dst, *bufsize-(dst-*dstbuf), " ");
		}
	}

	if (q) q->latest_context = this_context;
	if (q) q->print_depth--;
	return dst - *_dst;
}

size_t sprint2_term(char **dstbuf, size_t *bufsize, char **_dst, trealla *pl, tpl_query *q, node *_n, int listing)
{
	if (q) if (++q->print_depth > MAX_UNIFY_DEPTH)
		{ QABORT2(ABORT_MAXDEPTH,"CYCLIC TERM"); return 0; }

	char *dst = *_dst;
	int this_context = q?q->latest_context:-1;
	node *n = q?get_arg(q, _n, q->latest_context):_n;
	size_t xlen = (is_atom(n) ? LEN(n) : 64) + 1024;
	size_t rem = (*bufsize-(dst-*dstbuf));

	if (rem < xlen)
	{
		size_t offset = dst - *dstbuf;
		*bufsize *= 2;

		if (*bufsize < xlen)
			*bufsize += xlen;

		*dstbuf = (char*)realloc(*dstbuf, *bufsize);
		*_dst = *dstbuf + offset;
		dst = *_dst;
	}

	if (is_list(n))
		dst += sprint2_list(dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_compound(n))
		dst += sprint2_compound(dstbuf, bufsize, &dst, pl, q, n, listing);
	else if (is_ptr(n) && listing)
	{
		*dst++ = '@';
		dst += sprint_int(dst, *bufsize-(dst-*dstbuf), n->val_i, 10);
	}
	else if (is_integer(n) && (n->flags & FLAG_BINARY) && listing)
		dst += sprint_int(dst, *bufsize-(dst-*dstbuf), n->val_u, 2);
	else if (is_integer(n) && (n->flags & FLAG_OCTAL) && listing)
		dst += sprint_int(dst, *bufsize-(dst-*dstbuf), n->val_u, 8);
	else if (is_integer(n) && (n->flags & FLAG_HEX) && listing)
		dst += sprint_int(dst, *bufsize-(dst-*dstbuf), n->val_u, 16);
	else if (is_integer(n))
		dst += sprint_int(dst, *bufsize-(dst-*dstbuf), n->val_i, 10);
	else if (listing && is_float(n) && (n->flags & FLAG_PI))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", "pi");
	else if (is_float(n))
	{
		const char *save_dst = dst;
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%.*g", DBL_DIG, (double)n->val_f);
		if (!strchr(save_dst, '.')) dst += snprintf(dst, *bufsize-(dst-*dstbuf), ".0");
	}
	else if (is_var(n) && !listing)
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "_%d", n->slot);
	else if (listing && is_var(n) && (n->flags & FLAG_ANON))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "_");
	else if (is_var(n))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", n->val_s);
#ifndef ISO_ONLY
	else if (listing && is_blob(n))
	{
		char *tmpbuf = (char*)malloc((LEN(n)*3)+1);
		b64_encode(n->val_s, n->val_len, &tmpbuf, 0, 0);
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "`%s`", tmpbuf);
		free(tmpbuf);
	}
	else if (is_blob(n))
	{
		size_t len = (n->val_len<(*bufsize-(dst-*dstbuf))?n->val_len:(*bufsize-(dst-*dstbuf)));
		memcpy(dst, n->val_s, len);
		dst += len;
	}
#endif
	else if (listing && pl->flag_character_escapes && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE))
	{
		char *tmp = (char*)malloc((strlen(n->val_s)*2)+1);
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "\"%s\"", deescape(tmp, n->val_s, '"'));
		free(tmp);
	}
	else if (listing && pl->flag_character_escapes && is_atom(n) && (n->flags & FLAG_QUOTED))
	{
		char *tmp = (char*)malloc((strlen(n->val_s)*2)+1);
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "'%s'", deescape(tmp, n->val_s, '\''));
		free(tmp);
	}
	else if (listing && is_atom(n) && (n->flags & FLAG_DOUBLE_QUOTE) && needs_quoting(n->val_s))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "\"%s\"", n->val_s);
	else if (listing && is_atom(n) && (n->flags & FLAG_QUOTED) && needs_quoting(n->val_s))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "'%s'", n->val_s);
	else if (listing && is_atom(n) && !(n->flags & FLAG_QUOTED) && strchr(n->val_s, ARITY_CHAR))
	{
		const char *src = n->val_s;
		char tmpbuf[KEY_SIZE];
		strcpy(tmpbuf, src);
		char *end = strrchr(tmpbuf, ARITY_CHAR);
		if (end) *end = '\0';
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", tmpbuf);
	}
	else if (is_atom(n))
		dst += snprintf(dst, *bufsize-(dst-*dstbuf), "%s", n->val_s);

	if (q) q->print_depth--;
	if (q) q->latest_context = this_context;
	return dst - *_dst;
}

size_t sprint_term(char *_dstbuf, size_t _size, trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *dstbuf = (char*)malloc(size+1);
	char *dst = dstbuf;
	dst += sprint2_term(&dstbuf, &size, &dst, pl, q, n, listing);
	strcpy(_dstbuf, dstbuf);
	return dst - dstbuf;
}

void print_term(trealla *pl, tpl_query *q, node *n, int listing)
{
	size_t size = PRINTBUF_SIZE;
	char *dstbuf = (char*)malloc(size+1);
	char *dst = dstbuf;
	dst += sprint2_term(&dstbuf, &size, &dst, pl, q, n, listing);
	fwrite(dstbuf, 1, dst-dstbuf, stdout);
	free(dstbuf);
}

