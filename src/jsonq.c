#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "jsonq.h"

static const char *jsonq_internal(const char *s, const char *name, char *dstbuf, int dstlen, int idx, char *nambuf, int namlen)
{
	static const char *escapes = "\a\f\b\t\v\r\n";
	static const char *anti_escapes = "afbtvrn";

	if (!s || !dstbuf || !dstlen)
		return dstbuf;

	*dstbuf = 0;
	const char *src = s;
	char tmpbuf[1024]; // only used for name
	char *dst = tmpbuf;
	int found = 0, quoted = 0, level = 0, lhs = 1, i = 0;
	char ch;

	while (isspace(*s))
		s++;

	if (*s++ != '{')
		return dstbuf;

	while ((ch = *s++) != 0) {
		if (!quoted && isspace(ch))
			;
		else if (!quoted && (ch == '"'))
			quoted = 1;
		else if (quoted && (ch == '"'))
			quoted = 0;
		else if (quoted && lhs && (ch == '\\')) {
			const char *ptr = strchr(anti_escapes, ch = *src++);
			if (ptr)
				*dst++ = escapes[ptr - anti_escapes];
			else
				*dst++ = ch;
		}
		else if (!quoted && lhs && !found && !level && (ch == ':')) {
			*dst = 0;
			dst = tmpbuf;

			while (isspace(*s))
				s++;

			if (name) {
				if ((found = !strcmp(name, tmpbuf)) != 0)
					src = s;
			}
			else if (idx == i++) {
				found = 1;
				strcpy(nambuf, tmpbuf);
				src = s;
			}

			lhs = 0;
		}
		else if (found && !level && ((ch == ',') || (ch == '}'))) {
			int len = (s - src) - 1;

			if (*src == '"') {
				dst = dstbuf;
				src++;
				len -= 2;

				while (len-- > 0) {
					ch = *src++;

					if (ch == '\\') {

						len--;
						const char *ptr = strchr(anti_escapes, ch = *src++);
						if (ptr)
							*dst++ = escapes[ptr - anti_escapes];
						else
							*dst++ = ch;
					}
					else
						*dst++ = ch;
				}

				*dst = 0;
			}
			else
				strncpy(dstbuf, src, len < dstlen ? len : dstlen);

			dstbuf[len] = 0;
			return dstbuf;
		}
		else if (!level && (ch == ','))
			lhs = 1;
		else if ((ch == '{') || (ch == '['))
			level++;
		else if ((ch == '}') || (ch == ']'))
			level--;
		else if (lhs)
			*dst++ = ch;
	}

	*dst = 0;
	return found ? dstbuf : NULL;
}

const char *jsonq(const char *s, const char *name, char *dstbuf, int dstlen)
{
	return jsonq_internal(s, name, dstbuf, dstlen, -1, NULL, 0);
}

long long jsonq_int(const char *s, const char *name)
{
	char tmpbuf[256];
	long long v = 0;
	jsonq(s, name, tmpbuf, sizeof(tmpbuf));
	sscanf(tmpbuf, "%lld", &v);
	return v;
}

double jsonq_real(const char *s, const char *name)
{
	char tmpbuf[256];
	double v = 0.0;
	jsonq(s, name, tmpbuf, sizeof(tmpbuf));
	sscanf(tmpbuf, "%lg", &v);
	return v;
}

int jsonq_bool(const char *s, const char *name)
{
	char tmpbuf[256];
	jsonq(s, name, tmpbuf, sizeof(tmpbuf));
	return !strcmp(tmpbuf, "true");
}

int jsonq_null(const char *s, const char *name)
{
	char tmpbuf[256];
	jsonq(s, name, tmpbuf, sizeof(tmpbuf));
	if (!tmpbuf[0])
		return 1;
	return !strcmp(tmpbuf, "null");
}

const char *jsonqi(const char *s, int idx, char *nambuf, int namlen, char *dstbuf, int dstlen)
{
	return jsonq_internal(s, NULL, dstbuf, dstlen, idx, nambuf, namlen);
}
