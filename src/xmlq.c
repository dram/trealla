#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "xmlq.h"

#define MAX_NAME_LEN 1024

const char *xmlq(const char *s, int idx, const char *name, char *dstbuf, int dstlen)
{
	const char *save_s = s;

	if (!s || !name || !dstbuf || !dstlen)
		return NULL;

	*dstbuf = 0;

	if (strlen(name) >= MAX_NAME_LEN)
		return NULL;

	char tmpbuf1[MAX_NAME_LEN+10], tmpbuf2[MAX_NAME_LEN+10], tmpbuf3[MAX_NAME_LEN+10];
	char *dst = dstbuf;
	int i = 0;

	while (isspace(*s))
		s++;

	if (*s != '<')
		return NULL;

	snprintf(tmpbuf1, sizeof(tmpbuf1), "<%s>", name);
	snprintf(tmpbuf2, sizeof(tmpbuf2), "<%s ", name);
	snprintf(tmpbuf3, sizeof(tmpbuf3), "</%s>", name);

	const char *ptr = NULL, *ptr3;

	while (i++ <= idx) {
		ptr = strstr(s, tmpbuf1);

		if (!ptr) {
			ptr = strstr(s, tmpbuf2);
			if (!ptr)
				return NULL;

			while (*ptr != '>')
				ptr++;

			ptr++;
		}
		else
			ptr += strlen(tmpbuf1);

		s = ptr + 1;
	}

	ptr3 = strstr(ptr, tmpbuf3);
	if (!ptr3)
		ptr3 = save_s + strlen(save_s);

	while (isspace(*ptr))
		ptr++;

	while (ptr != ptr3)
		*dst++ = *ptr++;

	while ((dst != dstbuf) && isspace(dst[-1]))
		dst--;

	*dst = 0;
	return dstbuf;
}
