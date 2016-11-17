#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "xmlq.h"

const char *xmlq(const char *s, int idx, const char *name, char *dstbuf, int dstlen)
{
	const char *save_s = s;

	if (!s || !name || !dstbuf || !dstlen)
		return NULL;

	*dstbuf = 0;

	if (strlen(name) >= 1024)
		return NULL;

	char tmpbuf1[1024], tmpbuf2[1024], tmpbuf3[1024];
	char *dst = dstbuf;
	int i = 0;

	while (isspace(*s))
		s++;

	if (*s != '<')
		return NULL;

	sprintf(tmpbuf1, "<%s>", name);
	sprintf(tmpbuf2, "<%s ", name);
	sprintf(tmpbuf3, "</%s>", name);

	const char *ptr = NULL, *ptr3;

	while (i++ <= idx)
	{
		ptr = strstr(s, tmpbuf1);

		if (!ptr)
		{
			ptr = strstr(s, tmpbuf2);
			if (!ptr) return NULL;

			while (*ptr != '>')
				ptr++;

			ptr++;
		}
		else
			ptr += strlen(tmpbuf1);

		s = ptr+1;
	}

	ptr3 = strstr(ptr, tmpbuf3);
	if (!ptr3) ptr3 = save_s + strlen(save_s);

	while (ptr != ptr3)
		*dst++ = *ptr++;

	*dst = 0;
	return dstbuf;
}
