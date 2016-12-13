#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

size_t strlen_utf8(const char *s)
{
	size_t cnt = 0;

	while (*s)
	{
		unsigned char ch = *(unsigned char*)s++;

		if ((ch < 0x80) || (ch > 0xBF))
			cnt++;
	}

	return cnt;
}

int isalpha_utf8(int _ch)
{
	unsigned ch = (unsigned)_ch;
	return isalpha(ch) || (ch > 0x7F);
}

int is_char_utf8(const char *src)
{
	unsigned ch = *(const unsigned char*)src;
	return (ch >= 0x80) && (ch <= 0xBF);
}

int put_char_utf8(char *_dst, int _ch)
{
	unsigned ch = (unsigned)_ch;
	unsigned char *dst = (unsigned char*)_dst;
	int len = 0;

	if (ch <= 0x7F)
	{
		*dst++ = ch;
		len = 1;
	}
	else if (ch <= 0x07FF)
	{
		*dst = 0b11000000;
		*dst++ |= (ch>>6) & 0b00011111;
		*dst = 0b10000000;
		*dst++ |= (ch & 0b00111111);
		len = 2;
	}
	else if (ch <= 0xFFFF)
	{
		*dst = 0b11100000;
		*dst++ |= (ch>>12) & 0b00001111;
		*dst = 0b10000000;
		*dst++ |= ((ch>>6) & 0b00111111);
		*dst = 0b10000000;
		*dst++ |= (ch & 0b00111111);
		len = 3;
	}

	*dst = '\0';
	return len;
}

int get_char_utf8(const char **_src)
{
	const unsigned char *src = (const unsigned char*)*_src;
	int n = 0, len = 0, expect = 1;

	while (*src && expect--)
	{
		unsigned char ch = *src++;
		len++;

		if ((ch & 0b11110000) == 0b11110000)
		{
			n <<= 3;
			n |= ch & 0b00000111;
			expect = 3;
		}
		else if ((ch & 0b11100000) == 0b11100000)
		{
			n <<= 4;
			n |= ch & 0b00001111;
			expect = 2;
		}
		else if ((ch & 0b11000000) == 0b11000000)
		{
			n <<= 5;
			n |= ch & 0b00011111;
			expect = 1;
		}
		else if ((ch & 0b10000000) == 0b10000000)
		{
			n <<= 6;
			n |= ch & 0b00111111;
		}
		else
		{
			n <<= 8;
			n |= ch;
			break;
		}
	}

	*_src = (const char*)src;
	return n;
}

int getc_utf8(FILE *fp)
{
	int n = 0, len = 0, expect = 1;

	while (!feof(fp) && expect--)
	{
		int _ch = getc(fp);

		if (_ch == EOF)
			return EOF;

		unsigned char ch = (unsigned char)_ch;
		len++;

		if ((ch & 0b11110000) == 0b11110000)
		{
			n <<= 3;
			n |= ch & 0b00000111;
			expect = 3;
		}
		else if ((ch & 0b11100000) == 0b11100000)
		{
			n <<= 4;
			n |= ch & 0b00001111;
			expect = 2;
		}
		else if ((ch & 0b11000000) == 0b11000000)
		{
			n <<= 5;
			n |= ch & 0b00011111;
			expect = 1;
		}
		else if ((ch & 0b10000000) == 0b10000000)
		{
			n <<= 6;
			n |= ch & 0b00111111;
		}
		else
		{
			n <<= 8;
			n |= ch;
			break;
		}
	}

	return n;
}

