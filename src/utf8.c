#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef unix
#include <unistd.h>
#endif

size_t strlen_utf8(const char *s)
{
	size_t cnt = 0;

	while (*s) {
		unsigned char ch = *(unsigned char *)s++;

		if ((ch < 0x80) || (ch > 0xBF))
			cnt++;
	}

	return cnt;
}

size_t strcount_utf8(const char *s)
{
	size_t cnt = 0;

	while (*s) {
		unsigned char ch = *(unsigned char *)s++;

		if (ch <= 0xBF)
			cnt++;
	}

	return cnt;
}

int isalpha_utf8(int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	return isalpha(_ch) || (ch > 0x7F);
}

int isalnum_utf8(int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	return isdigit(_ch) || isalpha(_ch) || (ch > 0x7F);
}

int is_char_utf8(const char *src)
{
	unsigned int ch = *(const unsigned char *)src;
	return (ch >= 0x80) && (ch <= 0xBF);
}

int put_len_utf8(int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	int len = 0;

	if (ch <= 0x7F)
		len = 1;
	else if (ch <= 0x07FF)
		len = 2;
	else if (ch <= 0xFFFF)
		len = 3;
	else if (ch <= 0x01FFFFF)
		len = 4;
	else if (ch <= 0x03FFFFFF)
		len = 5;
	else if (ch <= 0x7FFFFFFF)
		len = 6;

	return len;
}

int put_char_bare_utf8(char *_dst, int _ch)
{
	unsigned int ch = (unsigned int)_ch;
	unsigned char *dst = (unsigned char *)_dst;
	int len = 0;

	if (ch <= 0x7F) {
		*dst++ = ch;
		len = 1;
	}
	else if (ch <= 0x07FF) {
		*dst = 0b11000000;
		*dst++ |= (ch >> 6) & 0b00011111;
		*dst = 0b10000000;
		*dst++ |= (ch & 0b00111111);
		len = 2;
	}
	else if (ch <= 0xFFFF) {
		*dst = 0b11100000;
		*dst++ |= (ch >> 12) & 0b00001111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 6) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= ch & 0b00111111;
		len = 3;
	}
	else if (ch <= 0x01FFFFF) {
		*dst = 0b11100000;
		*dst++ |= (ch >> 18) & 0b00000111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 12) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= (ch >> 6) & 0b00111111;
		*dst = 0b10000000;
		*dst++ |= ch & 0b00111111;
		len = 4;
	}

	return len;
}

int put_char_utf8(char *dst, int ch)
{
	int len = put_char_bare_utf8(dst, ch);
	dst[len] = '\0';
	return len;
}

int get_char_utf8(const char **_src)
{
	const unsigned char *src = (const unsigned char *)*_src;
	int expect = 1;
	unsigned int n = 0;

	while (*src && expect--) {
		unsigned char ch = *src++;

		if ((ch & 0b11111100) == 0b11111100) {
			n = ch & 0b00000001;
			expect = 5;
		}
		else if ((ch & 0b11111000) == 0b11111000) {
			n = ch & 0b00000011;
			expect = 4;
		}
		else if ((ch & 0b11110000) == 0b11110000) {
			n = ch & 0b00000111;
			expect = 3;
		}
		else if ((ch & 0b11100000) == 0b11100000) {
			n = ch & 0b00001111;
			expect = 2;
		}
		else if ((ch & 0b11000000) == 0b11000000) {
			n = ch & 0b00011111;
			expect = 1;
		}
		else if ((ch & 0b10000000) == 0b10000000) {
			n <<= 6;
			n |= ch & 0b00111111;
		}
		else {
			n = ch;
		}
	}

	*_src = (const char *)src;
	return (int)n;
}

int readc_utf8(int fd, int *res)
{
	unsigned int n = 0;
	int expect = 1;

	while (expect--) {
		unsigned char ch;
		int len;

		if ((len = read(fd, &ch, 1)) == -1) {
			*res = EOF;
			return 1;
		}

		if (!len)
			return len;

		if ((ch & 0b11111100) == 0b11111100) {
			n = ch & 0b00000001;
			expect = 5;
		}
		else if ((ch & 0b11111000) == 0b11111000) {
			n = ch & 0b00000011;
			expect = 4;
		}
		else if ((ch & 0b11110000) == 0b11110000) {
			n = ch & 0b00000111;
			expect = 3;
		}
		else if ((ch & 0b11100000) == 0b11100000) {
			n = ch & 0b00001111;
			expect = 2;
		}
		else if ((ch & 0b11000000) == 0b11000000) {
			n = ch & 0b00011111;
			expect = 1;
		}
		else if ((ch & 0b10000000) == 0b10000000) {
			n <<= 6;
			n |= ch & 0b00111111;
		}
		else {
			n = ch;
		}
	}

	*res = (int)n;
	return 1;
}

int getc_utf8(FILE *fp)
{
	unsigned int n = 0;
	int expect = 1;

	while (expect--) {
		int _ch = fgetc(fp);

		if (_ch == EOF)
			return EOF;

		unsigned char ch = (unsigned char)_ch;

		if ((ch & 0b11111100) == 0b11111100) {
			n = ch & 0b00000001;
			expect = 5;
		}
		else if ((ch & 0b11111000) == 0b11111000) {
			n = ch & 0b00000011;
			expect = 4;
		}
		else if ((ch & 0b11110000) == 0b11110000) {
			n = ch & 0b00000111;
			expect = 3;
		}
		else if ((ch & 0b11100000) == 0b11100000) {
			n = ch & 0b00001111;
			expect = 2;
		}
		else if ((ch & 0b11000000) == 0b11000000) {
			n = ch & 0b00011111;
			expect = 1;
		}
		else if ((ch & 0b10000000) == 0b10000000) {
			n <<= 6;
			n |= ch & 0b00111111;
		}
		else {
			n = ch;
		}
	}

	return (int)n;
}
