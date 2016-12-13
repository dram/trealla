#ifndef UTF8_H
#define UTF8_H

// These relate to similar stdc functions...

extern int getc_utf8(FILE *fp);
extern size_t strlen_utf8(const char *s);
extern int isalpha_utf8(int ch);

// These just get/put a memory buffer...

extern int get_char_utf8(const char **src);
extern int put_char_utf8(char *dst, int ch);

#endif