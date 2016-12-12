#ifndef UTF8_H
#define UTF8_H

extern size_t strlen_utf8(const char *s);
extern int get_char_utf8(const char **src);
extern int put_char_utf8(char *dst, int ch);
extern int isalpha_utf8(int ch);

extern int getc_utf8(FILE *fp);

#endif
