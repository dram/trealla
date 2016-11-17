#ifndef XMLQ_H
#define XMLQ_H

// XML quick and dirty search for named value.
//
// char *s = "<xml><entry>123</entry><entry>abc</entry></xml>";
//
// const char *src;
// char tmp[256];
// int idx = 0;
//
// while ((s = xmlq(s,idx++,"entry",tmp,sizeof(tmp))) != NULL
// {
// 	...
// }
//
// Doesn't use any memory other than what's on the stack.
//
// TO DO: need to be able to iterate objects and arrays
// and enumerate/index arrays directly.

extern const char *xmlq(const char *s, int idx, const char *name, char *dstbuf, int dstlen);

#endif
