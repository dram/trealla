#ifndef JSONQ_H
#define JSONQ_H

// JSON quick and dirty search for named value.
//
// Calls can be concatenated, eg:
//
// char *s = "{'a':1,'b':2,'c':{'c1':31,'c2':32,'c3':33},'d',4}";
//
// char tmp[256];
// long long v = jsonq(s,"c",tmp,sizeof(tmp))->jsonq_int(tmp,"c2");
//
// Doesn't use any memory other than what's on the stack.
//
// TO DO: need to be able to iterate objects and arrays
// and enumerate/index arrays directly.

extern const char *jsonq(const char *s, const char *name, char *dstbuf, int dstlen);
extern long long jsonq_int(const char *s, const char *name);
extern double jsonq_real(const char *s, const char *name);
extern int jsonq_bool(const char *s, const char *name);
extern int jsonq_null(const char *s, const char *name);

extern const char *jsonqi(const char *s, int idx, char *nambuf, int namlen, char *dstbuf, int dstlen);

#endif
