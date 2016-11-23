#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

#include "trealla.h"
#include "internal.h"
#include "bifs.h"

extern uint8_t _binary_modules_auth_pro_start[];
extern uint8_t _binary_modules_auth_pro_size;
extern uint8_t _binary_modules_blog_pro_start[];
extern uint8_t _binary_modules_blog_pro_size;
extern uint8_t _binary_modules_dict_pro_start[];
extern uint8_t _binary_modules_dict_pro_size;
extern uint8_t _binary_modules_http_client_pro_start[];
extern uint8_t _binary_modules_http_client_pro_size;
extern uint8_t _binary_modules_smtp_client_pro_start[];
extern uint8_t _binary_modules_smtp_client_pro_size;

struct library libs[] =
{
  { "auth", 		_binary_modules_auth_pro_start,			&_binary_modules_auth_pro_size },
  { "blog", 		_binary_modules_blog_pro_start,			&_binary_modules_blog_pro_size },
  { "dict",			_binary_modules_dict_pro_start,			&_binary_modules_dict_pro_size },
  { "smtp_client",	_binary_modules_smtp_client_pro_start,	&_binary_modules_smtp_client_pro_size },
  { "http_client",	_binary_modules_http_client_pro_start,	&_binary_modules_http_client_pro_size },
  {0}
};

