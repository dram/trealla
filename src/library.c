#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "trealla.h"

#include "internal.h"

extern uint8_t _binary_library_auth_pro_start[];
extern uint8_t _binary_library_auth_pro_end[];

extern uint8_t _binary_library_blog_pro_start[];
extern uint8_t _binary_library_blog_pro_end[];

extern uint8_t _binary_library_dict_pro_start[];
extern uint8_t _binary_library_dict_pro_end[];

extern uint8_t _binary_library_http_client_pro_start[];
extern uint8_t _binary_library_http_client_pro_end[];

extern uint8_t _binary_library_smtp_client_pro_start[];
extern uint8_t _binary_library_smtp_client_pro_end[];

extern uint8_t _binary_library_stomp_client_pro_start[];
extern uint8_t _binary_library_stomp_client_pro_end[];

extern uint8_t _binary_library_yahoo_pro_start[];
extern uint8_t _binary_library_yahoo_pro_end[];

extern uint8_t _binary_library_mime_pro_start[];
extern uint8_t _binary_library_mime_pro_end[];

library g_libs[] = {{"auth", _binary_library_auth_pro_start, _binary_library_auth_pro_end},
                    {"blog", _binary_library_blog_pro_start, _binary_library_blog_pro_end},
                    {"dict", _binary_library_dict_pro_start, _binary_library_dict_pro_end},
                    {"smtp_client", _binary_library_smtp_client_pro_start, _binary_library_smtp_client_pro_end},
                    {"http_client", _binary_library_http_client_pro_start, _binary_library_http_client_pro_end},
                    {"stomp_client", _binary_library_stomp_client_pro_start, _binary_library_stomp_client_pro_end},
                    {"yahoo", _binary_library_yahoo_pro_start, _binary_library_yahoo_pro_end},
                    {"mime", _binary_library_mime_pro_start, _binary_library_mime_pro_end},
                    {0}};
