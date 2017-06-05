CFLAGS = -Isrc -O3 $(OPT) -Wall -D_DEFAULT_SOURCE -D_BSD_SOURCE
CFLAGS += -I/usr/local/include
LDFLAGS = -lm

OBJECTS_ISO = src/trealla.o src/parser.o src/bifs_iso.o src/jela.o \
			src/list.o src/print.o src/skiplist.o src/skipbuck.o \
			src/utf8.o src/history.o src/daemon.o

OBJECTS_ALL = $(OBJECTS_ISO) src/base64.o src/bifs_sys.o src/bifs_http.o \
			src/jsonq.o src/bifs_net.o src/bifs_proc.o \
			src/network.o src/thread.o src/uncle.o \
			src/uuid.o src/xmlq.o src/bifs_dbs.o src/library.o \
			src/auth.o src/blog.o src/dict.o \
			src/http_client.o src/smtp_client.o src/stomp_client.o \
			src/yahoo.o src/mime.o

.ifndef USE_SSL
USE_SSL = 1
LDFLAGS +=  -lssl -lcrypto
.else
USE_SSL = 0
.endif

CFLAGS += -DUSE_SSL=$(USE_SSL)

.ifdef ISO_ONLY
OBJECTS = $(OBJECTS_ISO)
CFLAGS += -DISO_ONLY
.else
OBJECTS = $(OBJECTS_ALL)
CFLAGS += -pthread
LDFLAGS += -pthread
.endif

all: tpl

debug:
	make 'OPT=-O0 -g -DDEBUG'

profile:
	make 'OPT=-O1 -pg'

iso:
	make ISO_ONLY=1 USE_SSL=0

iso_debug:
	make 'OPT=-O0 -g -DDEBUG' ISO_ONLY=1 USE_SSL=0

iso_profile:
	make 'OPT=-O1 -pg' ISO_ONLY=1 USE_SSL=0

tpl: $(OBJECTS) tpl.o
	$(CC) -o tpl tpl.o $(OBJECTS) $(OPT) $(LDFLAGS)

# Library modules

src/auth.o: library/auth.pro
	ld -r -b binary -o src/auth.o library/auth.pro

src/blog.o: library/blog.pro
	ld -r -b binary -o src/blog.o library/blog.pro

src/dict.o: library/dict.pro
	ld -r -b binary -o src/dict.o library/dict.pro

src/http_client.o: library/http_client.pro
	ld -r -b binary -o src/http_client.o library/http_client.pro

src/smtp_client.o: library/smtp_client.pro
	ld -r -b binary -o src/smtp_client.o library/smtp_client.pro

src/stomp_client.o: library/stomp_client.pro
	ld -r -b binary -o src/stomp_client.o library/stomp_client.pro

src/yahoo.o: library/yahoo.pro
	ld -r -b binary -o src/yahoo.o library/yahoo.pro

src/mime.o: library/mime.pro
	ld -r -b binary -o src/mime.o library/mime.pro

#

clean:
	rm -f src/*.o gmon.* *.o tpl core

tpl.o: src/trealla.h src/internal.h src/daemon.h src/history.h
tpl.o: src/network.h src/skiplist.h src/skipbuck.h src/list.h

# from [gcc|clang] -MM

src/base64.o: src/base64.c src/base64.h
src/bifs_dbs.o: src/bifs_dbs.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/jela.h
src/bifs_http.o: src/bifs_http.c src/trealla.h src/base64.h src/bifs.h \
 src/internal.h src/skiplist.h src/skipbuck.h src/list.h src/network.h \
 src/thread.h src/utf8.h src/jela.h
src/bifs_iso.o: src/bifs_iso.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/jela.h
src/bifs_net.o: src/bifs_net.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/jela.h src/uncle.h
src/bifs_proc.o: src/bifs_proc.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/jela.h src/uncle.h
src/bifs_sys.o: src/bifs_sys.c src/trealla.h src/base64.h src/bifs.h \
 src/internal.h src/skiplist.h src/skipbuck.h src/list.h src/network.h \
 src/thread.h src/utf8.h src/jela.h src/jsonq.h src/uuid.h src/xmlq.h
src/daemon.o: src/daemon.c src/daemon.h
src/history.o: src/history.c src/history.h src/utf8.h
src/jela.o: src/jela.c src/trealla.h src/bifs.h src/internal.h src/skiplist.h \
 src/skipbuck.h src/list.h src/network.h src/thread.h src/utf8.h \
 src/jela.h
src/jsonq.o: src/jsonq.c src/jsonq.h
src/library.o: src/library.c src/trealla.h src/internal.h src/skiplist.h \
 src/skipbuck.h src/list.h src/network.h src/thread.h
src/list.o: src/list.c src/list.h
src/network.o: src/network.c src/network.h src/thread.h src/skipbuck.h \
 src/skiplist.h src/uncle.h
src/parser.o: src/parser.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/base64.h src/jsonq.h
src/print.o: src/print.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/jela.h src/base64.h
src/skipbuck.o: src/skipbuck.c src/skipbuck.h
src/skiplist.o: src/skiplist.c src/skiplist.h
src/thread.o: src/thread.c src/list.h src/thread.h
src/trealla.o: src/trealla.c src/trealla.h src/bifs.h src/internal.h \
 src/skiplist.h src/skipbuck.h src/list.h src/network.h src/thread.h \
 src/utf8.h src/history.h src/jela.h src/uuid.h
src/uncle.o: src/uncle.c src/jsonq.h src/network.h src/thread.h \
 src/skipbuck.h src/uncle.h
src/utf8.o: src/utf8.c
src/uuid.o: src/uuid.c src/uuid.h
src/xmlq.o: src/xmlq.c src/xmlq.h
