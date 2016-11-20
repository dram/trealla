CFLAGS = -Isrc -O3 $(OPT) -Wall -D_DEFAULT_SOURCE -D_BSD_SOURCE -Wwrite-strings
LDFLAGS = -lm

OBJECTS_ISO = src/trealla.o src/bifs_iso.o src/history.o src/jela.o \
			src/list.o src/print.o src/skiplist.o src/daemon.o

OBJECTS_ALL = $(OBJECTS_ISO) src/base64.o src/bifs_sys.o src/bifs_http.o \
			src/jsonq.o src/bifs_kvs.o src/bifs_net.o src/bifs_proc.o \
			src/network.o src/skipbuck.o src/thread.o src/uncle.o \
			src/uuid.o src/xmlq.o src/bifs_dbs.o

ifndef USE_SSL
USE_SSL = 1
LDFLAGS +=  -lssl -lcrypto
else
USE_SSL = 0
endif

CFLAGS += -DUSE_SSL=$(USE_SSL)

ifdef ISO_ONLY
OBJECTS = $(OBJECTS_ISO)
CFLAGS += -DISO_ONLY
else
OBJECTS = $(OBJECTS_ALL)
CFLAGS += -pthread
LDFLAGS +=  -pthread
endif

ifdef DONT_WANT_128
CFLAGS += -DDONT_WANT_128
endif

all: tpl

debug:
	make 'OPT=-O0 -g'

profile:
	make 'OPT=-O1 -pg'

small:
	make DONT_WANT_128=1

iso:
	make ISO_ONLY=1 USE_SSL=0

iso_small:
	make ISO_ONLY=1 DONT_WANT_128=1 USE_SSL=0

iso_debug:
	make 'OPT=-O0 -g' ISO_ONLY=1 USE_SSL=0

iso_profile:
	make 'OPT=-O1 -pg' ISO_ONLY=1 USE_SSL=0

tpl: $(OBJECTS) tpl.o
	$(CC) -o tpl tpl.o $(OBJECTS) $(OPT) $(LDFLAGS)

src/http_client.h: modules/http_client.pro
	xxd -i modules/http_client.pro >src/http_client.h

src/smtp_client.h: modules/smtp_client.pro
	xxd -i modules/smtp_client.pro >src/smtp_client.h

src/dict.h: modules/dict.pro
	xxd -i modules/dict.pro >src/dict.h

src/auth.h: modules/auth.pro
	xxd -i modules/auth.pro >src/auth.h

src/blog.h: modules/blog.pro
	xxd -i modules/blog.pro >src/blog.h

clean:
	rm -f src/*.o gmon.* *.o tpl

tpl.o: src/trealla.h src/daemon.h src/history.h
tpl.o: src/http_client.h src/dict.h src/smtp_client.h
tpl.o: src/auth.h src/blog.h

# DO NOT DELETE

src/base64.o: src/base64.h
src/bifs_iso.o: src/bifs.h src/history.h src/internal.h src/jela.h src/trealla.h
src/bifs_sys.o: src/base64.h src/bifs.h src/internal.h src/jela.h src/network.h src/xmlq.h src/trealla.h
src/bifs_http.o: src/base64.h src/bifs.h src/internal.h src/network.h src/jela.h src/trealla.h
src/bifs_kvs.o: src/bifs.h src/trealla.h src/internal.h src/skiplist.h src/jela.h
src/bifs_dbs.o: src/bifs.h src/trealla.h src/internal.h src/skiplist.h src/jela.h
src/bifs_net.o: src/bifs.h src/trealla.h src/internal.h src/skiplist.h src/jela.h src/network.h
src/daemon.o: src/daemon.h
src/history.o: src/history.h
src/jela.o: src/jela.h src/trealla.h src/internal.h src/bifs.h src/jela.h src/list.h src/skiplist.h
src/jsonq.o: src/jsonq.h
src/list.o: src/list.h
src/network.o: src/network.h src/skiplist.h src/skipbuck.h src/thread.h src/uncle.h
src/print.o: src/trealla.h src/internal.h src/bifs.h src/list.h src/base64.h
src/proc.o: src/bifs.h src/internal.h src/jela.h src/network.h src/trealla.h
src/skipbuck.o: src/skipbuck.h
src/skiplist.o: src/skiplist.h
src/thread.o: src/thread.h src/list.h
src/trealla.o: src/trealla.h src/jela.h src/internal.h src/bifs.h src/list.h src/base64.h src/skiplist.h src/jsonq.h src/xmlq.h src/uuid.h src/uncle.h src/network.h src/thread.h
src/uncle.o: src/uncle.h src/jsonq.h src/network.h src/skipbuck.h src/thread.h
src/uuid.o: src/uuid.h src/thread.h
src/xmlq.o: src/xmlq.h
