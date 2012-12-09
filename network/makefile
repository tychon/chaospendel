
CFLAGS = -std=gnu99 -Wall -Werror
LIBS = -lc -lpthread

HEADERFILES = uds_server.h memory_wrappers.h
NONMAINOBJS = uds_server.o memory_wrappers.o

all: replay testreader

compile: $(NONMAINOBJS)

%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

replay: $(NONMAINOBJS) replay.o
	gcc $(NONMAINOBJS) replay.o $(LIBS) -o replay

testreader: $(NONMAINOBJS) testreader.o
	gcc $(NONMAINOBJS) testreader.o $(LIBS) -o testreader

