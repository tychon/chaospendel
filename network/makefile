
CFLAGS = -std=gnu99 -Wall -Werror
LIBS = -lc -lpthread

HEADERFILES = uds_server.h uds_client.h memory_wrappers.h
NONMAINOBJS = uds_server.o uds_client.o memory_wrappers.o

all: replay testreader tracker

compile: $(NONMAINOBJS)

%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

replay: $(NONMAINOBJS) replay.o
	gcc $(NONMAINOBJS) replay.o $(LIBS) -o replay

tracker: $(NONMAINOBJS) tracker.o
	gcc $(NONMAINOBJS) tracker.o $(LIBS) -o tracker

testreader: $(NONMAINOBJS) testreader.o
	gcc $(NONMAINOBJS) testreader.o $(LIBS) -o testreader

