
CFLAGS = -std=gnu99 -Wall -Werror
LIBS = -lc -lpthread

NONMAINOBJS = uds_server.o uds_client.o protocol.o memory_wrappers.o
HEADERFILES = $(NONMAINOBJS:.o=.h)

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

protocol_test: $(NONMAINOBJS) protocol_test.o
	gcc $(NONMAINOBJS) protocol_test.o $(LIBS) -o protocol_test

