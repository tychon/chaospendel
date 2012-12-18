
CFLAGS = -std=gnu99 -Wall -Werror
LIBS = -lc -lpthread

NONMAINOBJS = uds_server.o uds_client.o protocol.o memory_wrappers.o
HEADERFILES = $(NONMAINOBJS:.o=.h) common.h

MAINOBJS = replay testreader tracker protocol_test test_endianness

all: $(MAINOBJS)

# make executables
%: %.o $(NONMAINOBJS)
	gcc $^ $(LIBS) -o $*

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

