
CFLAGS := -g -std=gnu99 -Wall -Werror
LIBS := -g -lc -lpthread -lm

NONMAINOBJS := uds_server.o uds_client.o protocol.o memory_wrappers.o projectreader.o
HEADERFILES := $(NONMAINOBJS:.o=.h) common.h

EXECUTABLES := replay.x testreader.x tracker.x protocol_test.x test_endianness.x normalisation.x uds_parrot.x

all: $(EXECUTABLES)

# make executables
%.x: %.o $(NONMAINOBJS)
	gcc $^ $(LIBS) -o $*.x

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

