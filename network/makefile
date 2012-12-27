
CFLAGS := -g -std=gnu99 -Wall -Werror
LIBS := -g -lc -lpthread -lm -lfftw3

NONMAINOBJS := memory_wrappers.o common.o uds_server.o uds_client.o protocol.o projectreader.o
HEADERFILES := $(NONMAINOBJS:.o=.h)

EXECUTABLES := replay.x testreader.x tracker.x protocol_test.x test_endianness.x normalisation.x uds_parrot.x fourier.x

all: $(EXECUTABLES)

# make executables
%.x: %.o $(NONMAINOBJS)
	gcc $^ $(LIBS) -o $*.x

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

