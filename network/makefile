
CFLAGS := -g -std=gnu99 -Wall
LIBS := -g -lc -lpthread -lm -lfftw3 -lX11 -lXext

NONMAINOBJS := memory_wrappers.o ringbuffer.o integral.o x11draw.o
NONMAINOBJS += common.o uds_server.o uds_client.o protocol.o projectreader.o
HEADERFILES := $(NONMAINOBJS:.o=.h)

EXECUTABLES := testreader.x replay.x normalisation.x tracker.x fourier.x x11drawtest.x reader.x buffertest.x

.PHONY: all clean

all: $(EXECUTABLES)

# make executables
%.x: %.o $(NONMAINOBJS)
	gcc $^ $(LIBS) -o $*.x

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

clean:
	rm -f *.x *.o
