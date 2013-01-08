
CFLAGS := -g -std=gnu99 -Wall -Werror
LIBS := -g -lc -lpthread -lm -lfftw3 -lX11 -lXext

NONMAINOBJS := memory_wrappers.o integral.o x11draw.o
NONMAINOBJS += common.o uds_server.o uds_client.o protocol.o projectreader.o
HEADERFILES := $(NONMAINOBJS:.o=.h)

EXECUTABLES := testreader.x x11drawtest.x
EXECUTABLES += reader.x replay.x
EXECUTABLES += rangetester.x normalisation.x tracker.x

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
