
CFLAGS := -g -std=gnu99 -Wall -Werror
LIBS := -g -lc -lpthread -lm -lfftw3 -lX11 -lXext

NONMAINOBJS := memory_wrappers.o common.o uds_server.o uds_client.o protocol.o projectreader.o gui/x11draw.o
HEADERFILES := $(NONMAINOBJS:.o=.h)

EXECUTABLES := testreader.x replay.x normalisation.x tracker.x fourier.x gui/x11drawtest.x

all: $(EXECUTABLES)

# make executables
%.x: %.o $(NONMAINOBJS)
	gcc $^ $(LIBS) -o $*.x

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

