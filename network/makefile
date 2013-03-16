
CFLAGS := -g -std=gnu99 -Wall -Werror -I../common/
LIBS := -g -lc -lpthread -lm -lfftw3 -lX11 -lXext -lev -lrt

NONMAINOBJS := integral.o markov_chain.o
NONMAINOBJS += uds_server.o uds_client.o protocol.o projectreader.o
HEADERFILES := $(NONMAINOBJS:.o=.h) ../numerical/pendulum.h

EXECUTABLES := testreader.x x11drawtest.x
EXECUTABLES += reader.x replay.x
EXECUTABLES += rangetester.x normalisation.x tracker.x
EXECUTABLES += markov_prediction.x
EXECUTABLES += markovchaintest.x
EXECUTABLES += measure_rtt.x

.PHONY: all clean ../common/libcommon.a

all: $(EXECUTABLES)

../common/libcommon.a:
	make -C../common/

# make executables
%.x: %.o $(NONMAINOBJS) ../common/libcommon.a
	gcc $^ $(LIBS) -o $*.x

# make object files
%.o: %.c $(HEADERFILES)
	gcc $(CFLAGS) -c $*.c -o $*.o

clean:
	rm -f *.x *.o
