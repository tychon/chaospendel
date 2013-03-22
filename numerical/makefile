
# Compiler options
CFLAGS :=
CFLAGS += --std=c99 # Specify standard: iso9899:199x
#CFLAGS += -g # produce debugging information
CFLAGS += -Wall # Turn on all optional warnings which are desirable for normal code
CFLAGS += -Werror # Make all warnings into hard errors
CFLAGS += -I../common/ # include files can be in ../common/
# Defines
CFLAGS += -D_BSD_SOURCE

# Options appended on linking
LIBS := ../common/libcommon.a -lfftw3 -lm -lowfat -lX11 -lXext -lOpenCL

# Options for compiling and linking
BOTH := -mfpmath=387 -malign-double -march=native -mtune=native -O3 -fwhole-program -flto -ffast-math

all: libcommon ncsvtopgm.x fourier.x pendulum.x genpmovie.x lighttrail.x fast_pendulum.x loopgraph.x binmerge.x incredible_pendulum.x

libcommon:
	make -C ../common/

%.o: %.c
	gcc $(CFLAGS) $(BOTH) $(OPTS) -c -o $*.o $*.c

%.x: %.o
	gcc $(BOTH) -o $*.x $*.o $(LIBS)

fast_pendulum.x: pendulum.c
	gcc -DLOOPINGS $(CFLAGS) $(BOTH) $(OPTS) -o fast_pendulum.x pendulum.c $(LIBS)

clean:
	rm -rf feigenbaum-data phases-data
	rm -f *.x *.o *.csv *.ppm *.pgm
