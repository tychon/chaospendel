
# Compiler options
CFLAGS :=
CFLAGS += --std=c99 # Specify standard: iso9899:199x
CFLAGS += -g # produce debugging information
CFLAGS += -Wall # Turn on all optional warnings which are desirable for normal code
CFLAGS += -Werror # Make all warnings into hard errors
CFLAGS += -I../common/ # include files can be in ../common/
# Defines
CFLAGS += -D_BSD_SOURCE

# Options appended on linking
LIBS := ../common/libcommon.a -lfftw3 -lm -lowfat -lX11 -lXext

# Options for compiling and linking
BOTH := -mfpmath=387 -malign-double -march=native -mtune=native -O3 -fwhole-program -flto -ffast-math

.PHONY: all clean ../common/libcommon.a

all: fourier.x pendulum.x genpmovie.x lighttrail.x

../common/libcommon.a:
	make -C ../common/

%.o: %.c
	gcc $(CFLAGS) $(BOTH) $(OPTS) -c -o $*.o $*.c

%.x: %.o ../common/libcommon.a
	gcc $(BOTH) -o $*.x $*.o $(LIBS)

clean:
	rm -f *.x *.o
