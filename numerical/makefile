
# Compiler options
CFLAGS :=
CFLAGS += --std=c99 # Specify standard: iso9899:199x
CFLAGS += -g # produce debugging information
CFLAGS += -Wall # Turn on all optional warnings which are desirable for normal code
CFLAGS += -Werror # Make all warnings into hard errors
# Defines
CFLAGS += -D_BSD_SOURCE

# Options appenden on linking
LIBS := -lfftw3 -lm -lowfat

# Options for compiling and linking
BOTH := -mfpmath=387 -malign-double -march=native -mtune=native -O3 -fwhole-program -flto -ffast-math

all: fourier.x pendulum.x genpmovie.x

%.o: %.c
	gcc $(CFLAGS) $(BOTH) $(OPTS) -c -o $*.o $*.c

%.x: %.o
	gcc $(BOTH) -o $*.x $*.o $(LIBS)

clean:
	rm -f *.x *.o

