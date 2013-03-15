
# Compiler options
CFLAGS :=
# C optimization options
CFLAGS += -mfpmath=387 -malign-double -march=native -mtune=native -ffast-math
CFLAGS += -O3 -fwhole-program -flto
CFLAGS += --std=c99 # Specify standard: iso9899:199x
CFLAGS += -g # produce debugging information
CFLAGS += -Wall # Turn on all optional warnings which are desirable for normal code
CFLAGS += -Werror # Make all warnings into hard errors

LIBS := -lfftw3 -lm -lowfat

all: fourier.x pendulum.x

%.o: %.c
	@echo $(CFLAGS)
	gcc $(CFLAGS) $(OPTS) -c -o $*.o $*.c

%.x: %.o
	gcc -o $*.x $*.o $(LIBS)

clean:
	rm -f *.x *.o

