
CFLAGS := -g -std=gnu99 -Wall -Werror

OBJS := common.o memory_wrappers.o x11draw.o

libcommon.a: $(OBJS)
	ar rcs libcommon.a $(OBJS)

%.o: %.c $(OBJS:.o=.h)
	gcc $(CFLAGS) -c -o $*.o $*.c

clean:
	rm -f *.o

