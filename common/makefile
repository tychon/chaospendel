
CFLAGS := -g -std=gnu99 -Wall -Werror

OBJS := common.o memory_wrappers.o x11draw.o

libcommon.a: $(OBJS)
	ar rcs libcommon.a $(OBJS)

clean:
	rm -f *.o

