GCC=gcc -std=c99 -Wall -Werror

all: replay testreader

replay: replay.c
	$(GCC) -lc replay.c -o replay

testreader: testreader.c
	$(GCC) -lc testreader.c -o testreader
