
#include <stdio.h>

#define LITTLE_ENDIAN 0
#define BIG_ENDIAN    1

// SEE
// http://www.linuxforums.org/forum/programming-scripting/63464-finding-out-systems-endianness.html
int machineEndianness() {
   int i = 1;
   char *p = (char *) &i;
   if (p[0] == 1) // Lowest address contains the least significant byte
      return LITTLE_ENDIAN;
   else
      return BIG_ENDIAN;
}

void main() {
  int x = machineEndianness();
  if (x == LITTLE_ENDIAN) printf("little endian\n");
  else printf("big endian\n");
}

