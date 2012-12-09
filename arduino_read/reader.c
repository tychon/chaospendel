#include <termios.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>

#define DOOR_OPEN 0
#define DOOR_CLOSED 1

int main(void) {
  int serial_fd = open("/dev/ttyACM0", O_RDONLY | O_NOCTTY);
  struct termios config;
  if(tcgetattr(serial_fd, &config) < 0) exit(1);
  config.c_iflag &= ~(IGNBRK | BRKINT | ICRNL | INLCR | PARMRK | INPCK | ISTRIP | IXON);
  config.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
  config.c_cflag &= ~(CSIZE | PARENB);
  config.c_cflag |= CS8;
  config.c_cc[VMIN]  = 1;
  config.c_cc[VTIME] = 0;
  if(cfsetispeed(&config, B57600) < 0 || cfsetospeed(&config, B57600) < 0) exit(1);
  if(tcsetattr(serial_fd, TCSAFLUSH, &config) < 0) exit(1);
  FILE *serial = fdopen(serial_fd, "r");

  setbuf(stdout, NULL);
  setbuf(serial, NULL);

  char l[255];
  while (1) {
    if (fgets(l, 255, serial) == NULL) return 0;
    char *c=l;
    while (*c != 0) {
      if ((*c < '0' || *c > '9') && *c != '\r') *c='\n';
      c++;
    }
    printf("%s", l);
  }
  return 42;
}
