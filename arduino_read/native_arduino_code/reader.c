#include <termios.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

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
  setbuf(stderr, NULL);
  setbuf(serial, NULL);

  int adc_was_blocking = -1;

  int wantport = 0;

  while (1) {
    char b;
    while (((b=fgetc(serial))&0xe0) != 0xa0) {
      // ignore bad head
      fprintf(stderr, "bad byte 0x%x - waiting for valid head\n", b);
    };

    int adc_blocked = (b&0x10) ? true : false;
    int port = (b&0x0f);
    int hpart = fgetc(serial);
    if (hpart&0x80) { ungetc(hpart, serial); goto badval; }
    int lpart = fgetc(serial);
    if (lpart&0x80) { ungetc(lpart, serial); goto badval; }
    // bits 9,10,11,12 are duplicated - make sure they're the same
    if ((hpart&0x0f) != (lpart>>3)) goto badval;
    int val = (hpart<<3)|lpart;
    if (port != wantport) {
      fprintf(stderr, "want port %i, but got data about port %i\n", wantport, port);
      continue;
    }
    wantport++;
    if (wantport == 16) wantport = 0;
    fprintf(stdout, "%i %i\n", port, val);

    if (adc_blocked != adc_was_blocking) {
      adc_was_blocking = adc_blocked;
      if (adc_blocked) {
        fprintf(stderr, "ADC is the slow part\n");
      } else {
        fprintf(stderr, "ADC isn't the slow part\n");
      }
    }

    continue;

badval:
    fprintf(stderr, "invalid data!\n");
  }
  return 42;
}
