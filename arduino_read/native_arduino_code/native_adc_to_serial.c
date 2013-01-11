#include <avr/io.h>
#include <stdint.h>

//#define USART_BAUDRATE 57600
#define USART_BAUDRATE 500000
#define BAUD_PRESCALE (((F_CPU / (USART_BAUDRATE * 16UL))) - 1)

// arduino digital pin 13 (LED) is at port B, bit 7 (high bit)

// set up the analog->digital converter chip
static void adc_init(void) {
  // increment instead of setting at the bottom of the method to
  // get rid of unused variable warnings, set initial value here
  // to get rid of uninitialized variable use. maybe -Wall -Werror
  // was a bad idea...
  uint16_t result=0;
 
  // use avcc as reference
  ADMUX = (0<<REFS1) | (1<<REFS0);
  
  // Bit ADFR ("free running") in ADCSRA steht beim Einschalten
  // schon auf 0, also single conversion
  // set frequency divisor to 128; 16000kHz/128=125kHz, must be 50kHz<=n<=200kHz
  ADCSRA = (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0);
  ADCSRA |= (1<<ADEN);                  // ADC aktivieren
 
  /* nach Aktivieren des ADC wird ein "Dummy-Readout" empfohlen, man liest
     also einen Wert und verwirft diesen, um den ADC "warmlaufen zu lassen" */
 
  ADCSRA |= (1<<ADSC);                  // eine ADC-Wandlung 
  while (ADCSRA & (1<<ADSC) ) {         // auf Abschluss der Konvertierung warten
  }
  /* ADCW muss einmal gelesen werden, sonst wird Ergebnis der nächsten
     Wandlung nicht übernommen. */
  result += ADCW;
}

// request an ADC measurement
// The result can later be read using adc_read_result. This call
// won't wait for the conversion to finish.
static void adc_measure(uint8_t ch) {
  // our input is in the range 0..15, but we have to convert that
  // according to the datasheet
  // (section 26.8.2 - ADC Control and Status Register B)
  if (ch >= 0x08) {
    ch -= 0x08;
    ch += 0x20;
  }
  
  // update the channel field (lower 5 bits) of ADMUX
  ADMUX = (ADMUX & ~(0x1F)) | (ch & 0x1F);
  
  // update the high bit (sixth) of the channel which is in ADCSRB
  ADCSRB = (ADCSRB & ~(1<<MUX5)) | ((ch&0x20)?(1<<MUX5):0);

  // request a single conversion
  ADCSRA |= (1<<ADSC);
}

static uint8_t analog_slow_flag = 0;
static uint16_t adc_read_result(void) {
  uint16_t r = 0;
  analog_slow_flag = 0;
  while (ADCSRA & (1<<ADSC)) {
    // busyloop until input is available
    analog_slow_flag = 1;
  }
  r = ADCL;
  r += (ADCH<<8);
  return r;
}

static void serial_init(void) {
  // load upper 8 bits of the baud rate into the high byte of the UBRR register
  UBRR0H = (BAUD_PRESCALE >> 8);
  // load lower 8 bits of the baud rate into the low byte of the UBRR register
  UBRR0L = BAUD_PRESCALE;

  // 8data,1stopbit
  UCSR0C = (0 << UMSEL00) | (1 << UCSZ00) | (1 << UCSZ01);
  // turn on the transmission and reception circuitry
  UCSR0B = (1 << RXEN0) | (1 << TXEN0) | (0 << UCSZ02);
}

static void sendbyte(uint8_t b) {
  // do nothing until UDR is ready for more data to be written to it
  while ((UCSR0A & (1 << UDRE0)) == 0) {};
  // memory was cleared - write to it
  UDR0 = b;
}

static void digi_init() {
  // configure port B7 (arduino digital port 13) as output and set it
  // low
  PORTB = (0<<PB7) | (0<<PB4);
  DDRB = (1<<DDB7) | (1<<DDB4);
}

static void digi_set(int val) {
  PORTB = (val<<PB7) | (val<<PB4);
}

int main(void) {
  uint8_t pin;
  uint16_t val;

  serial_init();
  adc_init();
  digi_init();
  digi_set(1);
  pin = 0;

  adc_measure(pin++);
  val = adc_read_result();
  while (1) {
    // do we have to change the output value?
    if ((UCSR0A & (1 << RXC0)) == 1) {
      uint8_t cmd = UDR0;
      cmd += 1; // UNDO THIS!!!!!
      digi_set(1);
      /*
      if (cmd == 42) {
        digi_set(1);
      } else if (cmd == 41) {
        digi_set(0);
      }
      */
    }
    
    adc_measure((pin++)&0x0f);

    // start by sending the pin id of the current value
    // high bit is 1, lowest 4 bits are the pin id
    // middle bits are used for perf and signature
    uint8_t headbyte = 0x80|0x20; // high bits are 1,0,1
    // bit above the pin id is whether analog input is the slowest part
    headbyte |= (analog_slow_flag)<<4;
    headbyte |= (pin-2)&0x0f; // low 4 bits are the pin id
    sendbyte(headbyte);

    // first bit of every value component is 0
    // send the value. it's stored as 16 bits: 0-15, but 0-5 are 0
    sendbyte((uint8_t)(val >> 3)); // bits 5-12 (first is 0)
    sendbyte((uint8_t)(val & 0x7f)); // bits 9-15 (no first bit -> first is 0)

    val = adc_read_result();
  }
}
