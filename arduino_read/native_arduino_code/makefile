ACFLAGS := -mmcu=atmega2560 -Os -Wall -Werror -DF_CPU=16000000L
DUDEFLAGS := -p m2560 -c stk500v2 -P /dev/ttyACM0 -b 115200

.PHONY: flash

all: native_adc_to_serial.hex

flash: native_adc_to_serial.hex
	avrdude $(DUDEFLAGS) -U flash:w:"native_adc_to_serial.hex"

native_adc_to_serial.hex: native_adc_to_serial.x
	avr-objcopy -j .text -j .data -O ihex native_adc_to_serial.x native_adc_to_serial.hex

native_adc_to_serial.x: native_adc_to_serial.c
	avr-gcc $(ACFLAGS) -o native_adc_to_serial.x native_adc_to_serial.c

clean:
	rm -f *.hex *.x

