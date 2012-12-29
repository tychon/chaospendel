ACFLAGS := -mmcu=atmega2560 -Os -Wall -Werror -DF_CPU=16000000L
CFLAGS := -g -std=gnu99 -Wall -Werror -D_GNU_SOURCE -lm -lX11 -lXext
DUDEFLAGS := -p m2560 -c stk500v2 -P /dev/ttyACM0 -b 115200

.PHONY: all flash_native_adc_to_serial

all: reader flash_native_adc_to_serial

flash_native_adc_to_serial: native_adc_to_serial.hex
	avrdude $(DUDEFLAGS) -U flash:w:"native_adc_to_serial.hex"

native_adc_to_serial.hex: native_adc_to_serial
	avr-objcopy -j .text -j .data -O ihex native_adc_to_serial native_adc_to_serial.hex

native_adc_to_serial: native_adc_to_serial.c
	avr-gcc $(ACFLAGS) -o native_adc_to_serial native_adc_to_serial.c
