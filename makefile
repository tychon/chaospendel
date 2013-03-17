DIRS := common arduino_read arduino_read/native_arduino_code network numerical wikihaskell

all:
	$(foreach var,$(DIRS),make -C $(var);)

clean:
	$(foreach var,$(DIRS),make -C $(var) clean;)
