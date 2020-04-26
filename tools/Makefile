CC ?= gcc
CFLAGS ?= -Wall -pipe -O2 -ffunction-sections -fdata-sections
LDFLAGS ?= -static-pie -Wl,--gc-sections

TOOLS:= setparts

.PHONY: all
all: $(TOOLS)

setparts: setparts.c
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

.PHONY: clean
clean:
	$(RM) $(TOOLS) *.o
