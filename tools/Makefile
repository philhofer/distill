CC ?= gcc
CFLAGS ?= -Wall -pipe -O2
LDFLAGS ?= -static-pie

setparts: setparts.c
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@
