CFLAGS=-std=c99
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Werror
CFLAGS += -Wpointer-arith
CFLAGS += -Wcast-align
CFLAGS += -Wwrite-strings
CFLAGS += -Wswitch-default
CFLAGS += -Wunreachable-code
CFLAGS += -Winit-self
CFLAGS += -Wmissing-field-initializers
CFLAGS += -Wno-unknown-pragmas
CFLAGS += -Wstrict-prototypes
CFLAGS += -Wundef
CFLAGS += -Wold-style-definition

LDFLAGS=

SOURCES=\
				00_scanf.c \
				04_dweight.c \
				05_dweight.c \
				ex1.c \
				ex2.c \
				ex3.c \
				ex4.c \
				ex5.c \

BINS=$(SOURCES:.c=.out)

all: $(BINS)

.c.out:
	@echo Compiling $@
	@$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *.out
