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

CC=clang

LDFLAGS=

SOURCES=\
	backwards_while.c \
	product.c \
	factorial.c \
	test.c \

BINS=$(SOURCES:.c=.out)

all: $(BINS)

.c.out:
	@echo Compiling $@
	@$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *.out
