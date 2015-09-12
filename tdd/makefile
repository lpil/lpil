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

all: main.out

run: main.out
	@./main.out

main.out:
	@echo Compiling src/main.c
	@cc $(CFLAGS) src/main.c -o main.out

clean:
	rm -f *.o *.out

unity.o:
	@echo Compiling test/vendor/unity.c
	@cc $(CFLAGS) -c test/vendor/unity.c

tests.out: unity.o
	@echo Compiling test/main.c
	@cc $(CFLAGS) *.o test/main.c -o tests.out

test: tests.out
	@./tests.out
