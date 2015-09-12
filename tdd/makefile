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
	@cc src/main.c -o main.out

clean:
	rm -f *.o *.out

unity.o:
	@echo Compiling $@
	@cc -c test/vendor/unity.c

tests.out: unity.o
	@echo Compiling $@
	@cc *.o test/main.c -o tests.out

test: tests.out
	@./tests.out
