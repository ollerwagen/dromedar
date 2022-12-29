SHELL = /bin/bash


OFLAGS = -O3
WFLAGS = -Wall -Wextra -Wpedantic

COMPILE = clang $(OFLAGS) $(WFLAGS)


gc:
	clang -fpic -shared cutils/gc.cpp -o cutils/gc.so -O3

intrinsics:
	$(COMPILE) -c cutils/common.c -o cutils/common.o
	$(COMPILE) -c cutils/intrinsics.c -o cutils/intrinsics.o
	$(COMPILE) -c cutils/runtime.c -o cutils/runtime.o
	$(COMPILE) -c cutils/sprintflib.c -o cutils/sprintflib.o

compiler:
	ocamlbuild -pkg num main.native
	mv main.native droml

all: gc intrinsics compiler

clean:
	rm -f cutils/*.o || true
	rm -f droml || true
	rm -f Out.ll || true
	rm -f Out.s || true