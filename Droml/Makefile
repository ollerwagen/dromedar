SHELL = /bin/bash


OFLAGS = -O3
WFLAGS = -Wall

COMPILE = clang $(OFLAGS) $(WFLAGS)


OUT = obj
STDLIB = cutils/drmstdlib

INCL = -I cutils/


gc:
	clang -fpic -shared cutils/gc.cpp -o $(OUT)/gc.so -O3

intrinsics:
	$(COMPILE) $(INCL) -o $(OUT)/File.o -c $(STDLIB)/File.c
	$(COMPILE) $(INCL) -o $(OUT)/IO.o -c $(STDLIB)/IO.c
	$(COMPILE) $(INCL) -o $(OUT)/Math.o -c $(STDLIB)/Math.c
	$(COMPILE) $(INCL) -o $(OUT)/Str.o -c $(STDLIB)/Str.c
	$(COMPILE) -c cutils/common.c -o $(OUT)/common.o
	$(COMPILE) -c cutils/intrinsics.c -o $(OUT)/intrinsics.o
	$(COMPILE) -c cutils/sprintflib.c -o $(OUT)/sprintflib.o
	clang -fpic -shared cutils/listops.cpp -o $(OUT)/listops.so -O3
	clang -fpic -shared -I cutils/ cutils/drmstdlib/cpputils/File.cpp -o $(OUT)/cpputils_File.so -O3

compiler:
	ocamlbuild -pkg num main.native
	mv main.native droml

all: gc intrinsics compiler

clean:
	rm -f $(OUT)/* || true
	rm -f droml || true
	rm -f Out.ll || true
	rm -f Out.s || true