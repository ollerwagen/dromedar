SHELL = /bin/bash


OFLAGS = -O3
WFLAGS = -Wall

COMPILE = clang $(OFLAGS) $(WFLAGS)
COMPILE_LIB = clang $(OFLAGS) $(WFLAGS) -fpic -shared


OUT = obj
STDLIB = cutils/drmstdlib

INCL = -I cutils/

setup:
	[ -d obj ] || mkdir obj

gc:
	$(COMPILE_LIB) cutils/gc.cpp -o $(OUT)/gc.so

intrinsics:
	$(COMPILE) -c cutils/common.c -o $(OUT)/common.o
	$(COMPILE) -c cutils/intrinsics.c -o $(OUT)/intrinsics.o
	$(COMPILE) -c cutils/sprintflib.c -o $(OUT)/sprintflib.o
	$(COMPILE_LIB) cutils/listops.cpp -o $(OUT)/listops.so

compiler:
	ocamlbuild -pkg num main.native
	mv main.native droml

stdlib:
	$(COMPILE) $(INCL) -o $(OUT)/File.o  -c $(STDLIB)/File.c
	$(COMPILE) $(INCL) -o $(OUT)/IO.o    -c $(STDLIB)/IO.c
	$(COMPILE) $(INCL) -o $(OUT)/Math.o  -c $(STDLIB)/Math.c
	$(COMPILE) $(INCL) -o $(OUT)/Str.o   -c $(STDLIB)/Str.c
	$(COMPILE) $(INCL) -o $(OUT)/Regex.o -c $(STDLIB)/Regex.c
	$(COMPILE_LIB) -I cutils/ cutils/drmstdlib/cpputils/File.cpp -o  $(OUT)/cpputils_File.so
	$(COMPILE_LIB) -I cutils/ cutils/drmstdlib/cpputils/Regex.cpp -o $(OUT)/cpputils_Regex.so

all: setup gc intrinsics stdlib compiler

clean:
	rm -f $(OUT)/* || true
	rm -f droml || true
	rm -f Out.ll || true
	rm -f Out.s || true
	ocamlbuild -clean