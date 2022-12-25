SHELL = /bin/bash

gc:
	clang -fpic -shared cutils/gc.cpp -o cutils/gc.so -O3

intrinsics:
	clang -c cutils/runtime.c -o cutils/runtime.o -O3

compiler:
	ocamlbuild -pkg num main.native
	mv main.native droml

all: gc intrinsics compiler