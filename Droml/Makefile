SHELL = /bin/bash


OFLAGS = -O3
WFLAGS = -Wall

COMPILE = clang $(OFLAGS) $(WFLAGS)
COMPILE_LIB = clang $(OFLAGS) $(WFLAGS) -fpic -shared


OUT = obj
STDLIB = cutils/drmstdlib
EDULIB = cutils/edulib

SFML = cutils/edulib/SFML-2.5.1

INCL = -I cutils/

setup:
	chmod +x togglegc.sh
	[ -d obj ] || mkdir obj

togglegc: setup
	./togglegc.sh

gc:
	$(COMPILE_LIB) cutils/gc.cpp -o $(OUT)/gc.so

intrinsics:
	$(COMPILE) -c cutils/common.c -o $(OUT)/common.o
	$(COMPILE) -c cutils/intrinsics.c -o $(OUT)/intrinsics.o
	$(COMPILE) -c cutils/sprintflib.c -o $(OUT)/sprintflib.o
	$(COMPILE_LIB) cutils/listops.cpp -o $(OUT)/listops.so
	$(COMPILE_LIB) cutils/templ_intrinsics.cpp -o $(OUT)/templ_intrinsics.so

compiler:
	ocamlbuild -pkg num main.native
	mv main.native droml

stdlib:
	$(COMPILE) $(INCL) -o $(OUT)/File.o   -c $(STDLIB)/File.c
	$(COMPILE) $(INCL) -o $(OUT)/IO.o     -c $(STDLIB)/IO.c
	$(COMPILE) $(INCL) -o $(OUT)/Math.o   -c $(STDLIB)/Math.c
	$(COMPILE) $(INCL) -o $(OUT)/Str.o    -c $(STDLIB)/Str.c
	$(COMPILE) $(INCL) -o $(OUT)/Regex.o  -c $(STDLIB)/Regex.c
	$(COMPILE) $(INCL) -o $(OUT)/Sys.o    -c $(STDLIB)/Sys.c
	$(COMPILE) $(INCL) -o $(OUT)/Time.o   -c $(STDLIB)/Time.c
	$(COMPILE) $(INCL) -o $(OUT)/Util.o   -c $(STDLIB)/Util.c
	$(COMPILE_LIB) $(INCL) cutils/drmstdlib/cpputils/File.cpp  -o $(OUT)/cpputils_File.so
	$(COMPILE_LIB) $(INCL) cutils/drmstdlib/cpputils/Regex.cpp -o $(OUT)/cpputils_Regex.so
	$(COMPILE_LIB) $(INCL) cutils/drmstdlib/cpputils/Time.cpp  -o $(OUT)/cpputils_Time.so

edulib:
	$(COMPILE) $(INCL) -o $(OUT)/Turtle.o -c $(EDULIB)/Turtle.c
	$(COMPILE_LIB) $(INCL) -I $(SFML)/include cutils/edulib/cpputils/Turtle.cpp -o $(OUT)/edulib_cpputils_Turtle.so -L $(SFML)/lib -lsfml-graphics -lsfml-window -lsfml-system

all: setup gc intrinsics stdlib edulib compiler

clean:
	rm -f $(OUT)/* || true
	rm -f droml || true
	rm -f Out.ll || true
	rm -f Out.s || true
	ocamlbuild -clean