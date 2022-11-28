SHELL = /bin/bash

CFLAGS = -std=c++2a
WFLAGS = -Wall -Wpedantic -Wextra
OFLAGS = -O3

compile:
	g++ -c src/*.cpp $(CFLAGS) $(WFLAGS) $(OFLAGS)
	mv *.o bin
	g++ -o app bin/*.o