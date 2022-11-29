SHELL = /bin/bash

CFLAGS = -std=c++17
WFLAGS = -Wall -Wpedantic -Wextra
OFLAGS = 

compile:
	g++ -c src/*.cpp $(CFLAGS) $(WFLAGS) $(OFLAGS)
	mv *.o bin
	g++ -o app bin/*.o