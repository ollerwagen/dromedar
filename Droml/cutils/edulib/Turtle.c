#include "common.h"
#include "cpputils/Turtle.h"

void _Turtle$make(IGNORE) {
    _cpputils_Turtle$make();
}

void _Turtle$close(IGNORE) {
    _cpputils_Turtle$close();
}

void _Turtle$fd(IGNORE, double x) {
    _cpputils_Turtle$fd(x);
}

void _Turtle$bk(IGNORE, double x) {
    _cpputils_Turtle$fd(-x);
}

void _Turtle$rt(IGNORE, double a) {
    _cpputils_Turtle$rt(a);
}

void _Turtle$lt(IGNORE, double a) {
    _cpputils_Turtle$rt(-a);
}

void _Turtle$penUp(IGNORE) {
    _cpputils_Turtle$pen(false);
}

void _Turtle$penDn(IGNORE) {
    _cpputils_Turtle$pen(true);
}