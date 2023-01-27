#ifndef __STDLIB_REGEX_C_CONNECTOR__
#define __STDLIB_REGEX_C_CONNECTOR__

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

    void _cpputils_Turtle$make();
    void _cpputils_Turtle$close();

    void _cpputils_Turtle$fd(double);
    void _cpputils_Turtle$rt(double);

    void _cpputils_Turtle$pen(bool);

    void _cpputils_Turtle$setPenColor(const char*);
    void _cpputils_Turtle$setPenWidth(double);

    void _cpputils_Turtle$showTurtle();
    void _cpputils_Turtle$hideTurtle();

#ifdef __cplusplus
}
#endif

#endif // __STDLIB_REGEX_C_CONNECTOR__
