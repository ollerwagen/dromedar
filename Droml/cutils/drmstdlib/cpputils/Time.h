#ifndef __STDLIB_TIME_C_CONNECTOR__
#define __STDLIB_TIME_C_CONNECTOR__

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

    i8* _cpputils_Time$now();
    i8* _cpputils_Time$dt(i8* a, i8* b);

    void _cpputils_Time$sleep_for(i8* d);

    i64 _cpputils_Time$s(i8* d);
    i64 _cpputils_Time$ms(i8* d);
    i64 _cpputils_Time$us(i8* d);

    i8* _cpputils_Time$of_s(i64 i);
    i8* _cpputils_Time$of_ms(i64 i);
    i8* _cpputils_Time$of_us(i64 i);

#ifdef __cplusplus
}
#endif

#endif // __STDLIB_TIME_C_CONNECTOR__
