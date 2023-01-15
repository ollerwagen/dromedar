#include <time.h>

#include "common.h"
#include "gc.h"

#include "cpputils/Time.h"

i64 _Time$clock(IGNORE) {
    return clock();
}

i64 _Time$time(IGNORE) {
    return time(NULL);
}

void _Time$sleep_for(IGNORE, i8* d) {
    return _cpputils_Time$sleep_for(d);
}

i8* _Time$now(IGNORE) {
    return _cpputils_Time$now();
}

i8* _Time$dt(IGNORE, i8* a, i8* b) {
    return _cpputils_Time$dt(a, b);
}

i64 _Time$s(IGNORE, i8* d) {
    return _cpputils_Time$s(d);
}

i64 _Time$ms(IGNORE, i8* d) {
    return _cpputils_Time$ms(d);
}

i64 _Time$us(IGNORE, i8* d) {
    return _cpputils_Time$us(d);
}

i8* _Time$of_s(IGNORE, i64 i) {
    return _cpputils_Time$of_s(i);
}

i8* _Time$of_ms(IGNORE, i64 i) {
    return _cpputils_Time$of_ms(i);
}

i8* _Time$of_us(IGNORE, i64 i) {
    return _cpputils_Time$of_us(i);
}