#include <time.h>

#include "common.h"
#include "gc.h"

#include "cpputils/Time.h"

i64 _Time$clock() {
    return clock();
}

i64 _Time$time() {
    return time(NULL);
}

i8* _Time$now() {
    return _cpputils_Time$now();
}

i8* _Time$dt(i8* a, i8* b) {
    return _cpputils_Time$dt(a, b);
}

i64 _Time$s(i8* d) {
    return _cpputils_Time$s(d);
}

i64 _Time$ms(i8* d) {
    return _cpputils_Time$ms(d);
}

i64 _Time$us(i8* d) {
    return _cpputils_Time$us(d);
}