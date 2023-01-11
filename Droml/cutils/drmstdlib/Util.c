#include <stdlib.h>

#include "common.h"
#include "gc.h"

i64 _Util$randint() {
    return (i64)rand() | (i64)rand() << 32;
}

double _Util$randflt() {
    return (double)_Util$randint() / (double) __LONG_LONG_MAX__;
}