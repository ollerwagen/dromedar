#include <stdlib.h>

#include "common.h"
#include "gc.h"

i64 _Util$randint(IGNORE) {
    return (i64)rand() | (i64)rand() << 32;
}

double _Util$randflt(IGNORE) {
    return (double)_Util$randint(NULL) / (double) __LONG_LONG_MAX__;
}