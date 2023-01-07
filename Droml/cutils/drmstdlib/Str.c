#include <stdio.h>

#include "common.h"
#include "gc.h"

string* _Str$of_int(IGNORE, i64 i) {
    string* res = allocate_string(30);
    sprintf(res->base, "%ld", i);
    return res;
}

string* _Str$of_flt(IGNORE, double d) {
    string* res = allocate_string(30);
    sprintf(res->base, "%lf", d);
    return res;
}