#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int64_t i64;
typedef int8_t  i8;

typedef struct { i64 size ; i8 *base; } string;

i64 _pow_ii(i64 base, i64 exp)
{
    i64 result = 1;
    for (;;)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }

    return result;
}

double _pow_ff(double base, double exp)
{
    return pow(base, exp);
}

void _memcpy(i8 *from, i8 *to, i64 size) {
    memcpy(to, from, size);
}



void print_int(i64 num) {
    printf("%ld\n", num);
}

void print_flt(double num) {
    printf("%lf\n", num);
}

void print_str(string *s) {
    printf("%s\n", s->base);
}


#include "gc.h"

/*
#include <stdio.h>
int main() {

    ptr a = _allocate(10);
    ptr b = _allocate(20);

    _removeref(b);

    _allocate(50);

    return 0;
}
*/
