#include <stdint.h>
#include <math.h>
#include <stdio.h>

typedef int64_t i64;

i64 pow_ii(i64 base, i64 exp)
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

double pow_ff(double base, double exp)
{
    return pow(base, exp);
}

void print_int(i64 num) {
    printf("%ld\n", num);
}

void print_flt(double num) {
    printf("%lf\n", num);
}