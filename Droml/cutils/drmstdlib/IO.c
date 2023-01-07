#include <stdio.h>

#include "common.h"

void _IO$print_int1(i64 *ignore, i64 i) {
    printf("%ld\n", i);
}

void _IO$print_flt(double d) {
    printf("%lf\n", d);
}

void _IO$print_str(string* s) {
    printf("%s", s->base);
}

void _IO$print_char(i8 c) {
    printf("%c", c);
}

void _IO$print_bool(i1 b) {
    printf("%s\n", b ? "true" : "false");
}