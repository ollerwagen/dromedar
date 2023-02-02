#include <stdio.h>

#include "common.h"

void _IO$print_int(IGNORE, i64 i) {
    printf("%ld\n", i);
}

void _IO$print_flt(IGNORE, double d) {
    printf("%lf\n", d);
}

void _IO$print_str(IGNORE, string* s) {
    for (i64 i = 0; i < s->size; i++)
        putchar(s->base[i]);
}

void _IO$print_char(IGNORE, i8 c) {
    printf("%c", c);
}

void _IO$print_bool(IGNORE, i1 b) {
    printf("%s\n", b ? "true" : "false");
}