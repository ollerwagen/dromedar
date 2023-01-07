#include <stdio.h>

#include "common.h"

void __IO$print_int(IGNORE, i64 i) {
    printf("%ld\n", i);
}

void __IO$print_flt(IGNORE, double d) {
    printf("%lf\n", d);
}

void __IO$print_str(IGNORE, string* s) {
    printf("%s", s->base);
}

void __IO$print_char(IGNORE, i8 c) {
    printf("%c", c);
}

void __IO$print_bool(IGNORE, i1 b) {
    printf("%s\n", b ? "true" : "false");
}