#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>


#include "common.h"
#include "intrinsics.h"
#include "sprintflib.h"

#include "gc.h"


void print_int(i64 num) {
    printf("%ld\n", num);
}

void print_flt(double num) {
    printf("%lf\n", num);
}

void print_char(char c) {
    printf("%c\n", c);
}

void print_str(string *s) {
    printf("%s", s->base);
}

void println() {
    putchar('\n');
}




string* int_to_str(i64 num) {
    return _sprintf_int(num);
}

string* flt_to_str(double num) {
    return _sprintf_flt(*(i64*)&num);
}

string *char_to_str(i8 c) {
    return _sprintf_char((i64) c);
}

string *bool_to_str(i1 b) {
    return _sprintf_bool((i64) b);
}
