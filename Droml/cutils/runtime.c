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
    printf("%ld", num);
}

void print_flt(double num) {
    printf("%lf", num);
}

void print_char(char c) {
    putchar(c);
}

void print_str(string *s) {
    printf("%s", s->base);
}

void println() {
    putchar('\n');
}




string* int_to_str(i64 num) {
    string *res = (string*) _allocate(sizeof(string));

    int size = 2;

    if (num > 0)
        size = (int)(ceil(log10(num)))+1;
    else if (num < 0)
        size = (int)(ceil(log10(num)))+2;

    res->size = size;
    res->base = (i8*) _allocate(size);

    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    sprintf(res->base, "%ld", num);

    return res;
}
