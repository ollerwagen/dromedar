#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

typedef int64_t i64;
typedef char    i8;

typedef struct string { i64 size ; i8 *base; } string;

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


string* _strconcat(string *a, string *b) {
    string* res = (string*) _allocate(sizeof(string));
    res->size = a->size + b->size - 1;
    res->base = _allocate(res->size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    strcpy(res->base, a->base);
    strcat(res->base, b->base);

    return res;
}

string* _strmul_1(string *s, i64 count) {
    if (count < 0)
        count = 0;
    
    string* res = (string*) _allocate(sizeof(string));
    res->size = count * (s->size - 1) + 1;
    res->base = _allocate(res->size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    strcpy(res->base, "");
    for (i64 i = 0; i < count; i++)
        strcat(res->base, s->base);
    
    return res;
}

string* _strmul_2(i64 count, string *s) {
    return _strmul_1(s, count);
}


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
