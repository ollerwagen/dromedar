#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "gc.h"

typedef int64_t i64;
typedef char    i8;
typedef _Bool   i1;

typedef struct string { i64 size ; i8 *base; } string;
typedef struct blindarr { i64 size ; i8 * base; } blindarr;


static string* allocate_string(i64 size) {
    string* res = (string*) _allocate(sizeof(string));
    res->size = size;
    res->base = _allocate(size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);
    return res;
}


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
    string* res = allocate_string(a->size + b->size - 1);

    strcpy(res->base, a->base);
    strcat(res->base, b->base);

    return res;
}

string* _strmul_1(string *s, i64 count) {
    if (count < 0)
        count = 0;
    
    string* res = allocate_string(count * (s->size - 1) + 1);

    strcpy(res->base, "");
    for (i64 i = 0; i < count; i++)
        strcat(res->base, s->base);
    
    return res;
}

string* _strmul_2(i64 count, string *s) {
    return _strmul_1(s, count);
}

i64 _strcmp(string *a, string *b) {
    return strcmp(a->base, b->base);
}

blindarr* _arrconcat(blindarr *a, blindarr *b, i64 elemsize, i1 areptrs) {
    i64 realasize = a->size * elemsize, realbsize = b->size * elemsize;

    blindarr *res = (blindarr*) _allocate(sizeof(blindarr));
    res->base = _allocate(realasize + realbsize);
    res->size = realasize + realbsize;
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    memcpy(res->base, a->base, realasize);
    memcpy(res->base + realasize, b->base, realbsize);

    if (areptrs) {
        for (i64 i = 0; i < a->size; i++)
            _addchild((i8*) res, (i8*) (a->base + i * elemsize));
        for (i64 i = 0; i < b->size; i++)
            _addchild((i8*) res, (i8*) (b->base + i * elemsize));
    }

    return res;
}


string* _sprintf_int(i64 i) {
    string* res = allocate_string(30);
    sprintf(res->base, "%ld", i);
    res->size = strlen(res->base) + 1;
    return res;
}

string* _sprintf_flt(double d) {
    string* res = allocate_string(50);
    sprintf(res->base, "%lf", d);
    res->size = strlen(res->base) + 1;
    return res;
}

string* _sprintf_char(i8 c) {
    string* res = allocate_string(2);
    sprintf(res->base, "%c", c);
    return res;
}

string* _sprintf_bool(i1 b) {
    string* res = allocate_string(b ? 5 : 6);
    sprintf(res->base, "%s", b ? "true" : "false");
    return res;
}



string* _sprintf_cat(i64 size, ...) {
    i64 sumlen = 0;
    va_list va;
    va_start(va, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(va, string*);
        sumlen += str->size - 1;
    }
    va_end(va);

    string *res = allocate_string(sumlen + 1);
    i64 index = 0;
    va_list vb;
    va_start(vb, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(vb, string*);
        strcpy(res->base + index, str->base);
        index += str->size - 1;
    }
    va_end(vb);
    return res;
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
