#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "intrinsics.h"
#include "templ_intrinsics.h"

void _abort(i64 code) {
    exit(code);
}

void _checknull(i8* ptr) {
    if (ptr == NULL) {
        fprintf(stderr, "Null Pointer Exception in assert expression\n");
        exit(1);
    }
}

stringarr* _makestrvec(i64 argc, i8** argv) {
    stringarr* res = (stringarr*) _allocate_blindarr(argc, sizeof(string*));

    for (i64 i = 0; i < argc; i++) {
        i8* str = argv[i];

        string* arrval = _allocate_string(strlen(str));

        _addchild((i8*) res, (i8*) arrval);
        _removeref((i8*) arrval);

        memcpy(arrval->base, str, arrval->size);

        res->base[i] = arrval;
    }

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
    string* res = _allocate_string(a->size + b->size);

    memcpy(res->base, a->base, a->size);
    memcpy(res->base + a->size, b->base, b->size);

    return res;
}

string* _strmul_1(string *s, i64 count) {
    if (count < 0)
        count = 0;
    
    string* res = _allocate_string(count * s->size);

    int index = 0;
    for (i64 i = 0; i < count; i++) {
        memcpy(res->base + index, s->base, s->size);
        index += s->size;
    }
    
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

    blindarr* res = _allocate_blindarr(a->size + b->size, elemsize);

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

blindarr* _arrmul(blindarr *a, i64 elemsize, i64 factor, i1 areptrs) {
    i64 actualsize = a->size * elemsize;

    if (factor < 0)
        factor = 0;

    blindarr *res = _allocate_blindarr(a->size * factor, elemsize);

    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    i64 index = 0;
    for (i64 i = 0; i < factor; i++) {
        memcpy(res->base + index, a->base, actualsize);
        index += actualsize;
    }

    if (areptrs)
        for (i64 i = 0; i < factor; i++)
            for (i64 i = 0; i < a->size; i++)
                _addchild((i8*) res, (i8*) * (i64*) (a->base + i * elemsize));
    
    return res;
}

chararr* _makerangecharlist(i8 start, i8 end, bool inclstart, bool inclend) {
    return _cpp_rangechar(start, end, inclstart, inclend);
}

intarr* _makerangeintlist(i64 start, i64 end, bool inclstart, bool inclend) {
    return _cpp_rangeint(start, end, inclstart, inclend);
}

void _print_string(string *s) {
    for (int i = 0; i < s->size; i++)
        putchar(s->base[i]);
}


