#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "intrinsics.h"

void _abort(i64 code) {
    exit(code);
}

stringarr* _makestrvec(i64 argc, i8** argv) {
    stringarr *res = (stringarr*) _allocate(sizeof(stringarr));
    res->size = argc;
    res->base = (string**) _allocate(sizeof(string*) * argc);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);

    for (i64 i = 0; i < argc; i++) {
        i8* str = argv[i];
        res->base[i] = (string*) _allocate(sizeof(string));
        _addchild((i8*) res, (i8*) res->base[i]);
        _removeref((i8*) res->base[i]);

        string* arrval = (string*) res->base[i];
        arrval->size = strlen(str) + 1;
        arrval->base = _allocate(arrval->size);
        _addchild((i8*) arrval, arrval->base);
        _removeref((i8*) arrval->base);

        strcpy(arrval->base, str);
        arrval->base[arrval->size - 1] = '\0';
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
    res->size = a->size + b->size;

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

static inline i64 max(i64 a, i64 b) {
    return a > b ? a : b;
}

intarr* _makerangelist(i64 start, i64 end, bool inclstart, bool inclend) {
    if (start < end) {
        if (!inclstart) ++start;
        if (!inclend) --end;
        intarr* res = allocate_intarr(max(end - start + 1, 0));
        for (i64 i = 0; i < res->size; i++) {
            res->base[i] = start + i;
        }
        return res;
    } else {
        if (!inclstart) --start;
        if (!inclend) ++end;
        intarr* res = allocate_intarr(max(start - end + 1, 0));
        for (i64 i = res->size - 1; i >= 0; i--) {
            res->base[i] = start - i;
        }
        return res;
    }
}
