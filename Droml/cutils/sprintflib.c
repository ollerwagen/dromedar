#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "sprintflib.h"


string* _sprintf_int(i64 i) {
    string* res = _allocate_string(30);
    sprintf(res->base, "%ld", i);
    res->size = strlen(res->base);
    return res;
}

string* _sprintf_flt(i64 d) {
    double dval = *(double*)(&d);
    string* res = _allocate_string(50);
    sprintf(res->base, "%lf", dval);
    res->size = strlen(res->base);
    return res;
}

string* _sprintf_char(i64 c) {
    string* res = _allocate_string(2);
    sprintf(res->base, "%c", (char) c);
    res->size = 1;
    return res;
}

string* _sprintf_bool(i64 b) {
    string* res = _allocate_string(b ? 5 : 6);
    sprintf(res->base, "%s", b ? "true" : "false");
    res->size = strlen(res->base);
    return res;
}

static string* makenullstr() {
    string* res = _allocate_string(5);
    sprintf(res->base, "null");
    res->size = strlen(res->base);
    return res;
}

string* _sprintf_str(i64 s) {
    if ((void*) s == NULL)
        return makenullstr();

    string* res = (string*) s;
    _addref((i8*) s);
    return res;
}

string* _sprintf_array(i64 ptr, i64 depth, i64 elemsize, string* (*f)(i64)) {

    if (depth == 0)
        return f(ptr);
    
    if ((void*) ptr == NULL)
        return makenullstr();

    string* res = (string*) _allocate(sizeof(string));
    res->size = 1;

    blindarr *a = (blindarr*) ptr;
    string** arrvalstrs = (string**) _allocate(a->size * sizeof(string));

    for (i64 i = 0; i < a->size; i++) {
        arrvalstrs[i] = _sprintf_array(* (i64*) (a->base + (depth == 1 ? elemsize : sizeof(i64*)) * i), depth - 1, elemsize, f);
        res->size += arrvalstrs[i]->size;
    }

    res->size += 2 + (a->size == 0 ? 0 : a->size - 1);
    res->capacity = res->size;

    res->base = _allocate(res->size);
    _addchild((i8*) res, res->base);
    _removeref(res->base);

    res->base[0] = '[';

    i64 index = 1;
    for (i64 i = 0; i < a->size; i++) {
        memcpy(res->base + index, arrvalstrs[i]->base, arrvalstrs[i]->size);
        index += arrvalstrs[i]->size;
        if (i + 1 < a->size)
            res->base[index++] = ',';
    }

    res->base[index] = ']';
    res->base[index+1] = '\0';
    
    for (i64 i = 0; i < a->size; i++)
        _removeref((i8*) arrvalstrs[i]);

    _removeref((i8*) arrvalstrs);

    return res;
}



string* _sprintf_cat(i64 size, ...) {
    i64 sumlen = 0;
    va_list va;
    va_start(va, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(va, string*);
        sumlen += str->size;
    }
    va_end(va);

    string *res = _allocate_string(sumlen);
    i64 index = 0;
    va_list vb;
    va_start(vb, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(vb, string*);
        memcpy(res->base + index, str->base, str->size);
        index += str->size;
    }
    va_end(vb);

    return res;
}
