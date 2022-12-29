#include "common.h"
#include "gc.h"


string* allocate_string(i64 size) {
    string* res = (string*) _allocate(sizeof(string));
    res->size = size;
    res->base = _allocate(size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);
    return res;
}

intarr* allocate_intarr(i64 size) {
    intarr* res = (intarr*) _allocate(sizeof(i64) * size);
    res->size = size;
    res->base = (i64*) _allocate(size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);
    return res;
}
