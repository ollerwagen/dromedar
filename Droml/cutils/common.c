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
    intarr* res = (intarr*) _allocate(sizeof(intarr));
    res->size = size;
    res->base = (i64*) _allocate(sizeof(i64) * size);
    _addchild((i8*) res, (i8*) res->base);
    _removeref((i8*) res->base);
    return res;
}

blindarr* allocate_blindarr(i64 elems, i64 elemsize) {
    blindarr* res = (blindarr*) _allocate(sizeof(blindarr));
    res->size = elems;
    res->base = _allocate(elems * elemsize);
    _addchild((i8*) res, res->base);
    _removeref(res->base);
    return res;
}