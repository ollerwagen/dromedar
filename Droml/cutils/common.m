inline blindarr* _allocate_blindarr(i64 elems, i64 elemsize) {
    blindarr* res = (blindarr*) _allocate(sizeof(blindarr));
    res->capacity = res->size = elems;
    res->base = _allocate(elems * elemsize);
    _addchild((i8*) res, res->base);
    _removeref(res->base);
    return res;
}

string* _allocate_string(i64 size) {
    return (string*) _allocate_blindarr(size, 1);
}

string* _allocate_string_of(const char *c) {
    int len = strlen(c);
    string* res = _allocate_string(len);
    memcpy(res->base, c, len);
    return res;
}

intarr* _allocate_intarr(i64 size) {
    return (intarr*) _allocate_blindarr(size, sizeof(i64));
}