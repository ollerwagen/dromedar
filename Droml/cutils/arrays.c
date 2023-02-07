#include <stdio.h>
#include <stdlib.h>

#include "arrays.h"

static inline void write(i8* base, i64 elemsize, i64 flag, i64 index, i64 elem) {
    * (i64*) (base + elemsize * index) &= ~flag;
    * (i64*) (base + elemsize * index) |= elem;
}

blindarr* _array_push(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 elem) {
    if (a->capacity <= a->size) {
        i64 newcapacity = 2 * a->capacity + 1;
        i8* newbase = _allocate(newcapacity * elemsize);
        memcpy(newbase, a->base, a->capacity * elemsize);
        _removechild((i8*) a, a->base);
        a->capacity = newcapacity;
        a->base = newbase;
        _addchild((i8*) a, a->base);
        _removeref(newbase);
    }

    write(a->base, elemsize, sizeflag, a->size, elem);

    if (isptrs)
        _addchild((i8*) a, (i8*) elem);
    a->size++;

    _addref((i8*) a);
    return a;
}

blindarr* _array_pop(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a) {
    if (a->size == 0) {
        fprintf(stderr, "Error in call to pop(): Array is already empty.\n");
        exit(1);
    }

    a->size--;
    if (isptrs)
        _removechild((i8*) a, * (i8**) (a->base + a->size * elemsize));
    
    _addref((i8*) a);
    return a;
}

blindarr* _array_insert(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index, i64 val) {
    if (index < 0 || index >= a->size) {
        fprintf(stderr, "Index %ld out of bounds for array size %ld.\n", index, a->size);
        exit(1);
    }

    if (a->capacity <= a->size) {
        i64 newcapacity = 2 * a->capacity + 1;
        i8* newbase = _allocate(newcapacity * elemsize);
        memcpy(newbase, a->base, index * elemsize);
        write(newbase, elemsize, sizeflag, index, val);
        memcpy(newbase + (index + 1) * elemsize, a->base + index * elemsize, (a->size - index) * elemsize);
        _removechild((i8*) a, a->base);
        a->capacity = newcapacity;
        a->base = newbase;
        _addchild((i8*) a, a->base);
    } else {
        i8* moved_elems = malloc((a->size - index) * elemsize);
        memcpy(moved_elems, a->base + index * elemsize, (a->size - index) * elemsize);
        write(a->base, elemsize, sizeflag, index, val);
        memcpy(a->base + (index + 1) * elemsize, moved_elems, (a->size - index) * elemsize);
        free(moved_elems);
    }

    if (isptrs)
        _addchild((i8*) a, (i8*) val);
    a->size++;

    _addref((i8*) a);
    return a;
}

blindarr* _array_insert_all(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index, blindarr* vals) {
    if (index < 0 || index > a->size) {
        fprintf(stderr, "Index %ld out of bounds for array size %ld\n", index, a->size);
        exit(1);
    }

    if (a->size + vals->size > a->capacity) {
        i64 newcapacity = 2 * a->capacity + vals->size + 1;
        i8* newbase = _allocate(newcapacity * elemsize);
        memcpy(newbase, a->base, index * elemsize);
        memcpy(newbase + index * elemsize, vals->base, vals->size * elemsize);
        memcpy(newbase + (index + vals->size) * elemsize, a->base + index * elemsize, (a->size - index) * elemsize);
        _removechild((i8*) a, a->base);
        a->capacity = newcapacity;
        a->base = newbase;
        _addchild((i8*) a, a->base);
    } else {
        i8* moved_elems = malloc((a->size - index) * elemsize);
        memcpy(moved_elems, a->base + index * elemsize, (a->size - index) * elemsize);
        memcpy(a->base + index * elemsize, vals->base, vals->size * elemsize);
        memcpy(a->base + (index + vals->size) * elemsize, moved_elems, (a->size - index) * elemsize);
        free(moved_elems);
    }

    if (isptrs)
        for (i64 i = 0; i < vals->size; i++)
            _addchild((i8*) a, * (i8**) (vals->base + i * elemsize));

    a->size += vals->size;

    _addref((i8*) a);
    return a;
}

blindarr* _array_erase(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index) {
    if (index < 0 || index >= a->size) {
        fprintf(stderr, "Index %ld out of bounds in erase().\n", index);
        exit(1);
    }

    if (isptrs)
        _removechild((i8*) a, a->base + index * elemsize);

    i8* moved_elems = malloc(a->size + (index + 1) * elemsize);
    memcpy(moved_elems, a->base + (index + 1) * elemsize, (a->size - index) * elemsize);
    memcpy(a->base + index * elemsize, moved_elems, (a->size - index) * elemsize);
    free(moved_elems);

    a->size--;

    _addref((i8*) a);
    return a;
}

blindarr* _array_sub(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 start, i64 length) {
    if (start < 0 || length < 0 || start + length >= a->size) {
        fprintf(stderr, "Index pair (start=%ld,length=%ld) invalid for array with size %ld in sub().\n", start, length, a->size);
        exit(1);
    }

    blindarr* res = _allocate_blindarr(length, elemsize);
    memcpy(res->base, a->base + start * elemsize, length * elemsize);
    if (isptrs)
        for (i64 i = 0; i < length; i++)
            _addchild((i8*) res, * (i8**) (res->base + i * elemsize));
    
    return res;
}
