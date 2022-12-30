#include <stdlib.h>
#include <vector>

#include "gc.h"
#include "listops.h"

// debug
#include <stdio.h>

extern "C" {

    i8* _make_vector() {
        i8* res = (i8*) malloc(sizeof(list));
        return res;
    }

    void _addelem(i8 *l, i64 elem) {
        list* vl = (list*) l;
        vl->push_back(elem);
    }

    blindarr* _genlist(i8 *l, i8 *childrenbuf, i64 elemsize, i1 addchildren) {
        list* vl = (list*) l;
        blindarr* res = (blindarr*) _allocate(sizeof(blindarr));
        res->size = vl->size();
        res->base = (i8*) _allocate(elemsize * vl->size());
        _addchild((i8*) res, (i8*) res->base);
        _removeref((i8*) res->base);
        for (i64 i = 0; i < res->size; i++) {
            * (i64*) (res->base + i * elemsize) = vl->at(i);
        }
        if (addchildren)
            _transferchildren(childrenbuf, (i8*) res);
        return res;
    }
}