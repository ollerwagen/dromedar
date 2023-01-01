#ifndef __LISTOPS_CONNECTOR__
#define __LISTOPS_CONNECTOR__

#include <stdbool.h>
#include <stdint.h>
#include <vector>

#include "cppallocator.h"

typedef int64_t i64;

typedef std::vector<i64, drm::allocator<i64>> list;

#ifdef __cplusplus
extern "C" {
#endif

    typedef bool i1;
    typedef char i8;
    typedef int64_t i64;
    typedef struct blindarr { i64 size; i8* base; } blindarr;

    i8* _make_vector();
    void _addelem(i8 *l, i64 elem);

    // also frees the object l
    blindarr* _genlist(i8 *l, i64 elemsize, i1 addchildren);

#ifdef __cplusplus
}
#endif

#endif // __LISTOPS_CONNECTOR__