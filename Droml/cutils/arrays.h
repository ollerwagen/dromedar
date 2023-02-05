#ifndef __ARRAYS__
#define __ARRAYS__

#include "common.h"
#include "gc.h"

blindarr* _array_push(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 elem);
blindarr* _array_pop(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a);

blindarr* _array_insert(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index, i64 val);
blindarr* _array_insert_all(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index, blindarr* vals);

blindarr* _array_erase(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 index);

blindarr* _array_sub(IGNORE, i64 elemsize, i64 sizeflag, i1 isptrs, blindarr* a, i64 start, i64 length);

#endif // __ARRAYS