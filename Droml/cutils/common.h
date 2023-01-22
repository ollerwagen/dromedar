#ifndef __COMMON__
#define __COMMON__

#define IGNORE i64 *ignore

#include <stdbool.h>
#include <stdint.h>

#include "gc.h"


typedef int64_t i64;
typedef char    i8;
typedef bool    i1;

typedef struct string { i64 size; i8 *base; } string;
typedef struct blindarr { i64 size; i8 *base; } blindarr;
typedef struct intarr { i64 size; i64 *base; } intarr;
typedef blindarr chararr;

typedef struct stringarr { i64 size; string** base; } stringarr;

string* allocate_string(i64 size);
intarr* allocate_intarr(i64 size);
blindarr* allocate_blindarr(i64 elems, i64 elemsize);


#endif // __COMMON__
