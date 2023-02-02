#ifndef __COMMON__
#define __COMMON__

#define IGNORE i64 *ignore

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "gc.h"


typedef int64_t i64;
typedef char    i8;
typedef bool    i1;

typedef struct string   { i64 size; i64 capacity; i8 *base; }  string;
typedef struct blindarr { i64 size; i64 capacity; i8 *base; }  blindarr;
typedef struct intarr   { i64 size; i64 capacity; i64 *base; } intarr;
typedef blindarr chararr;

typedef struct stringarr { i64 size; i64 capacity; string** base; } stringarr;

string*   _allocate_string(i64 size);
string*   _allocate_string_of(const char *c);
intarr*   _allocate_intarr(i64 size);
blindarr* _allocate_blindarr(i64 elems, i64 elemsize);


#endif // __COMMON__
