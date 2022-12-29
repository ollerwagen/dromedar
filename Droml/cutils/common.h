#ifndef __COMMON__
#define __COMMON__


#include <stdbool.h>
#include <stdint.h>


typedef int64_t i64;
typedef char    i8;
typedef bool    i1;

typedef struct string { i64 size; i8 *base; } string;
typedef struct blindarr { i64 size; i8 *base; } blindarr;
typedef struct intarr { i64 size; i64 *base; } intarr;

string* allocate_string(i64 size);
intarr* allocate_intarr(i64 size);


#endif // __COMMON__
