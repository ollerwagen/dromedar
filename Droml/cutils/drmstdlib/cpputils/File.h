#ifndef __STDLIB_FILE_C_CONNECTOR__
#define __STDLIB_FILE_C_CONNECTOR__

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct { i64 size; i64 capacity; string** base; } strlist;

    strlist* _cpputils_File$readall(string* filename);

#ifdef __cplusplus
}
#endif

#endif // __STDLIB_FILE_C_CONNECTOR__
