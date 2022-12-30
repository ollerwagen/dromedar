#ifndef __SPRINTFLIB__
#define __SPRINTFLIB__

#include "common.h"

string* _sprintf_int(i64 i);
string* _sprintf_flt(i64 d);
string* _sprintf_char(i64 c);
string* _sprintf_bool(i64 b);
string* _sprintf_array(i64 ptr, i64 depth, i64 elemsize, string* (*f)(i64));
string* _sprintf_cat(i64 size, ...);

#endif // __SPRINTFLIB__
