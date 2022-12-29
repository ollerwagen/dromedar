#ifndef __SPRINTFLIB__
#define __SPRINTFLIB__

#include "common.h"

string* _sprintf_int(i64 i);
string* _sprintf_flt(double d);
string* _sprintf_char(i8 c);
string* _sprintf_bool(i1 b);
string* _sprintf_cat(i64 size, ...);

#endif // __SPRINTFLIB__
