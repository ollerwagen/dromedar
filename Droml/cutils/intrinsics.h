#ifndef __INTRINSICS__
#define __INTRINSICS__

#include "common.h"

i64 _pow_ii(i64 base, i64 exp);
double _pow_ff(double base, double exp);

void _memcpy(i8 *from, i8 *to, i64 size);

string* _strconcat(string *a, string *b);
string* _strmul_1(string *s, i64 count);
string* _strmul_2(i64 count, string *s);
i64 _strcmp(string *a, string *b);

blindarr* _arrconcat(blindarr *a, blindarr *b, i64 elemsize, i1 areptrs);

intarr* _makerangelist(i64 start, i64 end, bool inclstart, bool inclend);

#endif // __INTRINSICS__