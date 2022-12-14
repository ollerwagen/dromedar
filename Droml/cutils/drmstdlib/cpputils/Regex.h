#ifndef __STDLIB_REGEX_C_CONNECTOR__
#define __STDLIB_REGEX_C_CONNECTOR__

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

    i8* _cpputils_Regex$compile(string* rstr);
    i1  _cpputils_Regex$matches(i8 *r, string* s);

    string* _cpputils_Regex$first_match(i8* r, string* s);
    stringarr* _cpputils_Regex$all_matches(i8* r, string* s);

#ifdef __cplusplus
}
#endif

#endif // __STDLIB_REGEX_C_CONNECTOR__
