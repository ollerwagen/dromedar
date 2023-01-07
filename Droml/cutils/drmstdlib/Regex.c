#include "common.h"
#include "cpputils/Regex.h"

i8* __Regex$compile(IGNORE, string* s) {
    return _cpputils_Regex$compile(s);
}

i1 __Regex$matches(IGNORE, i8* r, string* s) {
    return _cpputils_Regex$matches(r, s);
}