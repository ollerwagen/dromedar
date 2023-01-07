#include "common.h"
#include "cpputils/Regex.h"

i8* _Regex$compile(IGNORE, string* s) {
    return _cpputils_Regex$compile(s);
}

i1 _Regex$matches(IGNORE, i8* r, string* s) {
    return _cpputils_Regex$matches(r, s);
}