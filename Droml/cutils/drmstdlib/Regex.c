#include "common.h"
#include "cpputils/Regex.h"

i8* _Regex$compile(IGNORE, string* s) {
    return _cpputils_Regex$compile(s);
}

i1 _Regex$matches(IGNORE, i8* r, string* s) {
    return _cpputils_Regex$matches(r, s);
}

string* _Regex$first_match(IGNORE, i8* r, string* s) {
    return _cpputils_Regex$first_match(r, s);
}

stringarr* _Regex$all_matches(IGNORE, i8* r, string* s) {
    return _cpputils_Regex$all_matches(r, s);
}