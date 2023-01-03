#include <regex>

#include "gc.h"

#include "Regex.h"

extern "C" {

    i8* _cpputils_Regex$compile(string* rstr) {
        try {
            std::regex *res = (std::regex*) _allocate(sizeof(std::regex));
            new (res) std::regex(rstr->base);
            return (i8*) res;
        } catch (std::regex_error &e) {
            return nullptr;
        }
    }

    i1  _cpputils_Regex$matches(i8 *r, string* s) {
        return std::regex_match(s->base, * (std::regex*) r);
    }
}