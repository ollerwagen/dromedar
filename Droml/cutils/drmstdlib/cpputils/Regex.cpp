#include <regex>
#include <string>

#include "cppallocator.h"
#include "gc.h"

#include "Regex.h"

#include "common.m"

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

    string* _cpputils_Regex$first_match(i8* r, string* s) {
        if (!std::regex_search(s->base, * (std::regex*) r))
            return NULL;
    
        std::string match = std::cregex_iterator(s->base, s->base + s->size, * (std::regex*) r)->str();

        string* res = _allocate_string(match.length());
        memcpy(res->base, match.c_str(), res->size);

        return res;
    }

    stringarr* _cpputils_Regex$all_matches(i8* r, string* s) {
        std::cregex_iterator it = std::cregex_iterator(s->base, s->base + s->size, * (std::regex*) r);

        stringarr* res = (stringarr*) _allocate_blindarr(std::distance(it, std::cregex_iterator()), sizeof(string*));

        for (i64 i = 0; i < res->size; i++, ++it) {
            string* match = _allocate_string(it->str().length());
            memcpy(match->base, it->str().c_str(), match->size);
            res->base[i] = match;
            _addchild((i8*) res, (i8*) res->base[i]);
            _removeref((i8*) match);
        }
        
        return res;
    }
}