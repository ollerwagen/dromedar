#include <regex>
#include <string>

#include "cppallocator.h"
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

    string* _cpputils_Regex$first_match(i8* r, string* s) {
        if (!std::regex_search(s->base, * (std::regex*) r))
            return NULL;
    
        std::string match = std::cregex_iterator(s->base, s->base + s->size, * (std::regex*) r)->str();

        string* res = (string*) _allocate(sizeof(string));
        res->size = match.length();
        res->base = _allocate(res->size);
        _addchild((i8*) res, res->base);
        _removeref(res->base);

        memcpy(res->base, match.c_str(), res->size);

        return res;
    }

    stringarr* _cpputils_Regex$all_matches(i8* r, string* s) {
        stringarr* res = (stringarr*) _allocate(sizeof(stringarr));

        std::cregex_iterator it = std::cregex_iterator(s->base, s->base + s->size, * (std::regex*) r);

        res->size = std::distance(it, std::cregex_iterator());
        res->base = (string**) _allocate(res->size * sizeof(string*));
        _addchild((i8*) res, (i8*) res->base);
        _removeref((i8*) res->base);

        for (i64 i = 0; i < res->size; i++, ++it) {
            string* match = (string*) _allocate(sizeof(string));
            match->size = it->str().length();
            match->base = _allocate(match->size);
            _addchild((i8*) match, match->base);
            _removeref((i8*) match->base);
            memcpy(match->base, it->str().c_str(), match->size);
            res->base[i] = match;
            _addchild((i8*) res, (i8*) res->base[i]);
            _removeref((i8*) match);
        }
        
        return res;
    }
}