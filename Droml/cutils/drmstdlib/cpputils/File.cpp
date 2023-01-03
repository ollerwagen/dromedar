#include <fstream>
#include <string>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cppallocator.h"

#include "File.h"

extern "C" {

    strlist* _cpputils_File$readall(string* filename) {
        std::ifstream file(filename->base);
        if (!file) {
            fprintf(stderr, "could not open file %s\n", filename->base);
            exit(1);
        }

        using String = std::basic_string<char, std::char_traits<char>, drm::allocator<char>>;

        std::vector<String, drm::allocator<String>> strvec;
        while (!file.eof()) {
            String tmp;
            std::getline(file, tmp);
            strvec.push_back(tmp);
        }

        file.close();

        strlist* res = (strlist*) _allocate(sizeof(strlist));

        res->size = strvec.size();
        res->base = (string**) _allocate(res->size * sizeof(string*));
        _addchild((i8*) res, (i8*) res->base);
        _removeref((i8*) res->base);

        for (i64 i = 0; i < res->size; i++) {
            res->base[i] = (string*) _allocate(sizeof(string*));
            _addchild((i8*) res, (i8*) res->base[i]);
            _removeref((i8*) res->base[i]);

            res->base[i]->size = strvec.at(i).length() + 1;
            res->base[i]->base = _allocate(res->base[i]->size);
            _addchild((i8*) res->base[i], res->base[i]->base);
            _removeref(res->base[i]->base);
            
            strcpy(res->base[i]->base, strvec.at(i).c_str());
        }

        return res;
    }
}