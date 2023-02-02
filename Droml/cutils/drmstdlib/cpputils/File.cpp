#include <fstream>
#include <string>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cppallocator.h"

#include "File.h"

#include "common.m"

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

        strlist* res = (strlist*) _allocate_blindarr(strvec.size(), sizeof(string*));

        for (i64 i = 0; i < res->size; i++) {
            res->base[i] = _allocate_string(strvec.at(i).length());
            memcpy(res->base[i]->base, strvec.at(i).c_str(), res->base[i]->size);
        }

        return res;
    }
}