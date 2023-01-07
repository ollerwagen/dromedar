#include "common.h"
#include "cpputils/File.h"


strlist* _File$readall(IGNORE, string* filename) {
    return _cpputils_File$readall(filename);
}