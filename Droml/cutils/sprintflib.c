#include <stdarg.h>
#include <stdio.h>
#include <string.h>


#include "sprintflib.h"


string* _sprintf_int(i64 i) {
    string* res = allocate_string(30);
    sprintf(res->base, "%ld", i);
    res->size = strlen(res->base) + 1;
    return res;
}

string* _sprintf_flt(double d) {
    string* res = allocate_string(50);
    sprintf(res->base, "%lf", d);
    res->size = strlen(res->base) + 1;
    return res;
}

string* _sprintf_char(i8 c) {
    string* res = allocate_string(2);
    sprintf(res->base, "%c", c);
    return res;
}

string* _sprintf_bool(i1 b) {
    string* res = allocate_string(b ? 5 : 6);
    sprintf(res->base, "%s", b ? "true" : "false");
    return res;
}



string* _sprintf_cat(i64 size, ...) {
    i64 sumlen = 0;
    va_list va;
    va_start(va, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(va, string*);
        sumlen += str->size - 1;
    }
    va_end(va);

    string *res = allocate_string(sumlen + 1);
    i64 index = 0;
    va_list vb;
    va_start(vb, size);
    for (i64 i = 0; i < size; i++) {
        string* str = va_arg(vb, string*);
        strcpy(res->base + index, str->base);
        index += str->size - 1;
    }
    va_end(vb);
    return res;
}
