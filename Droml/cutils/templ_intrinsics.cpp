#include "templ_intrinsics.h"

#include "common.m"

template<typename T, typename U>
inline T max(T a, U b) {
    return a > b ? a : b;
}

template<typename T>
static inline blindarr* _makerangelist_basic(T start, T end, bool inclstart, bool inclend) {
    if (start < end) {
        if (!inclstart) ++start;
        if (!inclend) --end;
        blindarr* res = _allocate_blindarr(max(end - start + 1, 0), sizeof(T));
        for (i64 i = 0; i < res->size; i++) {
            * (T*) (res->base + sizeof(T) * i) = (start + i);
        }
        return res;
    } else {
        if (!inclstart) --start;
        if (!inclend) ++end;
        blindarr* res = _allocate_blindarr(max(start - end + 1, 0), sizeof(T));
        for (i64 i = res->size - 1; i >= 0; i--) {
            * (T*) (res->base + sizeof(T) * i) = (start - i);
        }
        return res;
    }
}

extern "C" {

    chararr* _cpp_rangechar(i8 start, i8 end, i1 inclstart, i1 inclend) {
        return (chararr*) _makerangelist_basic(start, end, inclstart, inclend);
    }

    intarr*  _cpp_rangeint(i64 start, i64 end, i1 inclstart, i1 inclend) {
        return (intarr*) _makerangelist_basic(start, end, inclstart, inclend);
    }

}