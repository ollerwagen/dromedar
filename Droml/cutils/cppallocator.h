#ifndef __CPP_ALLOCATOR__
#define __CPP_ALLOCATOR__

#include <stdlib.h>

#include "gc.h"

namespace drm {

    template<class T>
    struct allocator {
        typedef T value_type;

        allocator() = default;

        template<class U>
        constexpr allocator(const allocator <U>&) noexcept {}

        T* allocate(std::size_t n) {
            return (T*) malloc(n * sizeof(T));
            // return (T*) _allocate(n * sizeof(T));
        }
    
        void deallocate(T* p, std::size_t n) noexcept {
            free(p);
            // _removeref((ptr) p);
        }
    };

    template<class T, class U>
    bool operator==(const allocator <T>&, const allocator <U>&) { return true; }

    template<class T, class U>
    bool operator!=(const allocator <T>&, const allocator <U>&) { return false; }

} // namespace drm

#endif // __CPP_ALLOCATOR__