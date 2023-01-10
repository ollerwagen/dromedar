#define __DEBUG__         1
#define __AGGRESSIVE_GC__ 1


#include <vector>
#include <unordered_set>
#include <unordered_map>

// malloc and free
#include <cstdlib>
#include <cstdarg>


#include "cppallocator.h"
#include "gc.h"


struct GCEntry {
    std::unordered_set<ptr, std::hash<ptr>, std::equal_to<ptr>, drm::allocator<ptr>> children;
    uint16_t prefs = 0;
    bool reachable;
};

using GCTable = std::unordered_map<ptr, GCEntry, std::hash<ptr>, std::equal_to<ptr>, drm::allocator<std::pair<const ptr, GCEntry>>>;


static GCTable table;

static int64_t used_memory = 0;
static int64_t next_gc = 0x10000;


static void mark_reachable(GCEntry &ge) {
    if (ge.reachable)
        return; // has already been marked

    ge.reachable = true;
    for (const auto &u : ge.children)
        mark_reachable(table[u]);
}

#include <string>
#include <sstream>
#include <iomanip>
#include <iostream>

#if __DEBUG__
    static std::string print_table() {
        std::stringstream stream;
        stream << "BASE                   PREFS    CHILDREN\n";
        for (const auto &it : table) {
            stream << "0x" << std::setw(16) << std::setfill('0') << std::hex << reinterpret_cast<int64_t>(it.first)
                << std::dec << std::setfill(' ') << " -> " << std::setw(6) << it.second.prefs << " -> ";
            for (const ptr a : it.second.children)
                stream << "0x" << std::setw(16) << std::setfill('0') << std::hex << reinterpret_cast<int64_t>(a) << std::dec << std::setfill(' ') << ", ";
            stream << "\n";
        }
        return stream.str();
    }
#endif

static void gcrun() {
    
    // printf("Before the GC Run:\n%s\n", print_table().c_str());

#if __DEBUG__
    int64_t prev_size = table.size();
#endif

    // wipe all reachability info
    for (auto &it : table)
        it.second.reachable = false;

    // mark only reachable objects
    for (auto &it : table) {
        if (it.second.prefs > 0)
            mark_reachable(it.second);
    }

    for (auto it = table.begin(); it != table.end(); ) {
        if (!it->second.reachable) {
            free(it->first);
            it = table.erase(it);
        } else {
            ++it;
        }
    }

#if __DEBUG__
    printf("After GC Run: %ld freed objects, new %ld objects in table\n", prev_size - table.size(), table.size());
    // printf("After the GC Run:\n%s\n", print_table().c_str());
#endif
}


// functions called by the Dromedar executable


extern "C" {

    void _removeref(ptr p) {
#if __DEBUG__
        printf("removeref(%p)\n", p);
#endif

        if (p == NULL)
            return;
        table[p].prefs--;

#if __AGGRESSIVE_GC__
        gcrun();
#endif
    }

    ptr _allocate(size s) {
#if __DEBUG__
        printf("allocate(%llu)\n", s);
#endif

        if (used_memory >= next_gc) {
            used_memory = 0;
            gcrun();
        }

        ptr res = (ptr) malloc(s);
        table[res].prefs++;

#if __AGGRESSIVE_GC__
        gcrun();
#else
        used_memory += s;

#endif

        return res;
    }

    void _addref(ptr p) {
#if __DEBUG__
        printf("addref(%p)\n", p);
#endif

        if (p == NULL)
            return;
        table[p].prefs++;

#if __AGGRESSIVE_GC__
        gcrun();
#endif
    }

    void _addchild(ptr base, ptr child) {
#if __DEBUG__
        printf("addchild(%p -> %p)\n", base, child);
#endif

        table[base].children.insert(child);

#if __AGGRESSIVE_GC__
        gcrun();
#endif
    }

    void _transferchildren(ptr from, ptr to) {
#if __DEBUG__
        printf("transferchildren(%p -> %p)\n", from, to);
#endif 

        const auto &children = table[from].children;
        table[to].children.insert(children.begin(), children.end());
        table[from].children.clear();

#if __AGGRESSIVE_GC__
        gcrun();
#endif
    }
}