#define __DEBUG__ 1



#include <vector>
#include <unordered_set>
#include <unordered_map>

// malloc and free
#include <cstdlib>
#include <cstdarg>



#include "gc.h"


struct GCEntry {
    std::unordered_set<ptr> children;
    uint16_t prefs = 0;
    bool reachable;
};

using GCTable = std::unordered_map<ptr, GCEntry>;


static GCTable table;


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

static void gcrun() {

    // printf("Before the GC Run:\n%s\n", print_table().c_str());

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
    printf("After the GC Run:\n%s\n", print_table().c_str());
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

        gcrun();
    }

    ptr _allocate(size s) {
#if __DEBUG__
        printf("allocate(%llu)\n", s);
#endif

        ptr res = (ptr)malloc(s);
        table[res].prefs++;

        // need better heuristics than just calling it every time
        // stress testing -> find bugs by running it on every allocate()
        gcrun();

        return res;
    }

    void _addref(ptr p) {
#if __DEBUG__
        printf("addref(%p)\n", p);
#endif

        table[p].prefs++;
        gcrun();
    }

    void _addchild(ptr base, ptr child) {
#if __DEBUG__
        printf("addchild(%p -> %p)\n", base, child);
#endif

        table[base].children.insert(child);
        gcrun();
    }

}