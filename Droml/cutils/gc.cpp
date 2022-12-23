#include <vector>
#include <unordered_map>

// malloc and free
#include <cstdlib>
#include <cstdarg>



#include "gc.h"


struct GCEntry {
    std::vector<ptr> children;
    uint16_t prefs = 0;
    bool reachable;
};

using GCTable = std::unordered_map<ptr, GCEntry>;


static GCTable table;


static void mark_reachable(GCEntry &ge) {
    if (ge.reachable)
        return; // has alreaddy been marked

    ge.reachable = true;
    for (std::size_t i = 0; i < ge.children.size(); i++)
        mark_reachable(table[ge.children.at(i)]);
}

#include <string>
#include <sstream>
#include <iomanip>
#include <iostream>

static std::string print_table() {
    std::stringstream stream;
    stream << "BASE               PREFS    CHILDREN\n";
    for (const auto &it : table) {
        stream << "0x" << std::setw(16) << std::setfill('0') << std::hex << reinterpret_cast<int64_t>(it.first)
            << std::dec << std::setfill(' ') << " -> " << std::setw(6) << it.second.prefs << " -> ";
        for (const ptr a : it.second.children)
            stream << a << ", ";
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

    // printf("After the GC Run:\n%s\n", print_table().c_str());
}


// functions called by the Dromedar executable


extern "C" {

    void _removeref(ptr p) {
        table[p].prefs--;
    }

    ptr _allocate(size s) {
        ptr res = (ptr)malloc(s);
        table[res].prefs++;

        // need better heuristics than just calling it every time
        // stress testing -> find bugs by running it on every allocate()
        gcrun();

        return res;
    }

    void _addchild(ptr base, ptr child) {
        table[base].children.push_back(child);
    }

}


/*
int main() {

    Addr a = drmalloc(1000);
    Addr b = drmalloc(2000);
    Addr c = drmalloc(5000);

    drmaddchild(a, b);
    drmaddchild(b, c);
    drmaddchild(c, a);

    drmfree(b);
    drmfree(c);

    gcrun(); // this gc run should not remove anything

    std::cout << print_table() << std::endl;

    drmfree(a);

    gcrun(); // this gc run should remove a, b and c (clear the table)

    std::cout << print_table() << std::endl;

    return 0;
}*/