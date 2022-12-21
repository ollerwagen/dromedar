#include <vector>
#include <unordered_map>

// malloc and free
#include <cstdlib>
// variadics
#include <cstdarg>

template<typename K, typename V>
using HashMap = std::unordered_map<K, V>;

using Addr = void*;

struct GCEntry {
    int16_t prefs, irefs;
    std::vector<Addr> children;
    bool reachable;
};

using GCTable = HashMap<Addr, GCEntry>;


static GCTable table;


static void mark_reachable(GCEntry &ge) {
    if (ge.reachable)
        return; // has alreaddy been marked

    ge.reachable = true;
    for (std::size_t i = 0; i < ge.children.size(); i++)
        mark_reachable(table[ge.children.at(i)]);
}

void gcrun() {
    // wipe all reachability info
    for (auto &it : table)
        it.second.reachable = false;

    // mark only reachable objects
    for (auto &it : table) {
        if (it.second.prefs > 0)
            mark_reachable(it.second);
    }

    for (auto &it : table) {
        if (!it.second.reachable)
            free(it.first);
    }
}

#include <string>
#include <sstream>
#include <iomanip>
#include <iostream>

std::string print_table() {
    std::stringstream stream;
    for (const auto &it : table) {
        stream << it.first << " -> " << std::setw(3) << it.second.prefs << std::setw(3) << it.second.irefs << " -> ";
        for (const Addr a : it.second.children)
            stream << a << ", ";
        stream << "\n";
    }
    return stream.str();
}

void drmfree(Addr a) {
    table[a].prefs--;
}

Addr drmalloc(std::size_t size) {
    Addr res = malloc(size);
    table[res].prefs++;
    return res;
}



int main() {

    Addr a = drmalloc(10);
    Addr b = drmalloc(20);

    drmfree(a);

    std::cout << print_table() << std::endl;

    return 0;
}