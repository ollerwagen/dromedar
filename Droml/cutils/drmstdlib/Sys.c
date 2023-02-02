#include <stdlib.h>
#include <unistd.h>
#include <string.h> // for memcpy

#include "common.h"
#include "gc.h"

i64 _Sys$cmd(IGNORE, string* cmd) {
    string* cmd2 = _allocate_string(cmd->size + 1);
    memcpy(cmd2->base, cmd->base, cmd->size);
    cmd2->base[cmd->size] = '\0';
    i64 res = system(cmd->base);
    _removeref((i8*) cmd2);
    return res;
}

i64 _Sys$fork(IGNORE) {
    return fork();
}