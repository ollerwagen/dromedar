#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "gc.h"

i64 _Sys$cmd(IGNORE, string* cmd) {
    return system(cmd->base);
}

i64 _Sys$fork(IGNORE) {
    return fork();
}