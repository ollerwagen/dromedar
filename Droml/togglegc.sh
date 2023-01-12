#!/usr/bin/bash

read -r l1 < cutils/gc.cpp
c="${l1: -1}"

if [ "$c" = "0" ]; then
    sed -i "1s/.*/#define __DEBUG__ 1/" cutils/gc.cpp
    sed -i "2s/.*/#define __AGGRESSIVE_GC__ 1/" cutils/gc.cpp
else
    sed -i "1s/.*/#define __DEBUG__ 0/" cutils/gc.cpp
    sed -i "2s/.*/#define __AGGRESSIVE_GC__ 0/" cutils/gc.cpp
fi

make gc