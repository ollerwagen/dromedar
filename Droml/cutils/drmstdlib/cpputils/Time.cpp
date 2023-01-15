#include <chrono>
#include <thread>

#include <stdio.h>

#include "cppallocator.h"
#include "gc.h"

#include "Time.h"

typedef std::chrono::duration<i64, std::nano> D;
typedef std::chrono::time_point<std::chrono::system_clock> P;

extern "C" {

    i8* _cpputils_Time$now() {
        P* res = (P*) _allocate(sizeof(P));
        *res = std::chrono::system_clock::now();
        return (i8*) res;
    }

    i8* _cpputils_Time$dt(i8* a, i8* b) {
        P *atp = (P*) a, *btp = (P*) b;
        D* res = (D*) _allocate(sizeof(D));
        // new (res) D(std::chrono::duration_cast<D>(*atp - *btp));
        *res = *atp - *btp;
        return (i8*) res;
    }

    void _cpputils_Time$sleep_for(i8* d) {
        std::this_thread::sleep_for(* (D*) d);
    }

    i64 _cpputils_Time$s(i8* d) {
        return std::chrono::duration_cast<std::chrono::seconds>(* (D*) d).count();
    }

    i64 _cpputils_Time$ms(i8* d) {
        return std::chrono::duration_cast<std::chrono::milliseconds>(* (D*) d).count();
    }

    i64 _cpputils_Time$us(i8* d) {
        return std::chrono::duration_cast<std::chrono::microseconds>(* (D*) d).count();
    }

    i8* _cpputils_Time$of_s(i64 i) {
        D* res = (D*) _allocate(sizeof(D));
        new (res) D();
        *res = std::chrono::nanoseconds(i * 1000000000L);
        return (i8*) res;
    }

    i8* _cpputils_Time$of_ms(i64 i) {
        D* res = (D*) _allocate(sizeof(D));
        new (res) D(std::chrono::nanoseconds(i * 1000000L));
        return (i8*) res;
    }

    i8* _cpputils_Time$of_us(i64 i) {
        D* res = (D*) _allocate(sizeof(D));
        new (res) D(std::chrono::nanoseconds(i * 1000L));
        return (i8*) res;
    }
}