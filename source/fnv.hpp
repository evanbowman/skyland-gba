#pragma once

#include "number/int.h"



inline u32 fnv32(const char* data, u32 len)
{
    u32 hash = 2166136261U, i;

    for (i = 0; i < len; i++) {
        hash = hash ^ (data[i]);
        hash = hash * 16777619;
    }

    return hash;
}
