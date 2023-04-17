#pragma once

#include "number/int.h"



typedef struct SharedMetaclass
{
    u8 metaclass_index_;
    u8 size_x_;
    u8 size_y_;
    char name_[20];
    u8 tiles_[16];
} SharedMetaclass;



typedef struct SharedRoomMetatable
{

#define SHARED_MT_COUNT 32

    SharedMetaclass metaclasses_[SHARED_MT_COUNT];
    u8 metaclass_count_;
} SharedRoomMetatable;
