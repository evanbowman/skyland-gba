#include "room.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"



namespace skyland {


Room::Room(Island* parent,
         const char* name,
         const Vec2<u8>& size,
         const Vec2<u8>& position,
         Health health)
        : parent_(parent),
          size_(size),
          position_(position),
          health_(health)
{
    auto metatable = room_metatable();

    for (int i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_cmp(name, current->name()) == 0) {
            metaclass_ = &current;
        }
    }
}



}
