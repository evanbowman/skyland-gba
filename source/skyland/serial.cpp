#include "serial.hpp"
#include "island.hpp"
#include "room_metatable.hpp"



namespace skyland {



DynamicMemory<SerialString> serialize(Platform& pfrm, Island& island)
{
    auto str = allocate_dynamic<SerialString>(pfrm);
    if (not str) {
        return str;
    }

    str->push_back('(');

    for (auto& room : island.rooms()) {
        auto metac = room->metaclass();

        str->push_back('(');
        (*str) += (*metac)->name();
        str->push_back(' ');
        (*str) += stringify(room->position().x);
        str->push_back(' ');
        (*str) += stringify(room->position().y);

        if (room->health() not_eq room->max_health()) {
            str->push_back(' ');
            (*str) += stringify(room->health());
        }

        str->push_back(')');
    }

    str->push_back(')');

    return str;
}



} // namespace skyland
