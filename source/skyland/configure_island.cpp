////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "configure_island.hpp"
#include "room_metatable.hpp"
#include "script/lisp.hpp"



namespace skyland
{



void configure_island(Island& island, lisp::Value* island_desc_lat)
{
    island.clear_rooms();

    lisp::l_foreach(island_desc_lat, [&](lisp::Value* val) {
        auto name_symb = lisp::get_list(val, 0);
        if (name_symb->type() not_eq lisp::Value::Type::symbol) {
            if (APP.is_developer_mode()) {
                // TODO: error
            }
            return;
        }

        const auto len = length(val);


        if (len >= 3) {
            u8 x = lisp::get_list(val, 1)->integer().value_;
            u8 y = lisp::get_list(val, 2)->integer().value_;

            if (auto c = load_metaclass(name_symb->symbol().name())) {
                (*c)->create(&island, RoomCoord{x, y}, false);
                if (auto room = island.get_room({x, y})) {
                    room->deserialize(val);
                }
            }
        }
    });

    island.repaint();
}



void configure_island_from_codestring(Island& island, const char* lisp_data)
{
    lisp::BasicCharSequence seq(lisp_data);
    lisp::read(seq); // leaves result of (read) at top of operand stack.

    configure_island(island, lisp::get_op(0));

    lisp::pop_op();
}



} // namespace skyland
