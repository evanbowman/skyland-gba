////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "configure_island.hpp"
#include "room_metatable.hpp"



namespace skyland
{



void configure_island(Platform& pfrm,
                      App& app,
                      Island& island,
                      lisp::Value* island_desc_lat)
{
    island.clear_rooms(pfrm, app);

    lisp::foreach (island_desc_lat, [&](lisp::Value* val) {
        auto name_symb = lisp::get_list(val, 0);
        if (name_symb->type() not_eq lisp::Value::Type::symbol) {
            return;
        }

        const auto len = length(val);


        if (len >= 3) {
            u8 x = lisp::get_list(val, 1)->integer().value_;
            u8 y = lisp::get_list(val, 2)->integer().value_;

            if (auto c = load_metaclass(name_symb->symbol().name_)) {
                (*c)->create(pfrm, app, &island, Vec2<u8>{x, y}, false);
                if (auto room = island.get_room({x, y})) {
                    room->deserialize(val);
                }
            }
        }
    });

    island.repaint(pfrm, app);
}



void configure_island_from_codestring(Platform& pfrm,
                                      App& app,
                                      Island& island,
                                      const char* lisp_data)
{
    lisp::BasicCharSequence seq(lisp_data);
    lisp::read(seq); // leaves result of (read) at top of operand stack.

    configure_island(pfrm, app, island, lisp::get_op(0));

    lisp::pop_op();
}



} // namespace skyland
