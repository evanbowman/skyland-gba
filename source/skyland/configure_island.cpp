////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
