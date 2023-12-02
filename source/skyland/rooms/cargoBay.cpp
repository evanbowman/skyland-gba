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


#include "cargoBay.hpp"
#include "platform/libc.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



CargoBay::CargoBay(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    set_cargo("", 0);
}



void CargoBay::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cargo_bay)->c_str();
}



bool CargoBay::set_cargo(const char* cargo, u8 count)
{
    if (strlen(cargo) + 1 > sizeof cargo_) {
        return false;
    }

    count_ = 0;

    auto dest = cargo_;
    auto src = cargo;

    while (*src not_eq '\0') {
        ++count_;
        *dest++ = *src++;
    }
    *dest = '\0';

    Room::ready();

    return true;
}



void CargoBay::update(Microseconds delta)
{
    Room::update(delta);

    if (count_) {
        Room::ready();
    }
}



void CargoBay::display(Platform::Screen& screen)
{
    if (parent()->interior_visible()) {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y.as_integer() < 700) {
                screen.draw(c->prepare_sprite());
            }
        }

        if (*cargo() not_eq '\0') {
            Sprite sprite;
            sprite.set_texture_index(51);
            auto pos = origin();
            pos.y +=
                Fixnum::from_integer(16 + parent()->get_ambient_movement());
            sprite.set_position(pos);
            sprite.set_size(Sprite::Size::w16_h32);
            screen.draw(sprite);
        }
    }
}



void CargoBay::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cargo_bay;
    buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
}



void CargoBay::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



lisp::Value* CargoBay::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    builder.push_back(lisp::make_string(cargo()));

    if (health() not_eq max_health()) {
        builder.push_back(lisp::make_integer(health()));
    }

    return builder.result();
}



void CargoBay::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 4) {
        auto c = lisp::get_list(list, 3);
        if (c->type() == lisp::Value::Type::string) {
            set_cargo(c->string().value(), strlen(c->string().value()));
        }
    }

    if (lisp::length(list) >= 5) {
        __set_health(lisp::get_list(list, 4)->integer().value_);
    }
}



void CargoBay::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());

        if (cargo_[0] not_eq '\0') {
            time_stream::event::CargoBayContents e;

            static_assert(sizeof e.cargo_ == sizeof cargo_);
            memcpy(e.cargo_, cargo_, sizeof cargo_);
            e.count_ = count_;
            e.x_ = position().x;
            e.y_ = position().y;
            e.near_ = is_player_island(parent());

            APP.time_stream().push(APP.level_timer(), e);
        }
    }
}



} // namespace skyland
