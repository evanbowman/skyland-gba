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


#include "crane.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/worldScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Crane::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_crane)->c_str();
}



Crane::Crane(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Crane::rewind(Time delta)
{
    Room::rewind(delta);
    timer_ = 0;
    state_ = State::idle;
}



void Crane::update(Time delta)
{
    Room::update(delta);
    Room::ready();

    switch (state_) {
    case State::idle:
        break;

    case State::drop:
        timer_ += delta;
        break;

    case State::retract:
        timer_ -= delta;
        if (timer_ < 0) {
            timer_ = 0;
            state_ = State::idle;

            auto pos = center();
            pos.x += 14.0_fixed;
            pos.y += Fixnum::from_integer(parent()->get_ambient_movement());
            pos.y += 2.0_fixed;
            pos.y += Fixnum::from_integer(timer_) * Fixnum(0.00004f);

            if (item_ == Discoveries::Item::bomb) {
                PLATFORM.speaker().play_sound("explosion1", 2);
                big_explosion(pos);
                apply_damage(5);
            } else {
                // ...
            }
        }
        break;
    }
}



void Crane::display_on_hover(Platform::Screen& screen,

                             const RoomCoord& cursor)
{
    auto origin = parent()->visual_origin();

    const auto pos = position();
    origin.x += Fixnum::from_integer(pos.x * 16);
    origin.y += Fixnum::from_integer(pos.y * 16);

    if (cursor.y == pos.y) {
        origin.y += 16.0_fixed;
    }

    Sprite icon;
    icon.set_size(Sprite::Size::w16_h32);
    icon.set_texture_index(49);

    auto p1 = origin;
    icon.set_position(p1);

    screen.draw(icon);

    auto p2 = origin;
    p2.x += 32.0_fixed;
    icon.set_position(p2);
    icon.set_texture_index(50);

    screen.draw(icon);
}



void Crane::display(Platform::Screen& screen)
{
    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);

    auto pos = center();
    pos.x += 14.0_fixed;
    pos.y += Fixnum::from_integer(parent()->get_ambient_movement());
    pos.y += 2.0_fixed;
    const auto start_pos = pos;

    pos.y += Fixnum::from_integer(timer_) * Fixnum(0.00004f);
    spr.set_position(pos);


    if (state_ == State::retract) {
        switch (item_) {
        case 0:
            spr.set_texture_index(94);
            screen.draw(spr);
            break;

        default:
            spr.set_texture_index(51);
            screen.draw(spr);
            break;
        }
    }


    spr.set_texture_index(92);

    screen.draw(spr);

    const auto claw_pos = pos;

    spr.set_texture_index(93);

    pos = start_pos;
    pos.y -= 1.0_fixed;
    while (pos.y < claw_pos.y - 3.0_fixed) {
        spr.set_position(pos);
        screen.draw(spr);
        pos.y += 8.0_fixed;
    }
}



ScenePtr<Scene> Crane::select_impl(const RoomCoord& cursor)
{
    return null_scene();
}



void Crane::render_interior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    buffer[x][y] = InteriorTile::crane_1;
    buffer[x + 1][y] = InteriorTile::crane_2;
    buffer[x + 2][y] = InteriorTile::crane_3;
    buffer[x][y + 1] = InteriorTile::crane_4;
    buffer[x + 1][y + 1] = InteriorTile::crane_5;
    buffer[x + 2][y + 1] = InteriorTile::crane_6;
}



void Crane::render_exterior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    buffer[x][y] = Tile::crane_1;
    buffer[x + 1][y] = Tile::crane_2;
    buffer[x + 2][y] = Tile::crane_3;
    buffer[x][y + 1] = Tile::crane_4;
    buffer[x + 1][y + 1] = Tile::crane_5;
    buffer[x + 2][y + 1] = Tile::crane_6;
}



static const char* crane_save_fname = "/save/crane.dat";



Crane::Discoveries Crane::load_discoveries()
{
    Discoveries result;
    result.items_.set(0);

    const char* fname = crane_save_fname;

    Vector<char> output;
    const auto bytes_read =
        flash_filesystem::read_file_data_binary(fname, output);

    if (bytes_read == sizeof(result)) {
        for (u32 i = 0; i < bytes_read; ++i) {
            ((u8*)&result)[i] = output[i];
        }
    }


    return result;
}



void Crane::store_discoveries(const Discoveries& d)
{
    Vector<char> output;

    for (u32 i = 0; i < sizeof d; ++i) {
        output.push_back(((u8*)&d)[i]);
    }

    flash_filesystem::store_file_data_binary(crane_save_fname, output);
}



} // namespace skyland
