////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "crane.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/craneDropScene.hpp"
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



void Crane::rewind(Microseconds delta)
{
    Room::rewind(delta);
    timer_ = 0;
    state_ = State::idle;
}



void Crane::update(Microseconds delta)
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



class CraneItemScene : public ActiveWorldScene
{
public:
    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        auto d = Crane::load_discoveries();
        items_ = d.items();

        if (not items_.empty()) {
            show();
        }
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);
        text_.reset();
        PLATFORM.fill_overlay(0);
    }


    void show()
    {
        const u8 y = calc_screen_tiles().y - 1;

        if (text_) {
            for (int i = 0; i < text_->len() + 2; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, y - 1, 0);
            }
        }

        auto item = (int)items_[index_];
        auto name = get_line_from_file("/strings/crane.txt", 1 + item * 2);


        PLATFORM.set_tile(Layer::overlay, 0, y, 392);
        PLATFORM.set_tile(Layer::overlay, 1, y, 393);
        text_.emplace(":", OverlayCoord{2, y});
        text_->append(name->c_str());

        for (int i = 0; i < text_->len() + 2; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, y - 1, 425);
        }
    }


    ScenePtr<Scene> update(Microseconds delta) override
    {
        if (items_.empty()) {
            PLATFORM.speaker().play_sound("beep_error", 3);
            return scene_pool::alloc<ReadyScene>();
        }

        if (key_down<Key::action_2>()) {
            return scene_pool::alloc<ReadyScene>();
        }

        if (key_down<Key::down>()) {
            if (index_ == items_.size() - 1) {
                index_ = 0;
            } else {
                ++index_;
            }
            PLATFORM.speaker().play_sound("click", 1);
            show();
        }

        if (key_down<Key::up>()) {
            if (index_ == 0) {
                index_ = items_.size() - 1;
            } else {
                --index_;
            }
            PLATFORM.speaker().play_sound("click", 1);
            show();
        }

        if (key_down<Key::action_1>()) {
            auto item = (int)items_[index_];
            auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
            *buffer +=
                get_line_from_file("/strings/crane.txt", 1 + item * 2)->c_str();
            *buffer += ": ";
            *buffer +=
                get_line_from_file("/strings/crane.txt", 1 + item * 2 + 1)
                    ->c_str();
            auto next =
                scene_pool::alloc<BoxedDialogSceneWS>(std::move(buffer));

            next->pause_if_hostile_ = false;
            next->autorestore_music_volume_ = true;

            return next;
        }

        return null_scene();
    }


private:
    u32 index_ = 0;
    std::optional<Text> text_;
    Buffer<Crane::Discoveries::Item, (int)Crane::Discoveries::Item::count>
        items_;
};



ScenePtr<Scene> Crane::select(const RoomCoord& cursor)
{
    auto& env = APP.environment();
    auto clear_skies = not env.is_overcast();

    auto pos = position();
    if (cursor.x == pos.x + 2) {
        if (not clear_skies) {
            PLATFORM.speaker().play_sound("beep_error", 3);
        }

        if (state_bit_load(StateBit::crane_game_got_treasure)) {
            PLATFORM.speaker().play_sound("beep_error", 3);
        }
    } else {
        return scene_pool::alloc<CraneItemScene>();
    }

    if (state_ == State::idle) {
        state_ = State::drop;
        timer_ = 0;
    }

    return scene_pool::alloc<CraneDropScene>(position());
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
