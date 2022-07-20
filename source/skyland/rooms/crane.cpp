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



void Crane::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_crane)->c_str();
}



Crane::Crane(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Crane::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);
    timer_ = 0;
    state_ = State::idle;
}



void Crane::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
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
            pos.x += 14;
            pos.y += parent()->get_ambient_movement();
            pos.y += 2;
            pos.y += timer_ * Fixnum(0.00004f);

            if (item_ == Discoveries::Item::bomb) {
                pfrm.speaker().play_sound("explosion1", 2);
                big_explosion(pfrm, app, pos);
                apply_damage(pfrm, app, 5);
            } else {
                // ...
            }
        }
        break;
    }
}



void Crane::display_on_hover(Platform::Screen& screen,
                             App& app,
                             const RoomCoord& cursor)
{
    auto origin = parent()->visual_origin();

    const auto pos = position();
    origin.x += pos.x * 16;
    origin.y += pos.y * 16;

    if (cursor.y == pos.y) {
        origin.y += 16;
    }

    Sprite icon;
    icon.set_size(Sprite::Size::w16_h32);
    icon.set_texture_index(49);

    auto p1 = origin;
    icon.set_position(p1);

    screen.draw(icon);

    auto p2 = origin;
    p2.x += 32;
    icon.set_position(p2);
    icon.set_texture_index(50);

    screen.draw(icon);
}



void Crane::display(Platform::Screen& screen)
{
    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);

    auto pos = center();
    pos.x += 14;
    pos.y += parent()->get_ambient_movement();
    pos.y += 2;
    const auto start_pos = pos;

    pos.y += timer_ * Fixnum(0.00004f);
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
    pos.y -= 1;
    while (pos.y < claw_pos.y - 3) {
        spr.set_position(pos);
        screen.draw(spr);
        pos.y += 8;
    }
}



class CraneItemScene : public ActiveWorldScene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        ActiveWorldScene::enter(pfrm, app, prev);

        auto d = Crane::load_discoveries(pfrm);
        items_ = d.items();

        if (not items_.empty()) {
            show(pfrm);
        }
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        ActiveWorldScene::exit(pfrm, app, next);
        text_.reset();
        pfrm.fill_overlay(0);
    }


    void show(Platform& pfrm)
    {
        const u8 y = calc_screen_tiles(pfrm).y - 1;

        if (text_) {
            for (int i = 0; i < text_->len() + 2; ++i) {
                pfrm.set_tile(Layer::overlay, i, y - 1, 0);
            }
        }

        auto item = (int)items_[index_];
        auto name =
            get_line_from_file(pfrm, "/strings/crane.txt", 1 + item * 2);


        pfrm.set_tile(Layer::overlay, 0, y, 392);
        pfrm.set_tile(Layer::overlay, 1, y, 393);
        text_.emplace(pfrm, ":", OverlayCoord{2, y});
        text_->append(name->c_str());

        for (int i = 0; i < text_->len() + 2; ++i) {
            pfrm.set_tile(Layer::overlay, i, y - 1, 425);
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (items_.empty()) {
            pfrm.speaker().play_sound("beep_error", 3);
            return scene_pool::alloc<ReadyScene>();
        }

        if (key_down<Key::action_2>(pfrm)) {
            return scene_pool::alloc<ReadyScene>();
        }

        if (key_down<Key::down>(pfrm)) {
            if (index_ == items_.size() - 1) {
                index_ = 0;
            } else {
                ++index_;
            }
            pfrm.speaker().play_sound("click", 1);
            show(pfrm);
        }

        if (key_down<Key::up>(pfrm)) {
            if (index_ == 0) {
                index_ = items_.size() - 1;
            } else {
                --index_;
            }
            pfrm.speaker().play_sound("click", 1);
            show(pfrm);
        }

        if (key_down<Key::action_1>(pfrm)) {
            auto item = (int)items_[index_];
            auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
            *buffer +=
                get_line_from_file(pfrm, "/strings/crane.txt", 1 + item * 2)
                    ->c_str();
            *buffer += ": ";
            *buffer +=
                get_line_from_file(pfrm, "/strings/crane.txt", 1 + item * 2 + 1)
                    ->c_str();
            auto next =
                scene_pool::alloc<BoxedDialogSceneWS>(std::move(buffer), false);

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



ScenePtr<Scene> Crane::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    auto& env = app.environment();
    auto clear_skies = dynamic_cast<weather::ClearSkies*>(&env);

    auto pos = position();
    if (cursor.x == pos.x + 2) {
        if (not clear_skies) {
            pfrm.speaker().play_sound("beep_error", 3);
        }

        if (state_bit_load(app, StateBit::crane_game_got_treasure)) {
            pfrm.speaker().play_sound("beep_error", 3);
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



void Crane::render_interior(App& app, TileId buffer[16][16])
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



void Crane::render_exterior(App& app, TileId buffer[16][16])
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



Crane::Discoveries Crane::load_discoveries(Platform& pfrm)
{
    Discoveries result;
    result.items_.set(0);

    const char* fname = crane_save_fname;

    Vector<char> output;
    const auto bytes_read =
        flash_filesystem::read_file_data_binary(pfrm, fname, output);

    if (bytes_read == sizeof(result)) {
        for (u32 i = 0; i < bytes_read; ++i) {
            ((u8*)&result)[i] = output[i];
        }
    }


    return result;
}



void Crane::store_discoveries(Platform& pfrm, const Discoveries& d)
{
    Vector<char> output;

    for (u32 i = 0; i < sizeof d; ++i) {
        output.push_back(((u8*)&d)[i]);
    }

    flash_filesystem::store_file_data_binary(pfrm, crane_save_fname, output);
}



} // namespace skyland
