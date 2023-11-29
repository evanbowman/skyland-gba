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


#include "hideRoomsScene.hpp"
#include "skyland/player/player.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void HideRoomsScene::enter(Scene& prev)
{
    data_ = allocate_dynamic<Data>("hide-rooms-context");


    auto [mt, ms] = room_metatable();
    for (int i = 0; i < ms; ++i) {
        if (is_enabled(i) and
            not(mt[i]->properties() & RoomProperties::not_constructible)) {
            (*data_)->room_classes_.push_back(i);
        }
    }

    repaint();
}



void HideRoomsScene::exit(Scene& prev)
{
    names_.clear();
    hidden_.clear();
    PLATFORM.fill_overlay(0);

    if (changed_) {
        store_hidden_rooms();
    }
}



void HideRoomsScene::repaint()
{
    auto [mt, ms] = room_metatable();

    if (index_ >= ms or index_ >= (int)(*data_)->room_classes_.size()) {
        Platform::fatal("glossary: invalid index");
    }

    names_.clear();
    hidden_.clear();

    auto put = [&](int index, int vram, int y, bool shade) {
        // Lazy hack: plundered-room has no icon.

        auto& m = (index >= (int)(*data_)->room_classes_.size())
                      ? require_metaclass("plundered-room")
                      : mt[(*data_)->room_classes_[index]];

        auto icon = shade ? m->icon() : m->unsel_icon();
        draw_image(vram, 1, y, 4, 4, Layer::overlay);
        PLATFORM.load_overlay_chunk(vram, icon, 16);

        if (m->properties() & RoomProperties::not_constructible) {
            return;
        }

        StringBuffer<48> description;
        description += m->ui_name()->c_str();

        names_.emplace_back(description.c_str(), OverlayCoord{6, u8(y + 1)});

        Text::OptColors opts;
        if (shade) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        if (room_hidden((*data_)->room_classes_[index])) {
            auto str = SYSTR(yes);
            hidden_.emplace_back(

                OverlayCoord{
                    (u8)((calc_screen_tiles().x - 1) - utf8::len(str->c_str())),
                    u8(y + 2)});
            hidden_.back().assign(str->c_str());
        } else {
            auto str = SYSTR(no);
            hidden_.emplace_back(

                OverlayCoord{
                    (u8)((calc_screen_tiles().x - 1) - utf8::len(str->c_str())),
                    u8(y + 2)});
            hidden_.back().assign(str->c_str());
        }
    };

    put(index_, 181, 3, true);
    put(index_ + 1, 197, 10, false);
    put(index_ + 2, 213, 15, false);

    for (int x = 2; x < calc_screen_tiles().x - 2; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 8, 377);
    }
}



ScenePtr<Scene> HideRoomsScene::update(Microseconds delta)
{
    player().update(delta);

    if (player().key_down(Key::action_2)) {
        return next_();
    }

    if (player().key_down(Key::action_1)) {
        auto mti = (*data_)->room_classes_[index_];
        room_set_hidden(mti, not room_hidden(mti));
        repaint();
        changed_ = true;
    }

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    int limit = (int)(*data_)->room_classes_.size();
    if (test_key(Key::down) and index_ < limit - 1) {
        ++index_;
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::up) and index_ > 0) {
        --index_;
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    return null_scene();
}



} // namespace skyland
