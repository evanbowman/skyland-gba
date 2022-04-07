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



void HideRoomsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    data_ = allocate_dynamic<Data>("hide-rooms-context");


    Text::platform_retain_alphabet(pfrm);


    auto [mt, ms] = room_metatable();
    for (int i = 0; i < ms; ++i) {
        if (is_enabled(i) and
            not(mt[i]->properties() & RoomProperties::not_constructible)) {
            (*data_)->room_classes_.push_back(i);
        }
    }

    repaint(pfrm, app);
}



void HideRoomsScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    names_.clear();
    hidden_.clear();
    pfrm.fill_overlay(0);

    if (changed_) {
        save::store_global_data(pfrm, app.gp_);
    }
}



void HideRoomsScene::repaint(Platform& pfrm, App& app)
{
    auto [mt, ms] = room_metatable();

    if (index_ >= ms or index_ >= (int)(*data_)->room_classes_.size()) {
        Platform::fatal("glossary: invalid index");
    }

    names_.clear();
    hidden_.clear();

    auto put = [&](int index, int vram, int y, bool shade) {
        if (index >= (int)(*data_)->room_classes_.size()) {
            return;
        }
        auto& m = mt[(*data_)->room_classes_[index]];
        auto icon = shade ? m->icon() : m->unsel_icon();
        draw_image(pfrm, vram, 1, y, 4, 4, Layer::overlay);
        pfrm.load_overlay_chunk(vram, icon, 16);

        StringBuffer<48> description;
        description += m->ui_name(pfrm)->c_str();

        names_.emplace_back(
            pfrm, description.c_str(), OverlayCoord{6, u8(y + 1)});

        Text::OptColors opts;
        if (shade) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        if (app.gp_.hidden_rooms_.get((*data_)->room_classes_[index])) {
            auto str = SYSTR(yes);
            hidden_.emplace_back(
                pfrm,
                OverlayCoord{(u8)((calc_screen_tiles(pfrm).x - 1) -
                                  utf8::len(str->c_str())),
                             u8(y + 2)});
            hidden_.back().assign(str->c_str());
        } else {
            auto str = SYSTR(no);
            hidden_.emplace_back(
                pfrm,
                OverlayCoord{(u8)((calc_screen_tiles(pfrm).x - 1) -
                                  utf8::len(str->c_str())),
                             u8(y + 2)});
            hidden_.back().assign(str->c_str());
        }
    };

    put(index_, 181, 3, true);
    put(index_ + 1, 197, 10, false);
    put(index_ + 2, 213, 15, false);

    for (int x = 2; x < calc_screen_tiles(pfrm).x - 2; ++x) {
        pfrm.set_tile(Layer::overlay, x, 8, 377);
    }
}



ScenePtr<Scene>
HideRoomsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    if (player(app).key_down(pfrm, Key::action_2)) {
        return next_();
    }

    if (player(app).key_down(pfrm, Key::action_1)) {
        auto mti = (*data_)->room_classes_[index_];
        app.gp_.hidden_rooms_.set(mti, not app.gp_.hidden_rooms_.get(mti));
        repaint(pfrm, app);
        changed_ = true;
    }

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    int limit = (int)(*data_)->room_classes_.size();
    if (test_key(Key::down) and index_ < limit - 1) {
        ++index_;
        repaint(pfrm, app);
    }

    if (test_key(Key::up) and index_ > 0) {
        --index_;
        repaint(pfrm, app);
    }

    return null_scene();
}



} // namespace skyland
