////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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
