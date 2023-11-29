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


#include "helpScene.hpp"
#include "macroverseScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void HelpScene::show_page(int pg)
{
    PLATFORM.fill_overlay(0);

    if (not tv_) {
        tv_.emplace();
    }

    if (not heading_) {
        heading_.emplace(OverlayCoord{1, 1});
    }

    for (int i = 0; i < heading_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 0, 0);
    }
    heading_->assign(SYS_CSTR(macro_help_prefix));

    int margin = (calc_screen_tiles().x - page_count * 2) / 2;
    for (int i = 0; i < page_count; ++i) {
        if (i == page_) {
            PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 101);
        } else {
            PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 100);
        }
    }

    switch (pg) {
    case 0:
        heading_->append(SYS_CSTR(macro_help_title_1));
        PLATFORM.set_tile(Layer::overlay, heading_->len() + 1, 1, 413);
        tv_->assign(SYS_CSTR(macro_help_1), {1, 4}, {28, 7});
        break;

    case 1:
        heading_->append(SYS_CSTR(macro_help_title_2));
        PLATFORM.set_tile(Layer::overlay, heading_->len() + 1, 1, 415);
        tv_->assign(SYS_CSTR(macro_help_2), {1, 4}, {28, 7});
        break;

    case 2:
        heading_->append(SYS_CSTR(macro_help_title_3));
        PLATFORM.set_tile(Layer::overlay, heading_->len() + 1, 1, 414);
        tv_->assign(SYS_CSTR(macro_help_3), {1, 4}, {28, 7});
        break;

    case 3:
        heading_->append(SYS_CSTR(macro_help_title_3));
        PLATFORM.set_tile(Layer::overlay, heading_->len() + 1, 1, 414);
        tv_->assign(SYS_CSTR(macro_help_4), {1, 4}, {28, 7});
        break;

    case 4:
        heading_->append(SYS_CSTR(macro_help_title_4));
        tv_->assign(SYS_CSTR(macro_help_5), {1, 4}, {28, 7});
        break;

    case 5:
        heading_->append(SYS_CSTR(macro_help_title_4));
        tv_->assign(SYS_CSTR(macro_help_6), {1, 4}, {28, 7});
        break;

    case 6:
        heading_->append(SYS_CSTR(macro_help_title_4));
        tv_->assign(SYS_CSTR(macro_help_7), {1, 4}, {28, 7});
        break;

    case 7:
        heading_->append(SYS_CSTR(macro_help_title_5));
        tv_->assign(SYS_CSTR(macro_help_8), {1, 4}, {28, 7});
        break;
    }

    auto st = calc_screen_tiles();
    for (int x = 0; x < st.x; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 2, 107);
    }
}



void HelpScene::enter(Scene&)
{
    PLATFORM.screen().schedule_fade(1.f);
    PLATFORM.fill_overlay(0);

    show_page(0);
}



void HelpScene::exit(Scene&)
{
    PLATFORM.screen().schedule_fade(0);
    PLATFORM.fill_overlay(0);

    tv_.reset();
    heading_.reset();
}



ScenePtr<Scene> HelpScene::update(Microseconds delta)
{
    player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (player().key_down(Key::action_1) or player().key_down(Key::action_2)) {

        return scene_pool::alloc<SelectorScene>();
    }

    if (test_key(Key::right)) {
        if (page_ < page_count - 1) {
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_page(++page_);
        }
    }

    if (test_key(Key::left)) {
        if (page_ > 0) {
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_page(--page_);
        }
    }

    return null_scene();
}



} // namespace skyland::macro
