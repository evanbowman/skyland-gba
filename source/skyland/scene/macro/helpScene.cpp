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



ScenePtr HelpScene::update(Time delta)
{
    player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (player().key_down(Key::action_1) or player().key_down(Key::action_2)) {

        return make_scene<SelectorScene>();
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
