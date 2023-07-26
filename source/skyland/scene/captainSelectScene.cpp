////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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

#include "captainSelectScene.hpp"
#include "graphics/overlay.hpp"
#include "number/random.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "skyland/captain.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



void CaptainSelectScene::show_page(Platform& pfrm, App& app)
{
    const auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < st.y; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    pfrm.set_tile(Layer::overlay, 28, 1, 386);

    auto sel_captain_text = SYSTR(select_captain);
    u8 margin =
        centered_text_margins(pfrm, utf8::len(sel_captain_text->c_str()));
    if (not describe_only_) {
        Text::print(pfrm, sel_captain_text->c_str(), {margin, 1});
    }
    pfrm.screen().schedule_fade(0);
    pfrm.screen().schedule_fade(1);

    int offset = (captain_icon(options_[index_]) - 1) * 16;
    pfrm.load_overlay_chunk(181, offset, 16, "character_art");

    for (int x = 2; x < 28; ++x) {
        pfrm.set_tile(Layer::overlay, x, 3, 161);
    }

    int tile = 181;
    for (int y = 0; y < 4; ++y) {
        for (int x = 0; x < 4; ++x) {
            pfrm.set_tile(Layer::overlay, x + 2, y + 4, tile++, 10);
        }
    }

    Text::print(pfrm, captain_name(options_[index_]).c_str(), {7, 5});

    tv_.emplace(pfrm);
    tv_->assign(captain_desc(options_[index_]).c_str(), {1, 9}, {28, 8});

    int captain_count = options_.size();

    if (describe_only_) {
        return;
    }
    int pg_margin = (calc_screen_tiles(pfrm).x - captain_count * 2) / 2;
    for (int i = 0; i < captain_count; ++i) {
        if (i == index_) {
            pfrm.set_tile(Layer::overlay, pg_margin + i * 2, 18, 160);
        } else {
            pfrm.set_tile(Layer::overlay, pg_margin + i * 2, 18, 159);
        }
    }
}



void CaptainSelectScene::describe_only(CaptainAbility ability)
{
    describe_only_ = true;
    options_.clear();
    options_.push_back(ability);
}



void CaptainSelectScene::describe_all()
{
    describe_only_ = true;
    options_.clear();
    for (int i = 0; i < (int)CaptainAbility::none; ++i) {
        options_.push_back((CaptainAbility)i);
    }
}



void CaptainSelectScene::enter(Platform& pfrm, App& app, Scene&)
{
    for (int i = 0; i < 3; ++i) {
        if (describe_only_) {
            break;
        }
    RETRY:
        auto choice =
            rng::choice<(int)CaptainAbility::none>(rng::critical_state);
        for (auto& capn : options_) {
            if (capn == (CaptainAbility)choice) {
                goto RETRY;
            }
        }
        options_.push_back((CaptainAbility)choice);
    }

    Text::platform_retain_alphabet(pfrm);
    Text::print(pfrm, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", {0, 22});

    show_page(pfrm, app);

    pfrm.speaker().play_sound("openbag", 3);
}



void CaptainSelectScene::exit(Platform& pfrm, App&, Scene&)
{
    pfrm.fill_overlay(0);
    pfrm.screen().schedule_fade(0);
}



ScenePtr<Scene>
CaptainSelectScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::right) or test_key(Key::down)) {
        if (index_ < (int)options_.size() - 1) {
            ++index_;
            show_page(pfrm, app);
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    } else if (test_key(Key::left) or test_key(Key::up)) {
        if (index_ > 0) {
            --index_;
            show_page(pfrm, app);
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    } else if (test_key(Key::action_1)) {

        if (describe_only_) {
            if (next_) {
                return (*next_)();
            }
            return scene_pool::alloc<ReadyScene>();
        }

        pfrm.speaker().play_sound("button_wooden", 3);

        auto fn = lisp::get_var("on-capn-sel");
        if (fn->type() == lisp::Value::Type::function) {
            auto name = captain_name(options_[index_]);
            lisp::push_op(L_INT(captain_icon(options_[index_])));
            lisp::push_op(lisp::make_string(name.c_str()));
            lisp::safecall(fn, 2);
            lisp::pop_op(); // funcall result
        }

        if (next_) {
            return (*next_)();
        }
        return scene_pool::alloc<ReadyScene>();
    } else if (describe_only_ and test_key(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
