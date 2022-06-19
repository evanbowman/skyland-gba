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


#include "citizensInfoScene.hpp"
#include "selectorScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



CitizensInfoScene::CitizensInfoScene()
    : s_(allocate_dynamic<State>("citizens-info"))
{
}



void CitizensInfoScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    Text::platform_retain_alphabet(pfrm);

    pfrm.fill_overlay(0);
    pfrm.screen().display();
    pfrm.load_overlay_texture("overlay_challenges");

    pfrm.screen().schedule_fade(0.8f);

    pfrm.screen().pixelate(128, false);

    show(pfrm, macrocosm(app));
}



void CitizensInfoScene::show(Platform& pfrm, macro::EngineImpl& state)
{
    if (not s_->heading_) {
        s_->heading_.emplace(pfrm, OverlayCoord{1, 1});
        s_->heading_->assign(SYSTR(macro_citizens)->c_str());
        s_->heading_->append(": ");
        s_->heading_->append(state.sector().name().c_str());
        for (int i = 0; i < s_->heading_->len(); ++i) {
            pfrm.set_tile(Layer::overlay, 1 + i, 2, 86);
        }
    }

    s_->lines_.clear();

    auto b = state.sector().annotate_happiness();

    const auto st = calc_screen_tiles(pfrm);

    auto append_value = [&](Float fval) {
        int value = fval;
        auto tl = integer_text_length(value);

        bool fraction = false;

        if (fval not_eq 0.f and abs(fval) < 1.f) {
            fraction = true;
            tl = 3 + (fval < 0);
        }


        auto add_space = (st.x - 3) - (s_->lines_.back().len() + tl);
        if (add_space > 0) {
            while (add_space) {
                s_->lines_.back().append(" ");
                --add_space;
            }
        }

        if (fraction) {
            auto upscale = abs(fval) * 10;
            if (fval < 0) {
                s_->lines_.back().append("-");
            }
            s_->lines_.back().append("0.");
            char c[2] = {stringify(upscale)[0], '\0'};
            s_->lines_.back().append(c);
        } else {
            s_->lines_.back().append(value);
        }

        pfrm.set_tile(Layer::overlay,
                      s_->lines_.back().coord().x + s_->lines_.back().len(),
                      s_->lines_.back().coord().y,
                      89);
    };

    Float total = 0.f;

    int skip = s_->lines_.capacity() * s_->page_;

    int count = 0;
    auto item = b.entries();
    while (item) {
        ++count;
        item = item->next_;
    }

    s_->lines_.emplace_back(pfrm,
                            OverlayCoord{1, u8(4 + s_->lines_.size() * 2)});
    s_->lines_.back().assign("");
    s_->lines_.back().append(SYSTR(macro_fiscal_happiness)->c_str());
    s_->lines_.back().append(":");

    item = b.entries();
    while (item and not s_->lines_.full()) {

        if (skip == 0) {
            s_->lines_.emplace_back(
                pfrm, OverlayCoord{1, u8(5 + s_->lines_.size() * 2)});
            s_->lines_.back().assign(item->label_.c_str());

            append_value(item->contribution_);
        } else {
            --skip;
        }

        total += item->contribution_;

        item = item->next_;
    }

    s_->pages_ = count / s_->lines_.capacity();
    // Add an extra page for the Total line.
    s_->pages_ += (count % s_->lines_.capacity()) == 0;

    if (not s_->lines_.full()) {
        s_->lines_.emplace_back(pfrm,
                                OverlayCoord{1, u8(5 + s_->lines_.size() * 2)});
        s_->lines_.back().append(SYSTR(macro_total_happiness)->c_str());

        append_value(total);
    }

    if (count + 1 > (int)s_->lines_.capacity()) {
        int margin = (calc_screen_tiles(pfrm).x - s_->pages_ * 2) / 2;
        for (int i = 0; i < s_->pages_ + 1; ++i) {
            if (i == s_->page_) {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



ScenePtr<Scene>
CitizensInfoScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (exit_) {
        pfrm.load_overlay_texture("overlay");
        return scene_pool::alloc<SelectorScene>();
    }

    if (player(app).key_down(pfrm, Key::left)) {
        if (s_->page_ > 0) {
            --s_->page_;
            show(pfrm, macrocosm(app));
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    }

    if (player(app).key_down(pfrm, Key::right)) {
        if (s_->page_ < s_->pages_) {
            ++s_->page_;
            show(pfrm, macrocosm(app));
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    }

    if (player(app).key_down(pfrm, Key::action_2)) {
        exit_ = true;
        pfrm.fill_overlay(0);
        s_->heading_.reset();
        s_->lines_.clear();
        pfrm.screen().schedule_fade(0);
        pfrm.screen().pixelate(0);
    }

    return null_scene();
}



} // namespace skyland::macro
