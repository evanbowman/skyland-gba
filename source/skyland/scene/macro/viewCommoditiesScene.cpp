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


#include "viewCommoditiesScene.hpp"
#include "selectorScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



ViewCommoditiesScene::ViewCommoditiesScene()
    : s_(allocate_dynamic<State>("commodities-overlay-buffer"))
{
    // ...
}



void ViewCommoditiesScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    Text::platform_retain_alphabet(pfrm);

    pfrm.fill_overlay(0);
    pfrm.screen().display();
    pfrm.load_overlay_texture("overlay_challenges");

    pfrm.screen().schedule_fade(0.8f);

    pfrm.screen().pixelate(128, false);

    auto& state = macrocosm(app);
    auto& sector = state.sector();

    auto stats = sector.base_stats();

    auto gather_imports = [&](terrain::Sector& other) {
        if (other.coordinate() not_eq sector.coordinate()) {
        }
    };

    gather_imports(state.data_->origin_sector_);

    for (auto& s : state.data_->other_sectors_) {
        gather_imports(*s);
    }

    stats = sector.base_stats();

    show(pfrm, macrocosm(app));
}



void ViewCommoditiesScene::show(Platform& pfrm, macro::EngineImpl& state)
{
    if (not s_->heading_) {
        s_->heading_.emplace(pfrm, OverlayCoord{1, 1});
        s_->heading_->assign(SYSTR(macro_commodities)->c_str());
        s_->heading_->append(": ");
        s_->heading_->append(state.sector().name().c_str());
        for (int i = 0; i < s_->heading_->len(); ++i) {
            pfrm.set_tile(Layer::overlay, 1 + i, 2, 86);
        }
    }

    s_->lines_.clear();

    int skip = s_->lines_.capacity() * s_->page_;

    for (auto& info : s_->info_) {

        if (skip) {
            --skip;
            continue;
        }

        StringBuffer<56> tmp;
        tmp += loadstr(pfrm, terrain::name(info.type_))->c_str();
        tmp += "(";
        tmp += stringify(info.supply_);
        tmp += ")";

        if (info.source_ == State::CommodityInfo::exported) {
            tmp += "->";
            if (auto sector = state.load_sector(info.destination_)) {
                tmp += sector->name();
            }
        } else if (info.source_ == State::CommodityInfo::imported) {
            tmp += "<-";
            if (auto sector = state.load_sector(info.destination_)) {
                tmp += sector->name();
            }
        }

        s_->lines_.emplace_back(
            pfrm,
            tmp.c_str(),
            OverlayCoord{1, (u8)(4 + s_->lines_.size() * 2)});
    }

    s_->pages_ = s_->info_.size() / s_->lines_.capacity();
    s_->pages_ += s_->info_.size() % s_->lines_.capacity() > 0;

    if (s_->pages_ > 1) {
        int margin = (calc_screen_tiles(pfrm).x - s_->pages_ * 2) / 2;
        for (int i = 0; i < s_->pages_; ++i) {
            if (i == s_->page_) {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



ScenePtr<Scene>
ViewCommoditiesScene::update(Platform& pfrm, App& app, Microseconds delta)
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
        if (s_->page_ < s_->pages_ - 1) {
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
