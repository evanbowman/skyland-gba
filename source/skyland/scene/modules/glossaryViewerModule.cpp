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


#include "glossaryViewerModule.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u16 room_category_icon(Room::Category category);



void GlossaryViewerModule::load_page(Platform& pfrm, int page)
{
    auto [mt, ms] = room_metatable();

    auto icon = mt[page]->unsel_icon();
    draw_image(pfrm, 181, 1, 1, 4, 4, Layer::overlay);
    pfrm.load_overlay_chunk(181, icon, 16);

    if (not item_name_) {
        item_name_.emplace(pfrm, OverlayCoord{6, 1});
    }


    pfrm.set_tile(
        Layer::overlay, 28, 1, room_category_icon(mt[page]->category()));

    StringBuffer<30> temp;
    temp += mt[page]->ui_name(pfrm)->c_str();
    temp += " (";
    temp += stringify(mt[page]->size().x);
    temp += ",";
    temp += stringify(mt[page]->size().y);
    temp += ")";

    item_name_->assign(temp.c_str());

    temp.clear();

    if (not dependency_text_) {
        dependency_text_.emplace(pfrm, OverlayCoord{1, 18});
    }

    const auto cond = mt[page]->properties();
    if (cond & RoomProperties::workshop_required) {
        dependency_text_->assign("(requires workshop)",
                                 FontColors{ColorConstant::med_blue_gray,
                                            ColorConstant::rich_black});
    } else if (cond & RoomProperties::manufactory_required) {
        dependency_text_->assign("(requires manufactory)",
                                 FontColors{ColorConstant::med_blue_gray,
                                            ColorConstant::rich_black});
    } else {
        dependency_text_.reset();
    }


    if (not item_details_) {
        item_details_.emplace(pfrm, OverlayCoord{6, 3});
    }

    temp += stringify(mt[page]->cost());
    temp += "@ ";
    temp += stringify(mt[page]->consumes_power());
    temp += "` ";
    temp += stringify(mt[page]->full_health());
    temp += "hp";

    item_details_->assign(temp.c_str());

    StringBuffer<512> description;

    if (is_enabled((MetaclassIndex)page)) {
        mt[page]->format_description(pfrm, description);
    } else {
        description = "Locked! See achievements!";
    }

    if (not item_description_) {
        item_description_.emplace(pfrm);
    }

    item_description_->assign(
        description.c_str(), OverlayCoord{1, 6}, OverlayCoord{28, 11});
}



void GlossaryViewerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    load_page(pfrm, page_);
    pfrm.screen().fade(0.95f);
    pfrm.screen().fade(1.f);

    pfrm.speaker().set_music_volume(10);

    Text::platform_retain_alphabet(pfrm);
}



void GlossaryViewerModule::exit(Platform& pfrm, App& app, Scene& next)
{
    item_name_.reset();
    item_details_.reset();
    item_description_.reset();
    dependency_text_.reset();

    pfrm.fill_overlay(0);

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



ScenePtr<Scene>
GlossaryViewerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto [mt, ms] = room_metatable();

    app.player().update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };


    if (test_key(Key::right) and page_ < ms - 1 and
        page_ < plugin_rooms_begin() - 1) {
        load_page(pfrm, ++page_);
    }

    if (test_key(Key::left) and page_ > 0) {
        load_page(pfrm, --page_);
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (next_scene_) {
            return (*next_scene_)();
        }
        return scene_pool::alloc<TitleScreenScene>(3);
    }


    return null_scene();
}



GlossaryViewerModule::Factory GlossaryViewerModule::factory_;



} // namespace skyland
