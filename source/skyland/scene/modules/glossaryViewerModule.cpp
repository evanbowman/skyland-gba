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
    if (state_ == State::quickview) {
        load_page(pfrm, page_);
    } else {
        load_categories(pfrm);
    }

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



void GlossaryViewerModule::load_categories(Platform& pfrm)
{
    Text heading(pfrm, OverlayCoord{1, 1});
    heading.assign("- ");
    heading.append(SYSTR(module_glossary)->c_str());
    heading.append(" -");
    heading.__detach();

    int row = 4;
    for (int i = 0; i < (int)Room::Category::count; ++i) {
        Text t(pfrm, OverlayCoord{3, (u8)(row + i * 2)});
        auto category_str = (SystemString)(((int)SystemString::category_begin) +
                                           i);
        t.append(loadstr(pfrm, category_str)->c_str());
        t.__detach();
    }

    pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 396);
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


    switch (state_) {
    case State::show_categories:
        if (test_key(Key::up) and cg_cursor_ > 0) {
            --cg_cursor_;
            pfrm.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 396);
        }

        if (test_key(Key::down) and cg_cursor_ <
            (int)Room::Category::count - 1) {
            ++cg_cursor_;
            pfrm.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 396);
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::view;
            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 0);
                }
            }

            auto [mt, ms] = room_metatable();

            page_ = 0;
            for (int i = 0; i < ms; ++i) {
                if (mt[i]->category() == (Room::Category)cg_cursor_) {
                    break;
                }
                ++page_;
            }

            filter_begin_ = page_;
            filter_end_ = page_;
            while (filter_end_ < ms and
                   mt[filter_end_]->category() == (Room::Category)cg_cursor_) {
                ++filter_end_;
            }
            if (filter_end_ == ms) {
                ++filter_end_;
            }

            load_page(pfrm, page_);
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            if (next_scene_) {
                return (*next_scene_)();
            }
            return scene_pool::alloc<TitleScreenScene>(3);
        }
        break;

    case State::view:
    case State::quickview:
        if (test_key(Key::right) and page_ < ms - 1 and
            page_ < plugin_rooms_begin() - 1 and
            (not filter_end_ or (filter_end_ and filter_end_ - 1 > page_))) {
            load_page(pfrm, ++page_);
        }

        if (test_key(Key::left) and page_ > 0 and
            (not filter_begin_ or (filter_begin_ and filter_begin_ < page_))) {
            load_page(pfrm, --page_);
        }

        if (app.player().key_down(pfrm, Key::action_2)) {
            if (state_ == State::quickview) {
                if (next_scene_) {
                    return (*next_scene_)();
                }
                return scene_pool::alloc<TitleScreenScene>(3);
            } else {
                state_ = State::show_categories;
                for (int x = 0; x < 30; ++x) {
                    for (int y = 0; y < 20; ++y) {
                        pfrm.set_tile(Layer::overlay, x, y, 0);
                    }
                }
                Text::platform_retain_alphabet(pfrm);
                load_categories(pfrm);
            }
        }
        break;
    }


    return null_scene();
}



GlossaryViewerModule::Factory GlossaryViewerModule::factory_;



} // namespace skyland
