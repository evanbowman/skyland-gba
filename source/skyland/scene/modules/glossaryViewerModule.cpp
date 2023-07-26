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
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/captainSelectScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u16 room_category_icon(Room::Category category);



static const auto cg_highlight_colors =
    Text::OptColors{{custom_color(0xffffff), custom_color(0x406e98)}};



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

    if (inspect_ or is_enabled((MetaclassIndex)page)) {
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



s8 last_cover_img_ = -1;



void GlossaryViewerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (state_ not_eq State::quickview) {
        pfrm.screen().set_shader(passthrough_shader);
    }

    if (last_cover_img_ == -1) {
        cover_img_ = 3;
    } else {
        do {
            cover_img_ = rng::choice<5>(rng::utility_state);
        } while (cover_img_ == last_cover_img_);
    }

    last_cover_img_ = cover_img_;


    if (state_ == State::quickview) {
        load_page(pfrm, page_);
    } else {
        for (int x = 0; x < 32; ++x) {
            for (int y = 0; y < 32; ++y) {
                pfrm.set_raw_tile(Layer::map_0, x, y, 1);
            }
        }
        load_categories(pfrm);
    }

    if (state_ not_eq State::quickview) {
        for (int x = 15; x < 32; ++x) {
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, x, y, 112);
            }
        }
    }

    pfrm.screen().set_view(View{});
    pfrm.set_scroll(Layer::map_0_ext, 0, 0);

    pfrm.screen().fade(0.95f);
    pfrm.screen().fade(1.f);

    pfrm.speaker().set_music_volume(10);

    Text::platform_retain_alphabet(pfrm);
    Text::print(pfrm, "0123456789()[].", OverlayCoord{0, 21});

    pfrm.screen().clear();
    pfrm.screen().display();
    pfrm.delta_clock().reset();
}



void GlossaryViewerModule::exit(Platform& pfrm, App& app, Scene& next)
{
    if (state_ not_eq State::quickview) {
        pfrm.screen().set_shader(app.environment().shader(app));
        pfrm.screen().set_shader_argument(0);
    }

    item_name_.reset();
    item_details_.reset();
    item_description_.reset();
    dependency_text_.reset();

    pfrm.fill_overlay(0);

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



static const int filter_opt_count =
    (int)SystemString::filter_end - (int)SystemString::filter_begin;



void GlossaryViewerModule::show_captain(Platform& pfrm, int index)
{
    const auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < st.y; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    pfrm.set_tile(Layer::overlay, 28, 1, 386);

    item_description_.emplace(pfrm);

    pfrm.screen().schedule_fade(0);
    pfrm.screen().schedule_fade(1);

    int offset = (captain_icon((CaptainAbility)index) - 1) * 16;
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

    Text::print(pfrm, captain_name((CaptainAbility)index).c_str(), {7, 5});

    item_description_->assign(
        captain_desc((CaptainAbility)index).c_str(), {1, 9}, {28, 8});

    int captain_count = (int)CaptainAbility::none;

    int pg_margin = (calc_screen_tiles(pfrm).x - captain_count * 2) / 2;
    for (int i = 0; i < captain_count; ++i) {
        if (i == index) {
            pfrm.set_tile(Layer::overlay, pg_margin + i * 2, 18, 160);
        } else {
            pfrm.set_tile(Layer::overlay, pg_margin + i * 2, 18, 159);
        }
    }
}



void GlossaryViewerModule::load_filters(Platform& pfrm)
{
    Text heading(pfrm, OverlayCoord{1, 1});
    heading.assign("- ");
    heading.append(SYSTR(module_glossary)->c_str());
    heading.append(" (");
    heading.append(SYSTR(glossary_filters)->c_str());
    heading.append(")");
    heading.append(" -");
    heading.__detach();

    int row = 4;
    for (int i = 0; i < filter_opt_count; ++i) {
        Text t(pfrm, OverlayCoord{3, (u8)(row + i * 2)});
        auto str = (SystemString)(i + (int)SystemString::filter_begin);
        t.append(loadstr(pfrm, str)->c_str());
        t.__detach();
    }

    pfrm.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 396);
}



void GlossaryViewerModule::draw_category_line(Platform& pfrm,
                                              int line,
                                              Text::OptColors colors)
{
    const u8 y = 4 + line * 2;
    const u8 x = 5;
    Text t(pfrm, OverlayCoord{x, y});
    if (line == (int)Room::Category::count) {
        auto category_str = SYSTR(glossary_captains);
        t.append(category_str->c_str(), colors);
        pfrm.set_tile(Layer::overlay, 3, y, 386);
    } else if (line == (int)Room::Category::count + 1) {
        auto category_str = SYSTR(glossary_filters);
        t.append(category_str->c_str(), colors);

        pfrm.set_tile(Layer::overlay, 3, y, 385);
    } else {
        auto category_str =
            (SystemString)(((int)SystemString::category_begin) + line);
        t.append(loadstr(pfrm, category_str)->c_str(), colors);

        pfrm.set_tile(
            Layer::overlay, 3, y, room_category_icon((Room::Category)line));
    }

    for (int i = t.len(); i < 10; ++i) {
        t.append(" ", colors);
    }
    t.__detach();
}



void GlossaryViewerModule::load_categories(Platform& pfrm)
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 112);
        }
    }

    for (int x = 20; x < 32; ++x) {
        for (int y = 0; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }


    pfrm.screen().clear();
    pfrm.screen().display();

    Text heading(pfrm, OverlayCoord{1, 1});
    heading.assign("- ");
    heading.append(SYSTR(module_glossary)->c_str());
    heading.append(" -");
    heading.__detach();

    int i;
    for (i = 0; i < (int)Room::Category::count; ++i) {
        draw_category_line(pfrm, i);
    }

    draw_category_line(pfrm, i);
    draw_category_line(pfrm, i + 1);

    pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);

    show_category_image(pfrm, cg_cursor_);
}



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void GlossaryViewerModule::show_category_image(Platform& pfrm, int img)
{
    pfrm.system_call("vsync", 0); // fixme

    switch (cover_img_) {
        // FIXME: I was using distinct images for each page of the glossary. But
        // then, I thought that the effect was distracting, so I settled on a
        // single image.
    case 0:
        pfrm.load_tile0_texture("glossary_decoration_cg_flattened");
        break;

    case 1:
        pfrm.load_tile0_texture("glossary_factory_cg_flattened");
        break;

    case 2:
        pfrm.load_tile0_texture("glossary_misc_cg_flattened");
        break;

    case 3:
        pfrm.load_tile0_texture("glossary_power_cg_flattened");
        break;

    case 4:
        pfrm.load_tile0_texture("glossary_wall_cg_flattened");
        break;
    }
    __draw_image(pfrm, 1, 15, 0, 16, 20, Layer::map_0);
    pfrm.screen().schedule_fade(0);

    if (state_ not_eq State::category_transition_enter) {
        for (int x = 16; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, x, y, 0);
            }
        }
    }
}



ScenePtr<Scene> GlossaryViewerModule::show_categories_impl(Platform& pfrm,
                                                           App& app,
                                                           Microseconds delta)
{
    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::up) and cg_cursor_ > 0) {
        draw_category_line(pfrm, cg_cursor_);
        --cg_cursor_;
        pfrm.speaker().play_sound("cursor_tick", 0);
        draw_category_line(pfrm, cg_cursor_, cg_highlight_colors);
        for (int y = 2; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, 1, y, 112);
        }
        pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);
    }

    if (test_key(Key::down) and cg_cursor_ < (int)Room::Category::count + 1) {
        draw_category_line(pfrm, cg_cursor_);
        ++cg_cursor_;
        pfrm.speaker().play_sound("cursor_tick", 0);
        draw_category_line(pfrm, cg_cursor_, cg_highlight_colors);
        for (int y = 2; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, 1, y, 112);
        }
        pfrm.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        state_ = State::category_transition_out;
        pfrm.speaker().play_sound("button_wooden", 3);
        timer_ = 0;
    } else if (app.player().key_down(pfrm, Key::action_2)) {
        state_ = State::fadeout;
        timer_ = 0;
        draw_category_line(pfrm, cg_cursor_);

        for (int y = 20; y < 32; ++y) {
            for (int x = 0; x < 16; ++x) {
                pfrm.set_tile(Layer::overlay, x, y, 112);
            }
            for (int x = 16; x < 30; ++x) {
                pfrm.set_tile(Layer::overlay, x, y, 0);
            }
        }
        pfrm.screen().clear();
        pfrm.set_overlay_origin(0, 1); // FIXME: hack due to offscreen copying
                                       // optimization in gba_platform.cpp
        pfrm.screen().display();
        pfrm.set_overlay_origin(0, 0);
    }

    return null_scene();
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
    case State::captains:
        if ((test_key(Key::left) or test_key(Key::up)) and capn_cursor_ > 0) {
            --capn_cursor_;
            show_captain(pfrm, capn_cursor_);
        }
        if ((test_key(Key::right) or test_key(Key::down)) and
            capn_cursor_ < (int)CaptainAbility::none - 1) {
            ++capn_cursor_;
            show_captain(pfrm, capn_cursor_);
        }
        if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::category_transition_in;
            pfrm.fill_overlay(112);
            pfrm.screen().clear();
            pfrm.screen().display();
            show_category_image(pfrm, cg_cursor_);
            pfrm.fill_overlay(112);
            timer_ = 0;
        }
        break;

    case State::filters:
        if (test_key(Key::up) and filter_cursor_ > 0) {
            --filter_cursor_;
            pfrm.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 483);
        }

        if (test_key(Key::down) and filter_cursor_ < filter_opt_count - 1) {
            ++filter_cursor_;
            pfrm.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, y, 0);
            }

            pfrm.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 483);
        }

        if (app.player().key_down(pfrm, Key::action_1)) {

            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 0);
                }
            }

            auto [mt, ms] = room_metatable();

            filter_buf_ = allocate_dynamic<FilterBuf>("filter-buf");


            static const auto offset = (int)SystemString::filter_begin;
            for (int i = 0; i < ms; ++i) {
                const auto cond = mt[i]->properties();

                switch ((SystemString)(filter_cursor_ + offset)) {
                case SystemString::filter_req_workshop:
                    if (cond & RoomProperties::workshop_required) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_req_manufactory:
                    if (cond & RoomProperties::manufactory_required) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_habitable:
                    if (cond & RoomProperties::habitable) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_ion_damage:
                    if (cond & RoomProperties::accepts_ion_damage) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_highly_flammable:
                    if (cond & RoomProperties::highly_flammable) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_surface_weapons:
                    if (cond & RoomProperties::only_constructible_in_sandbox or
                        cond & RoomProperties::not_constructible) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                default:
                    Platform::fatal("invalid filter option");
                    break;
                }
            }

            if (not(*filter_buf_)->empty()) {
                state_ = State::view_filtered;
                page_ = 0;
                load_page(pfrm, (**filter_buf_)[0]);
            }
        } else if (app.player().key_down(pfrm, Key::action_2) or
                   app.player().key_down(pfrm, Key::left)) {
            state_ = State::category_transition_in;
            pfrm.fill_overlay(112);
            pfrm.screen().clear();
            pfrm.screen().display();
            show_category_image(pfrm, cg_cursor_);
            pfrm.fill_overlay(112);
            timer_ = 0;
        }

        break;

    case State::swap_category_image:
        break;

    case State::category_transition_out: {
        timer_ += delta;
        auto fade_duration = milliseconds(200);
        const auto amt = smoothstep(0.f, fade_duration, timer_);
        pfrm.screen().schedule_fade(
            0.25f * amt, ColorConstant::rich_black, false, false, false);

        s16 scrl = -1 * (amt * 16);
        pfrm.set_scroll(Layer::map_0_ext, scrl, 0);

        int progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 0; i < low; ++i) {
            for (int y = 0; y < 20; ++y) {
                if (pfrm.get_tile(Layer::overlay, 16 + i, y) not_eq 112) {
                    pfrm.set_tile(Layer::overlay, 16 + i, y, 112);
                } else {
                    break;
                }
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (pfrm.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                pfrm.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }


        if (timer_ >= fade_duration) {
            pfrm.set_scroll(Layer::map_0_ext, 0, 0);
            timer_ = 0;
            state_ = State::view;
            if (cg_cursor_ == (int)Room::Category::count) {
                state_ = State::captains;
                capn_cursor_ = 0;
                show_captain(pfrm, 0);
            } else if (cg_cursor_ == (int)Room::Category::count + 1) {
                state_ = State::filters;
                for (int x = 0; x < 30; ++x) {
                    for (int y = 0; y < 20; ++y) {
                        pfrm.set_tile(Layer::overlay, x, y, 0);
                    }
                }
                filter_cursor_ = 0;
                load_filters(pfrm);
                pfrm.screen().schedule_fade(0.5); // wtf? fixme
                pfrm.screen().schedule_fade(1);
            } else {
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
                while (filter_end_ < ms and mt[filter_end_]->category() ==
                                                (Room::Category)cg_cursor_) {
                    ++filter_end_;
                }
                if ((Room::Category)cg_cursor_ == Room::Category::decoration) {
                    filter_end_ = ms;
                }

                load_page(pfrm, page_);


                pfrm.screen().schedule_fade(0.5); // wtf? fixme
                pfrm.screen().schedule_fade(1);
            }
        }
        break;
    }

    case State::show_categories:
        if (auto scn = show_categories_impl(pfrm, app, delta)) {
            return scn;
        }
        break;

    case State::fadeout: {
        timer_ += delta;
        auto fade_duration = milliseconds(250);
        const auto amt = smoothstep(0.f, fade_duration, timer_);

        pfrm.screen().schedule_fade(
            amt, ColorConstant::rich_black, true, true, true);

        s16 scrl = amt * 10;
        pfrm.set_scroll(Layer::map_0_ext, 0, -scrl);
        pfrm.set_overlay_origin(0, -scrl);

        if (timer_ >= fade_duration) {
            state_ = State::exit;
        }
        break;
    }

    case State::exit:
        if (next_scene_) {
            return (*next_scene_)();
        }
        return scene_pool::alloc<TitleScreenScene>(3);
        break;

    case State::view_filtered:
        if ((test_key(Key::right) or test_key(Key::down)) and
            page_ < (int)(*filter_buf_)->size() - 1) {
            load_page(pfrm, (**filter_buf_)[++page_]);
            pfrm.speaker().play_sound("cursor_tick", 0);
        }

        if ((test_key(Key::up) or test_key(Key::left)) and page_ > 0) {
            load_page(pfrm, (**filter_buf_)[--page_]);
            pfrm.speaker().play_sound("cursor_tick", 0);
        }

        if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::filters;
            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 0);
                }
            }
            load_filters(pfrm);
        }
        break;

    case State::view:
    case State::quickview:
        if (not inspect_) {
            if ((test_key(Key::down) or test_key(Key::right)) and
                page_ < ms - 1 and page_ < plugin_rooms_begin() - 1 and
                (not filter_end_ or
                 (filter_end_ and filter_end_ - 1 > page_))) {
                load_page(pfrm, ++page_);
                pfrm.speaker().play_sound("cursor_tick", 0);
            }

            if ((test_key(Key::up) or test_key(Key::left)) and page_ > 0 and
                (not filter_begin_ or
                 (filter_begin_ and filter_begin_ < page_))) {
                load_page(pfrm, --page_);
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (app.player().key_down(pfrm, Key::action_2)) {
            if (state_ == State::quickview) {
                if (next_scene_) {
                    return (*next_scene_)();
                }
                return scene_pool::alloc<TitleScreenScene>(3);
            } else {
                state_ = State::category_transition_in;
                pfrm.fill_overlay(112);
                pfrm.screen().clear();
                pfrm.screen().display();
                show_category_image(pfrm, cg_cursor_);
                pfrm.fill_overlay(112);
                timer_ = 0;
            }
        }
        break;

    case State::category_transition_enter: {
        if (auto scn = show_categories_impl(pfrm, app, delta)) {
            return scn;
        }

        timer_ += delta;
        auto fade_duration = milliseconds(400);
        const auto amt = 1.f - smoothstep(0.f, fade_duration, timer_);

        pfrm.screen().schedule_fade(
            0.45f * amt, ColorConstant::rich_black, false, false, false);

        auto progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 16 + low + 1; i < 30; ++i) {
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, i, y, 0);
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (pfrm.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                pfrm.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }

        if (timer_ >= fade_duration) {
            state_ = State::show_categories;
            for (int i = 16; i < 30; ++i) {
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, i, y, 0);
                }
            }
            draw_category_line(pfrm, cg_cursor_, cg_highlight_colors);
        }
        break;
    }

    case State::category_transition_in: {
        timer_ += delta;
        auto fade_duration = milliseconds(150);
        const auto amt = 1.f - smoothstep(0.f, fade_duration, timer_);

        s16 scrl = -1 * (amt * 4);
        pfrm.set_scroll(Layer::map_0_ext, scrl, 0);


        pfrm.screen().schedule_fade(
            0.45f * amt, ColorConstant::rich_black, false, false, false);

        auto progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 16 + low + 1; i < 30; ++i) {
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, i, y, 0);
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (pfrm.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                pfrm.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }

        if (timer_ >= fade_duration) {
            show_category_image(pfrm, cg_cursor_);
            state_ = State::show_categories;
            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 0);
                }
            }
            Text::platform_retain_alphabet(pfrm);
            Text::print(pfrm, "0123456789()[].", OverlayCoord{0, 21});
            load_categories(pfrm);
            draw_category_line(pfrm, cg_cursor_, cg_highlight_colors);
        }

        break;
    }
    }


    return null_scene();
}



GlossaryViewerModule::Factory GlossaryViewerModule::factory_;



} // namespace skyland
