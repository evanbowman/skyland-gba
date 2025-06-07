////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "glossaryViewerModule.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u16 room_category_icon(Room::Category category);



static const auto cg_highlight_colors =
    Text::OptColors{{custom_color(0xffffff), custom_color(0x406e98)}};



void GlossaryViewerModule::load_page(int page)
{
    auto [mt, ms] = room_metatable();

    auto icon = mt[page]->unsel_icon();
    draw_image(181, 1, 1, 4, 4, Layer::overlay);
    PLATFORM.load_overlay_chunk(181, icon, 16);

    if (not item_name_) {
        item_name_.emplace(OverlayCoord{6, 1});
    }


    PLATFORM.set_tile(
        Layer::overlay, 28, 1, room_category_icon(mt[page]->category()));

    StringBuffer<30> temp;
    temp += mt[page]->ui_name()->c_str();
    temp += " (";
    temp += stringify(mt[page]->size().x);
    temp += "x";
    temp += stringify(mt[page]->size().y);
    temp += ")";

    item_name_->assign(temp.c_str());

    temp.clear();

    if (not dependency_text_) {
        dependency_text_.emplace(OverlayCoord{1, 18});
    }

    const auto cond = mt[page]->properties();

    bool ingame_glossary = static_cast<bool>(next_scene_);

    bool cannot_build_human_excl = (cond & RoomProperties::human_only) and
                                   APP.faction() not_eq Faction::human;

    bool cannot_build_goblin_excl = (cond & RoomProperties::goblin_only) and
                                    APP.faction() not_eq Faction::goblin;

    bool cannot_build_sylph_excl = (cond & RoomProperties::sylph_only) and
                                   APP.faction() not_eq Faction::sylph;

    auto dependency_colors =
        FontColors{ColorConstant::med_blue_gray, ColorConstant::rich_black};


    if (ingame_glossary and
        (cannot_build_human_excl or cannot_build_goblin_excl or
         cannot_build_sylph_excl)) {
        if (cannot_build_human_excl) {
            dependency_text_->assign(SYS_CSTR(glossary_human_only),
                                     dependency_colors);
        } else if (cannot_build_goblin_excl) {
            dependency_text_->assign(SYS_CSTR(glossary_goblin_only),
                                     dependency_colors);
        } else if (cannot_build_sylph_excl) {
            dependency_text_->assign(SYS_CSTR(glossary_sylph_only),
                                     dependency_colors);
        }
    } else if (cond & RoomProperties::workshop_required) {
        dependency_text_->assign(SYS_CSTR(glossary_workshop_required),
                                 dependency_colors);
    } else if (cond & RoomProperties::manufactory_required) {
        dependency_text_->assign(SYS_CSTR(glossary_manufactory_required),
                                 dependency_colors);
    } else {
        dependency_text_.reset();
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 18, 0);
        }
    }


    if (not item_details_) {
        item_details_.emplace(OverlayCoord{6, 3});
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
        mt[page]->format_description(description);
    } else {
        description = "Locked! See achievements!";
    }

    if (not item_description_) {
        item_description_.emplace();
    }

    item_description_->assign(
        description.c_str(), OverlayCoord{1, 6}, OverlayCoord{28, 11});
}



s8 last_cover_img = -1;



void GlossaryViewerModule::enter(Scene& prev)
{
    PLATFORM.fill_overlay(0);
    if (state_ not_eq State::quickview) {
        PLATFORM.screen().set_shader(passthrough_shader);
    }

    if (last_cover_img == -1) {
        cover_img_ = 3;
    } else {
        do {
            cover_img_ = rng::choice<5>(rng::utility_state);
        } while (cover_img_ == last_cover_img);
    }
    last_cover_img = cover_img_;


    if (state_ == State::quickview) {
        load_page(page_);
    } else {
        for (int x = 0; x < 32; ++x) {
            for (int y = 0; y < 32; ++y) {
                PLATFORM.set_raw_tile(Layer::map_0, x, y, 1);
            }
        }
        load_categories();
    }

    if (state_ not_eq State::quickview) {
        for (int x = 15; x < 32; ++x) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 112);
            }
        }
    }

    PLATFORM.screen().set_view(View{});
    PLATFORM.set_scroll(Layer::map_0_ext, 0, 0);

    PLATFORM.screen().schedule_fade(0.95f);
    PLATFORM.screen().schedule_fade(1.f);

    PLATFORM.speaker().set_music_volume(10);

    PLATFORM.screen().clear();
    PLATFORM.screen().display();
    PLATFORM.delta_clock().reset();
}



void set_gamespeed(GameSpeed speed);



void GlossaryViewerModule::exit(Scene& next)
{
    if (state_ not_eq State::quickview) {
        PLATFORM.screen().set_shader(APP.environment().shader());
        PLATFORM.screen().set_shader_argument(0);
    }

    item_name_.reset();
    item_details_.reset();
    item_description_.reset();
    dependency_text_.reset();

    PLATFORM.fill_overlay(0);

    if (disable_fade_on_exit_) {
        PLATFORM.screen().schedule_fade(0);
    }

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    if (APP.game_speed() not_eq GameSpeed::normal) {
        set_gamespeed(APP.game_speed());
    }
}



static const int filter_opt_count =
    (int)SystemString::filter_end - (int)SystemString::filter_begin;



void GlossaryViewerModule::load_filters()
{
    Text heading(OverlayCoord{1, 1});
    heading.assign("- ");
    heading.append(SYSTR(module_glossary)->c_str());
    heading.append(" (");
    heading.append(SYSTR(glossary_filters)->c_str());
    heading.append(")");
    heading.append(" -");
    heading.__detach();

    int row = 4;
    for (int i = 0; i < filter_opt_count; ++i) {
        Text t(OverlayCoord{3, (u8)(row + i * 2)});
        auto str = (SystemString)(i + (int)SystemString::filter_begin);
        t.append(loadstr(str)->c_str());
        t.__detach();
    }

    PLATFORM.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 396);
}



void GlossaryViewerModule::draw_category_line(int line, Text::OptColors colors)
{
    int offset = 0;
    if (line == (int)Room::Category::count) {
        ++offset;
    }
    const u8 y = offset + 4 + line * 2;
    const u8 x = 5;
    Text t(OverlayCoord{x, y});
    if (line == (int)Room::Category::count) {
        ++offset;
        auto category_str = SYSTR(glossary_filters);
        t.append(category_str->c_str(), colors);

        PLATFORM.set_tile(Layer::overlay, 3, y, 385);

    } else {
        auto category_str =
            (SystemString)(((int)SystemString::category_begin) + line);
        t.append(loadstr(category_str)->c_str(), colors);

        PLATFORM.set_tile(
            Layer::overlay, 3, y, room_category_icon((Room::Category)line));
    }

    for (int i = t.len(); i < 10; ++i) {
        t.append(" ", colors);
    }
    t.__detach();
}



void GlossaryViewerModule::load_categories()
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 112);
        }
    }

    for (int x = 20; x < 32; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }


    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    Text heading(OverlayCoord{1, 1});
    heading.assign("- ");
    heading.append(SYSTR(module_glossary)->c_str());
    heading.append(" -");
    heading.__detach();

    int i;
    for (i = 0; i < (int)Room::Category::count; ++i) {
        draw_category_line(i);
    }

    draw_category_line(i);

    if (cg_cursor_ == (int)Room::Category::count) {
        PLATFORM.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2 + 1, 483);
    } else {
        PLATFORM.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);
    }

    show_category_image(cg_cursor_);
}



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void GlossaryViewerModule::show_category_image(int img)
{
    PLATFORM_EXTENSION(force_vsync);

    switch (cover_img_) {
        // FIXME: I was using distinct images for each page of the glossary. But
        // then, I thought that the effect was distracting, so I settled on a
        // single image.
    case 0:
        PLATFORM.load_tile0_texture("glossary_decoration_cg_flattened");
        break;

    case 1:
        PLATFORM.load_tile0_texture("glossary_factory_cg_flattened");
        break;

    case 2:
        PLATFORM.load_tile0_texture("glossary_misc_cg_flattened");
        break;

    case 3:
        PLATFORM.load_tile0_texture("glossary_power_cg_flattened");
        break;

    case 4:
        PLATFORM.load_tile0_texture("glossary_wall_cg_flattened");
        break;
    }
    __draw_image(1, 15, 0, 16, 20, Layer::map_0);
    PLATFORM.screen().schedule_fade(0);

    if (state_ not_eq State::category_transition_enter) {
        for (int x = 16; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
    }
}



ScenePtr GlossaryViewerModule::show_categories_impl(Time delta)
{
    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::up) and cg_cursor_ > 0) {
        draw_category_line(cg_cursor_);
        --cg_cursor_;
        PLATFORM.speaker().play_sound("cursor_tick", 0);
        draw_category_line(cg_cursor_, cg_highlight_colors);
        for (int y = 2; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 112);
        }
        PLATFORM.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);
    }

    if (test_key(Key::down) and cg_cursor_ < (int)Room::Category::count) {
        draw_category_line(cg_cursor_);
        ++cg_cursor_;
        PLATFORM.speaker().play_sound("cursor_tick", 0);
        draw_category_line(cg_cursor_, cg_highlight_colors);
        for (int y = 2; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 112);
        }
        if (cg_cursor_ == (int)Room::Category::count) {
            PLATFORM.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2 + 1, 483);
        } else {
            PLATFORM.set_tile(Layer::overlay, 1, 4 + cg_cursor_ * 2, 483);
        }
    }

    if (APP.player().key_down(Key::action_1)) {
        state_ = State::category_transition_out;
        PLATFORM.speaker().play_sound("button_wooden", 3);
        timer_ = 0;
    } else if (APP.player().key_down(Key::action_2)) {
        state_ = State::fadeout;
        timer_ = 0;
        draw_category_line(cg_cursor_);

        for (int y = 20; y < 32; ++y) {
            for (int x = 0; x < 16; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, y, 112);
            }
            for (int x = 16; x < 30; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
        PLATFORM.screen().clear();
        PLATFORM.set_overlay_origin(0,
                                    1); // FIXME: hack due to offscreen copying
                                        // optimization in gba_platform.cpp
        PLATFORM.screen().display();
        PLATFORM.set_overlay_origin(0, 0);
    }

    return null_scene();
}



ScenePtr GlossaryViewerModule::update(Time delta)
{
    auto [mt, ms] = room_metatable();

    APP.player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };


    switch (state_) {
    case State::filters:
        if (test_key(Key::up) and filter_cursor_ > 0) {
            --filter_cursor_;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 1, y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 483);
        }

        if (test_key(Key::down) and filter_cursor_ < filter_opt_count - 1) {
            ++filter_cursor_;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            for (int y = 2; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 1, y, 0);
            }

            PLATFORM.set_tile(Layer::overlay, 1, 4 + filter_cursor_ * 2, 483);
        }

        if (APP.player().key_down(Key::action_1)) {

            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 0);
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

                case SystemString::filter_sylph_exclusive:
                    if (cond & RoomProperties::sylph_only) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_goblin_exclusive:
                    if (cond & RoomProperties::goblin_only) {
                        (*filter_buf_)->push_back(i);
                    }
                    break;

                case SystemString::filter_human_exclusive:
                    if (cond & RoomProperties::human_only) {
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
                load_page((**filter_buf_)[0]);
            }
        } else if (APP.player().key_down(Key::action_2) or
                   APP.player().key_down(Key::left)) {
            state_ = State::category_transition_in;
            PLATFORM.fill_overlay(112);
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            show_category_image(cg_cursor_);
            PLATFORM.fill_overlay(112);
            timer_ = 0;
        }

        break;

    case State::swap_category_image:
        break;

    case State::category_transition_out: {
        timer_ += delta;
        auto fade_duration = milliseconds(200);
        const auto amt = smoothstep(0.f, fade_duration, timer_);
        PLATFORM.screen().schedule_fade(
            0.25f * amt, ColorConstant::rich_black, false, false, false);

        s16 scrl = -1 * (amt * 16);
        PLATFORM.set_scroll(Layer::map_0_ext, scrl, 0);

        int progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 0; i < low; ++i) {
            for (int y = 0; y < 20; ++y) {
                if (PLATFORM.get_tile(Layer::overlay, 16 + i, y) not_eq 112) {
                    PLATFORM.set_tile(Layer::overlay, 16 + i, y, 112);
                } else {
                    break;
                }
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (PLATFORM.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                PLATFORM.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }


        if (timer_ >= fade_duration) {
            PLATFORM.set_scroll(Layer::map_0_ext, 0, 0);
            timer_ = 0;
            state_ = State::view;
            if (cg_cursor_ == (int)Room::Category::count) {
                state_ = State::filters;
                for (int x = 0; x < 30; ++x) {
                    for (int y = 0; y < 20; ++y) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 0);
                    }
                }
                filter_cursor_ = 0;
                load_filters();
                PLATFORM.screen().schedule_fade(0.5); // wtf? fixme
                PLATFORM.screen().schedule_fade(1);
            } else {
                for (int x = 0; x < 30; ++x) {
                    for (int y = 0; y < 20; ++y) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 0);
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

                load_page(page_);


                PLATFORM.screen().schedule_fade(0.5); // wtf? fixme
                PLATFORM.screen().schedule_fade(1);
            }
        }
        break;
    }

    case State::show_categories:
        if (auto scn = show_categories_impl(delta)) {
            return scn;
        }
        break;

    case State::fadeout: {
        timer_ += delta;
        auto fade_duration = milliseconds(250);
        const auto amt = smoothstep(0.f, fade_duration, timer_);

        PLATFORM.screen().schedule_fade(
            amt, ColorConstant::rich_black, true, true, true);

        s16 scrl = amt * 10;
        PLATFORM.set_scroll(Layer::map_0_ext, 0, -scrl);
        PLATFORM.set_overlay_origin(0, -scrl);

        if (timer_ >= fade_duration) {
            state_ = State::exit;
        }
        break;
    }

    case State::exit:
        if (next_scene_) {
            return (*next_scene_)();
        }
        return make_scene<TitleScreenScene>(3);
        break;

    case State::view_filtered:
        if ((test_key(Key::right) or test_key(Key::down)) and
            page_ < (int)(*filter_buf_)->size() - 1) {
            load_page((**filter_buf_)[++page_]);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if ((test_key(Key::up) or test_key(Key::left)) and page_ > 0) {
            load_page((**filter_buf_)[--page_]);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if (APP.player().key_down(Key::action_2)) {
            state_ = State::filters;
            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 0);
                }
            }
            load_filters();
        }
        break;

    case State::view:
    case State::quickview:
        if (not inspect_) {
            if ((test_key(Key::down) or test_key(Key::right)) and
                page_ < ms - 1 and page_ < plugin_rooms_begin() - 1 and
                (not filter_end_ or
                 (filter_end_ and filter_end_ - 1 > page_))) {
                load_page(++page_);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }

            if ((test_key(Key::up) or test_key(Key::left)) and page_ > 0 and
                (not filter_begin_ or
                 (filter_begin_ and filter_begin_ < page_))) {
                load_page(--page_);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (APP.player().key_down(Key::action_2)) {
            if (state_ == State::quickview) {
                if (next_scene_) {
                    return (*next_scene_)();
                }
                return make_scene<TitleScreenScene>(3);
            } else {
                state_ = State::category_transition_in;
                PLATFORM.fill_overlay(112);
                PLATFORM.screen().clear();
                PLATFORM.screen().display();
                show_category_image(cg_cursor_);
                PLATFORM.fill_overlay(112);
                timer_ = 0;
            }
        }
        break;

    case State::category_transition_enter: {
        if (auto scn = show_categories_impl(delta)) {
            return scn;
        }

        timer_ += delta;
        auto fade_duration = milliseconds(400);
        const auto amt = 1.f - smoothstep(0.f, fade_duration, timer_);

        PLATFORM.screen().schedule_fade(
            0.45f * amt, ColorConstant::rich_black, false, false, false);

        auto progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 16 + low + 1; i < 30; ++i) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, i, y, 0);
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (PLATFORM.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                PLATFORM.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }

        if (timer_ >= fade_duration) {
            state_ = State::show_categories;
            for (int i = 16; i < 30; ++i) {
                for (int y = 0; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, i, y, 0);
                }
            }
            draw_category_line(cg_cursor_, cg_highlight_colors);
        }
        break;
    }

    case State::category_transition_in: {
        timer_ += delta;
        auto fade_duration = milliseconds(150);
        const auto amt = 1.f - smoothstep(0.f, fade_duration, timer_);

        s16 scrl = -1 * (amt * 4);
        PLATFORM.set_scroll(Layer::map_0_ext, scrl, 0);


        PLATFORM.screen().schedule_fade(
            0.45f * amt, ColorConstant::rich_black, false, false, false);

        auto progress = 8 * 14 * amt;
        auto low = (int)progress / 8;
        auto rem = (int)progress % 8;
        for (int i = 16 + low + 1; i < 30; ++i) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, i, y, 0);
            }
        }
        for (int y = 0; y < 20; ++y) {
            auto t = 433 - rem;
            if (PLATFORM.get_tile(Layer::overlay, 16 + low, y) == t) {
                break;
            } else {
                PLATFORM.set_tile(Layer::overlay, 16 + low, y, t);
            }
        }

        if (timer_ >= fade_duration) {
            show_category_image(cg_cursor_);
            state_ = State::show_categories;
            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 0);
                }
            }
            load_categories();
            draw_category_line(cg_cursor_, cg_highlight_colors);
        }

        break;
    }
    }


    return null_scene();
}



GlossaryViewerModule::Factory GlossaryViewerModule::factory_;



} // namespace skyland
