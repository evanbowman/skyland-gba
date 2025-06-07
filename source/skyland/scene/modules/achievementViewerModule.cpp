////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "achievementViewerModule.hpp"
#include "skyland/achievement.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u16 room_category_icon(Room::Category category);



void AchievementViewerModule::load_page(int page)
{
    const auto achievement = (achievements::Achievement)(page + 1);

    auto mt = load_metaclass(achievements::reward(achievement));
    if (not mt) {
        // TODO: fatal error?
        return;
    }

    auto icon = (*mt)->unsel_icon();
    draw_image(181, 1, 15, 4, 4, Layer::overlay);
    PLATFORM.load_overlay_chunk(181, icon, 16);

    if (not item_name_) {
        item_name_.emplace(OverlayCoord{6, 15});
    }

    for (int x = 1; x < 29; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 12, 377);
    }

    if (not unlocks_text_) {
        unlocks_text_.emplace("Unlocks:", OverlayCoord{1, 13});
    }

    if (not achievement_name_) {
        achievement_name_.emplace(OverlayCoord{1, 5});
    }

    StringBuffer<30> temp;
    temp += loadstr(achievements::name(achievement))->c_str();
    achievement_name_->assign(temp.c_str());

    if (is_unlocked(achievement)) {
        PLATFORM.set_tile(Layer::overlay, 28, 5, 378);
    } else {
        PLATFORM.set_tile(Layer::overlay, 28, 5, 112);
    }

    temp.clear();

    PLATFORM.set_tile(
        Layer::overlay, 28, 15, room_category_icon((*mt)->category()));

    temp += (*mt)->ui_name()->c_str();

    item_name_->assign(temp.c_str());

    temp.clear();


    if (not item_details_) {
        item_details_.emplace(OverlayCoord{6, 17});
    }

    temp += stringify((*mt)->cost());
    temp += "@ ";
    temp += stringify((*mt)->consumes_power());
    temp += "` ";
    temp += stringify((*mt)->full_health());
    temp += "hp";

    item_details_->assign(temp.c_str());

    StringBuffer<512> description =
        loadstr(achievements::description(achievement))->c_str();

    if (not achievement_description_) {
        achievement_description_.emplace();
    }

    achievement_description_->assign(
        description.c_str(), OverlayCoord{1, 8}, OverlayCoord{28, 4});

    for (int x = 0; x < 30; ++x) {
        for (int y = 4; y < 20; ++y) {
            if (PLATFORM.get_tile(Layer::overlay, x, y) == 0) {
                PLATFORM.set_tile(Layer::overlay, x, y, 112);
            }
        }
    }
}



void AchievementViewerModule::enter(Scene& prev)
{
    achievements_heading_.emplace(OverlayCoord{0, 1});

    const auto banner_color =
        Text::OptColors{{ColorConstant::rich_black, custom_color(0xead873)}};

    achievements_heading_->append(" ", banner_color);
    achievements_heading_->append(SYSTR(module_achievements)->c_str(),
                                  banner_color);

    PLATFORM.set_tile(Layer::overlay, achievements_heading_->len(), 1, 476);

    for (int x = 0; x < achievements_heading_->len() + 1; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 0, 477);
    }

    load_page(0);
    PLATFORM.screen().fade(1.f, custom_color(0x39395a));

    int count = 0;
    for (int i = 0; i < achievements::count; ++i) {
        if (achievements::is_unlocked((achievements::Achievement)i)) {
            ++count;
        }
    }

    auto count_str = format("(%/%)", count, achievements::count);
    count_text_.emplace(OverlayCoord{u8(29 - count_str.length()), 1});
    count_text_->assign(
        count_str.c_str(),
        Text::OptColors{{custom_color(0xead873), custom_color(0x39395a)}});

    // TODO: remove screen fade entirely, we want to show a banner across the
    // top of the achievements page.

    PLATFORM.speaker().set_music_volume(10);
}



void AchievementViewerModule::exit(Scene& next)
{
    PLATFORM.screen().fade(1.f);

    count_text_.reset();
    achievements_heading_.reset();
    item_name_.reset();
    item_details_.reset();
    achievement_description_.reset();
    unlocks_text_.reset();
    achievement_name_.reset();

    PLATFORM.fill_overlay(0);

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



ScenePtr AchievementViewerModule::update(Time delta)
{

    APP.player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::right) and
        // -1 because we skip the first Achievement::none enumeration
        page_ < achievements::count - 2) {
        load_page(++page_);
    }

    if (test_key(Key::left) and page_ > 0) {
        load_page(--page_);
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<TitleScreenScene>(3);
    }


    return null_scene();
}



AchievementViewerModule::Factory AchievementViewerModule::factory_;



} // namespace skyland
