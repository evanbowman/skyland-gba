#include "achievementViewerModule.hpp"
#include "skyland/achievement.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



u16 room_category_icon(Room::Category category);



void AchievementViewerModule::load_page(Platform& pfrm, App& app, int page)
{
    const auto achievement = (achievements::Achievement)(page + 1);

    auto mt = load_metaclass(achievements::reward(achievement));
    if (not mt) {
        // TODO: fatal error?
        return;
    }

    auto icon = (*mt)->unsel_icon();
    draw_image(pfrm, 181, 1, 15, 4, 4, Layer::overlay);
    pfrm.load_overlay_chunk(181, icon, 16);

    if (not item_name_) {
        item_name_.emplace(pfrm, OverlayCoord{6, 15});
    }

    for (int x = 1; x < 29; ++x) {
        pfrm.set_tile(Layer::overlay, x, 12, 377);
    }

    if (not unlocks_text_) {
        unlocks_text_.emplace(pfrm, "Unlocks:", OverlayCoord{1, 13});
    }

    if (not achievement_name_) {
        achievement_name_.emplace(pfrm, OverlayCoord{1, 5});
    }

    StringBuffer<30> temp;
    temp += achievements::name(achievement);
    achievement_name_->assign(temp.c_str());

    if (is_unlocked(app, achievement)) {
        pfrm.set_tile(Layer::overlay, 28, 5, 378);
    } else {
        pfrm.set_tile(Layer::overlay, 28, 5, 112);
    }

    temp.clear();

    pfrm.set_tile(
        Layer::overlay, 28, 15, room_category_icon((*mt)->category()));

    temp += (*mt)->name();
    temp += " (";
    temp += stringify((*mt)->size().x);
    temp += ",";
    temp += stringify((*mt)->size().y);
    temp += ")";

    item_name_->assign(temp.c_str());

    temp.clear();


    if (not item_details_) {
        item_details_.emplace(pfrm, OverlayCoord{6, 17});
    }

    temp += stringify((*mt)->cost());
    temp += "@ ";
    temp += stringify((*mt)->consumes_power());
    temp += "` ";
    temp += stringify((*mt)->full_health());
    temp += "hp";

    item_details_->assign(temp.c_str());

    StringBuffer<512> description = achievements::description(achievement);

    if (not achievement_description_) {
        achievement_description_.emplace(pfrm);
    }

    achievement_description_->assign(
        description.c_str(), OverlayCoord{1, 8}, OverlayCoord{28, 4});

    for (int x = 0; x < 30; ++x) {
        for (int y = 4; y < 20; ++y) {
            if (pfrm.get_tile(Layer::overlay, x, y) == 0) {
                pfrm.set_tile(Layer::overlay, x, y, 112);
            }
        }
    }
}



void AchievementViewerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    load_page(pfrm, app, 0);
    pfrm.screen().fade(0.95f);
    pfrm.screen().fade(1.f);

    // TODO: remove screen fade entirely, we want to show a banner across the
    // top of the achievements page.
}



void AchievementViewerModule::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.screen().fade(1.f);

    item_name_.reset();
    item_details_.reset();
    achievement_description_.reset();
    unlocks_text_.reset();
    achievement_name_.reset();

    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
AchievementViewerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::right) and
        // -1 because we skip the first Achievement::none enumeration
        page_ < achievements::count - 2) {
        load_page(pfrm, app, ++page_);
    }

    if (app.player().key_down(pfrm, Key::left) and page_ > 0) {
        load_page(pfrm, app, --page_);
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }


    return null_scene();
}



AchievementViewerModule::Factory AchievementViewerModule::factory_;



} // namespace skyland
