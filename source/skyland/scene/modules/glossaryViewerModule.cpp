#include "glossaryViewerModule.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



u16 room_category_icon(Room::Category category);



void GlossaryViewerModule::load_page(Platform& pfrm, int page)
{
    auto [mt, ms] = room_metatable();

    if (page >= ms) {
        Platform::fatal("glossary: invalid index");
    }

    auto icon = mt[page]->unsel_icon();
    draw_image(pfrm, 181, 1, 1, 4, 4, Layer::overlay);
    pfrm.load_overlay_chunk(181, icon, 16);

    if (not item_name_) {
        item_name_.emplace(pfrm, OverlayCoord{6, 1});
    }


    pfrm.set_tile(
        Layer::overlay, 28, 1, room_category_icon(mt[page]->category()));

    StringBuffer<30> temp;
    temp += mt[page]->name();
    temp += " (";
    temp += stringify(mt[page]->size().x);
    temp += ",";
    temp += stringify(mt[page]->size().y);
    temp += ")";

    item_name_->assign(temp.c_str());

    temp.clear();


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
        mt[page]->format_description(description);
    } else {
        description = "Locked! See achievements!";
    }

    if (not item_description_) {
        item_description_.emplace(pfrm);
    }

    item_description_->assign(
        description.c_str(), OverlayCoord{1, 7}, OverlayCoord{28, 12});
}



void GlossaryViewerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    load_page(pfrm, 0);
    pfrm.screen().fade(0.95f);
    pfrm.screen().fade(1.f);
}



void GlossaryViewerModule::exit(Platform& pfrm, App& app, Scene& next)
{
    item_name_.reset();
    item_details_.reset();
    item_description_.reset();

    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
GlossaryViewerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto [mt, ms] = room_metatable();


    if (app.player().key_down(pfrm, Key::right) and page_ < ms - 1 and
        page_ < plugin_rooms_begin() - 1) {
        load_page(pfrm, ++page_);
    }

    if (app.player().key_down(pfrm, Key::left) and page_ > 0) {
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
