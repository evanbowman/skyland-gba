#include "readyScene.hpp"
#include "constructionScene.hpp"
#include "fadeOutScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "lispReplScene.hpp"
#include "platform/platform.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "worldScene.hpp"



namespace skyland {



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description)
{
    if (not room_description) {
        return;
    }

    const u8 y = calc_screen_tiles(pfrm).y - 2;

    for (int i = 0; i < room_description->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, y, 0);
    }
    room_description.reset();
}



ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (app.exit_level()) {
        set_gamespeed(pfrm, app, GameSpeed::normal);
        app.exit_level() = false;
        return scene_pool::alloc<FadeOutScene>();
    }

    if (app.player().key_down(pfrm, Key::alt_2)) {
        return scene_pool::alloc<ConstructionScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;



    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (app.player().key_down(pfrm, Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    } else if (app.player().key_down(pfrm, Key::right)) {
        if (cursor_loc.x < app.player_island().terrain().size()) {
            ++cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        } else if (mt_prep_seconds == 0) {
            // Do not allow the player to inspect the other island if we're in
            // the multiplayer waiting room.
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    if (app.player().key_down(pfrm, Key::up)) {
        if (cursor_loc.y > 6) {
            --cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    } else if (app.player().key_down(pfrm, Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            return room->select(pfrm, app);
        }
    }

    if (app.player().key_pressed(pfrm, Key::start)) {
        start_key_held_timer_ += delta;
    }
    if (app.player().key_up(pfrm, Key::start)) {
        if (not pfrm.network_peer().is_connected()) {
            if (start_key_held_timer_ > milliseconds(1500)) {
                return scene_pool::alloc<LispReplScene>(pfrm);
            }
        }
        start_key_held_timer_ = 0;
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (app.player_island().get_room(cursor_loc)) {
            return scene_pool::alloc<SalvageRoomScene>();
        }
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(
                pfrm, app, &app.player_island(), cursor_loc, room_description_);
        }
    }

    return null_scene();
}



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description)
{
    if (auto room = island->get_room(cursor_loc)) {
        if (not room_description) {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
        }
        if (room->parent() == &app.player_island() or
            room->description_visible()) {

            int i = 0;
            if (length(room->characters())) {
                room_description->erase();
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() == cursor_loc) {
                        if (i > 0) {
                            room_description->append(",");
                        }
                        Text::OptColors opts;
                        if (chr->owner() == &app.player()) {
                            opts = {custom_color(0xff6675),
                                    ColorConstant::rich_black};
                            if (chr->is_replicant()) {
                                room_description->append("(replicant) ", opts);
                            } else {
                                room_description->append("(human) ", opts);
                            }
                        } else {
                            opts = {custom_color(0xcf54ff),
                                    ColorConstant::rich_black};
                            room_description->append("(goblin) ", opts);
                        }
                        room_description->append(chr->health() / 10);
                        room_description->append("/");
                        room_description->append(25);
                        ++i;
                    }
                }
            }

            if (i == 0) {
                auto metac = room->metaclass();
                StringBuffer<32> desc;
                desc += "(";
                desc += (*metac)->name();
                desc += ") ";
                room_description->assign(desc.c_str());
                room_description->append(room->health());
                room_description->append("/");
                room_description->append(room->max_health());
                room_description->append(" ");
                room_description->append((*metac)->consumes_power());
                room_description->append("`");
            }

        } else {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});

            room_description->assign("(??"); // Split to avoid trigraph
            room_description->append("?) ??");
            room_description->append("?/???");
        }
    }

    if (room_description) {
        const u8 y = calc_screen_tiles(pfrm).y - 2;

        for (int i = 0; i < 32; ++i) {
            pfrm.set_tile(Layer::overlay, i, y, 0);
        }

        for (int i = 0; i < room_description->len(); ++i) {
            pfrm.set_tile(Layer::overlay, i, y, 425);
        }
    }
}



void ReadyScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    auto origin = app.player_island().origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



void ReadyScene::exit(Platform& pfrm, App&, Scene& next)
{
    clear_room_description(pfrm, room_description_);
}



} // namespace skyland
