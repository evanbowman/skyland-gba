#include "readyScene.hpp"
#include "constructionScene.hpp"
#include "fadeOutScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "platform/platform.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "worldScene.hpp"



namespace skyland {



ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (app.exit_level()) {
        app.exit_level() = false;
        return scene_pool::alloc<FadeOutScene>();
    }

    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ConstructionScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;


    if (pfrm.keyboard().down_transition<Key::left>()) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        }
    }

    if (pfrm.keyboard().down_transition<Key::right>()) {
        if (cursor_loc.x < app.player_island().terrain().size()) {
            ++cursor_loc.x;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    if (pfrm.keyboard().down_transition<Key::up>()) {
        if (cursor_loc.y > 6) {
            --cursor_loc.y;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        }
    }

    if (pfrm.keyboard().down_transition<Key::down>()) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            return room->select(pfrm, app);
        }
    }

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        if (app.player_island().get_room(cursor_loc)) {
            return scene_pool::alloc<SalvageRoomScene>();
        }
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            if (auto room = app.player_island().get_room(cursor_loc)) {
                if (not room_description_) {
                    room_description_.emplace(pfrm,
                                              OverlayCoord{0,
                                                  u8(calc_screen_tiles(pfrm).y - 1)});
                }
                int i = 0;
                if (length(room->characters())) {
                    room_description_->erase();
                    for (auto& chr : room->characters()) {
                        if (chr->grid_position() == cursor_loc) {
                            if (i > 0) {
                                room_description_->append(",");
                            }
                            Text::OptColors opts;
                            if (chr->owner() == &app.player()) {
                                opts = {
                                    custom_color(0xff6675),
                                    ColorConstant::rich_black
                                };
                                room_description_->append("(human) ", opts);
                            } else {
                                opts = {
                                    custom_color(0xcf54ff),
                                    ColorConstant::rich_black
                                };
                                room_description_->append("(goblin) ", opts);

                            }
                            room_description_->append(chr->health() / 10);
                            room_description_->append("/");
                            room_description_->append(25);
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
                    room_description_->assign(desc.c_str());
                    room_description_->append(room->health());
                    room_description_->append("/");
                    room_description_->append(room->max_health());
                }

            }
        }
    }

    return null_scene();
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



void ReadyScene::exit(Platform&, App&, Scene& next)
{
    room_description_.reset();
}



} // namespace skyland
