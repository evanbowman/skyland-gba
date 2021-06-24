#include "inspectP2Scene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



void InspectP2Scene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    cursor_loc.x = 0;
    cursor_loc.y = std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y;

    far_camera();
}


void InspectP2Scene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    room_description_.reset();
}



ScenePtr<Scene>
InspectP2Scene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }


    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }


    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    if (pfrm.keyboard().down_transition<Key::left>()) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }

    if (pfrm.keyboard().down_transition<Key::right>()) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
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


    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = 0;

            if (auto room = app.opponent_island()->get_room(cursor_loc)) {

                if (room->description_visible()) {
                    auto metac = room->metaclass();
                    room_description_.reset();
                    room_description_.emplace(pfrm,
                                              OverlayCoord{0,
                                                  u8(calc_screen_tiles(pfrm).y - 1)});
                    room_description_->append("(");
                    room_description_->append((*metac)->name());
                    room_description_->append(") ");
                    room_description_->append(room->health());
                    room_description_->append("/");
                    room_description_->append(room->max_health());
                } else {
                    room_description_.emplace(pfrm,
                                              OverlayCoord{0,
                                                  u8(calc_screen_tiles(pfrm).y - 1)});

                    room_description_->assign("(??"); // Split to avoid trigraph
                    room_description_->append("?) ??");
                    room_description_->append("?/???");
                }
            }
        }
    }

    return null_scene();
}



void InspectP2Scene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (app.opponent_island()) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(15 + cursor_anim_frame_);

        auto origin = app.opponent_island()->origin();

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);

        pfrm.screen().draw(cursor);
    }
}


} // namespace skyland
