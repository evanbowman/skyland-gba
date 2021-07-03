#include "weaponSetTargetScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



WeaponSetTargetScene::WeaponSetTargetScene(const Vec2<u8>& weapon_loc)
    : weapon_loc_(weapon_loc)
{
}



ScenePtr<Scene>
WeaponSetTargetScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (targets_.empty() or not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    if (freeform_) {

        if (key_down<Key::alt_2>(pfrm)) {
            freeform_ = false;
        }

        if (key_down<Key::right>(pfrm)) {
            if (cursor_loc.x < app.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
                room_description_.reset();
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (key_down<Key::down>(pfrm)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                room_description_.reset();
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (key_down<Key::up>(pfrm)) {
            if (cursor_loc.y > 6) {
                --cursor_loc.y;
                room_description_.reset();
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (key_down<Key::left>(pfrm)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                room_description_.reset();
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (key_down<Key::action_1>(pfrm)) {
            if (auto target_room = app.opponent_island()->get_room(cursor_loc)) {
                if (auto room = app.player_island().get_room(weapon_loc_)) {
                    room->set_target(target_room->position());

                    network::packet::WeaponSetTarget packet;
                    packet.weapon_x_ = weapon_loc_.x;
                    packet.weapon_y_ = weapon_loc_.y;
                    packet.target_x_ = target_room->position().x;
                    packet.target_y_ = target_room->position().y;
                    network::transmit(pfrm, packet);
                }
                return scene_pool::alloc<ReadyScene>();
            }
        }
    } else {
        cursor_loc.x = targets_[selector_].x;
        cursor_loc.y = targets_[selector_].y;

        if (key_down<Key::alt_2>(pfrm)) {
            freeform_ = true;
        }

        if (key_down<Key::right>(pfrm) or key_down<Key::down>(pfrm)) {

            if (selector_ < (int)targets_.size() - 1) {
                selector_++;
            } else {
                selector_ = 0;
            }

            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        }

        if (key_down<Key::left>(pfrm) or key_down<Key::up>(pfrm)) {

            if (selector_ > 0) {
                --selector_;
            } else {
                selector_ = targets_.size() - 1;
            }

            room_description_.reset();
            describe_room_timer_ = milliseconds(300);
        }

        if (key_down<Key::action_1>(pfrm)) {
            const auto target = targets_[selector_];
            if (auto room = app.player_island().get_room(weapon_loc_)) {
                room->set_target(target);

                network::packet::WeaponSetTarget packet;
                packet.weapon_x_ = weapon_loc_.x;
                packet.weapon_y_ = weapon_loc_.y;
                packet.target_x_ = target.x;
                packet.target_y_ = target.y;
                network::transmit(pfrm, packet);
            }
            return scene_pool::alloc<ReadyScene>();
        }
    }


    if (key_down<Key::action_2>(pfrm)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            if (app.opponent_island()) {
                describe_room(pfrm,
                              app,
                              &*app.opponent_island(),
                              cursor_loc,
                              room_description_);
            }
        }
    }

    return null_scene();
}


void WeaponSetTargetScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (targets_.empty()) {
        return;
    }

    if (not app.opponent_island()) {
        return;
    }

    auto origin = app.opponent_island()->origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index(17);
    sprite.set_size(Sprite::Size::w16_h32);

    pfrm.screen().draw(sprite);
}



void WeaponSetTargetScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    collect_targets(pfrm, app);

    if (not targets_.empty()) {
        auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
        cursor_loc.x = targets_[selector_].x;
        cursor_loc.y = targets_[selector_].y;
    }

    far_camera();
}



void WeaponSetTargetScene::collect_targets(Platform& pfrm, App& app)
{
    targets_.clear();

    if (app.opponent_island()) {
        Island& island = *app.opponent_island();

        for (auto& room : island.rooms()) {
            targets_.push_back(room->position());
        }
    }

    std::sort(targets_.begin(),
              targets_.end(),
              [](const Vec2<u8>& lhs, const Vec2<u8>& rhs) {
                  return lhs.x < rhs.x || (lhs.x == rhs.x and lhs.y < rhs.y);
              });
}



} // namespace skyland
