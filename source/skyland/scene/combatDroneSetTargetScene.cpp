#include "combatDroneSetTargetScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene>
CombatDroneSetTargetScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    auto drone_sp = drone_.promote();
    if (not drone_sp) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto exit_scene = [&]() -> ScenePtr<Scene> {
        if ((*drone_sp)->destination() == &app.player_island()) {
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_ =
                (*drone_sp)->position();
            return scene_pool::alloc<ReadyScene>();
        } else {
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_ =
                (*drone_sp)->position();
            return scene_pool::alloc<InspectP2Scene>();
        }
    };

    if (targets_.empty()) {
        return exit_scene();
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return exit_scene();
    }

    if (app.player().key_down(pfrm, Key::action_1)) {

        network::packet::DroneSetTarget packet;
        packet.drone_x_ = (*drone_sp)->position().x;
        packet.drone_y_ = (*drone_sp)->position().y;
        packet.target_x_ = cursor_loc_.x;
        packet.target_y_ = cursor_loc_.y;
        packet.drone_near_ = (*drone_sp)->destination() == &app.player_island();
        packet.target_near_ = near_;
        network::transmit(pfrm, packet);

        (*drone_sp)->set_target(cursor_loc_, near_);

        return exit_scene();
    }

    if (app.player().key_down(pfrm, Key::right)) {
        ++selector_;
        if (selector_ >= (int)targets_.size()) {
            selector_ = 0;
        }
    }

    if (app.player().key_down(pfrm, Key::left)) {
        --selector_;
        if (selector_ < 0) {
            selector_ = targets_.size() - 1;
        }
    }

    if (auto target_sp = targets_[selector_].promote()) {
        auto loc = (*target_sp)->position();
        cursor_loc_ = loc;

        if ((*target_sp)->destination() == &app.player_island()) {
            near_camera();
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_ = loc;
            near_ = true;
        } else {
            far_camera();
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_ = loc;
            near_ = false;
        }
    }


    return null_scene();
}



void CombatDroneSetTargetScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    auto drone = drone_.promote();

    auto collect = [&](auto& list) {
        for (auto& drone_wp : list) {
            if (auto drone_sp = drone_wp.promote()) {
                if (drone_sp->get() == drone->get()) {
                    continue;
                }
                if ((*drone_sp)->parent() not_eq (*drone)->parent()) {
                    targets_.push_back(*drone_sp);
                }
            }
        }
    };

    if (drone) {
        near_ = (*drone)->destination() == &app.player_island();
        if (not near_) {
            far_camera();
        }
    }

    if (near_) {
        if (app.opponent_island()) {
            collect(app.opponent_island()->drones());
        }
        collect(app.player_island().drones());
    } else {
        collect(app.player_island().drones());
        if (app.opponent_island()) {
            collect(app.opponent_island()->drones());
        }
    }
}



void CombatDroneSetTargetScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);
}



void CombatDroneSetTargetScene::display(Platform& pfrm, App& app)
{
    if (targets_.empty()) {
        WorldScene::display(pfrm, app);
        return;
    }

    if (not app.opponent_island()) {
        WorldScene::display(pfrm, app);
        return;
    }

    Island* island;
    if (near_) {
        island = &app.player_island();
    } else {
        island = &*app.opponent_island();
    }

    auto origin = island->origin();

    origin.x += cursor_loc_.x * 16;
    origin.y += cursor_loc_.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index(17);
    sprite.set_size(Sprite::Size::w16_h32);

    pfrm.screen().draw(sprite);

    WorldScene::display(pfrm, app);
}



} // namespace skyland
