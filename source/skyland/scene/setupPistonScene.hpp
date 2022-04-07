////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "inspectP2Scene.hpp"
#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/rooms/piston.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class SetupPistonScene : public NotificationScene
{
public:
    SetupPistonScene(Platform& pfrm, Vec2<u8> piston_loc, bool near)
        : NotificationScene(SYSTR(piston_setup)->c_str(),
                            scene_pool::make_deferred_scene<ReadyScene>()),
          piston_loc_(piston_loc)
    {
        if (not near) {
            far_camera();
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
            return scene;
        }

        Room* room = nullptr;

        if (is_far_camera()) {
            if (opponent_island(app)) {
                room = opponent_island(app)->get_room(piston_loc_);
            }
        } else {
            room = player_island(app).get_room(piston_loc_);
        }

        Piston* piston = dynamic_cast<Piston*>(room);

        auto last_dir = dir_;

        if (not piston or player(app).key_down(pfrm, Key::action_1)) {
            if (is_far_camera()) {
                return scene_pool::alloc<InspectP2Scene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
            }
        } else if (player(app).key_down(pfrm, Key::left)) {
            dir_ = Piston::Direction::left;
        } else if (player(app).key_down(pfrm, Key::right)) {
            dir_ = Piston::Direction::right;
        } else if (player(app).key_down(pfrm, Key::up)) {
            dir_ = Piston::Direction::up;
        } else if (player(app).key_down(pfrm, Key::down)) {
            dir_ = Piston::Direction::down;
        }

        if (dir_ not_eq last_dir) {
            piston->set_direction(dir_);
            piston->parent()->schedule_repaint();
        }

        return null_scene();
    }


private:
    Vec2<u8> piston_loc_;
    Piston::Direction dir_ = Piston::Direction::right;
};



} // namespace skyland
