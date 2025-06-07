////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    SetupPistonScene(RoomCoord piston_loc, bool near)
        : NotificationScene(SYSTR(piston_setup)->c_str(),
                            make_deferred_scene<ReadyScene>()),
          piston_loc_(piston_loc)
    {
        if (not near) {
            far_camera();
        }
    }


    ScenePtr update(Time delta) override
    {
        if (auto scene = ActiveWorldScene::update(delta)) {
            return scene;
        }

        Room* room = nullptr;

        if (is_far_camera()) {
            if (opponent_island()) {
                room = opponent_island()->get_room(piston_loc_);
            }
        } else {
            room = player_island().get_room(piston_loc_);
        }

        Piston* piston = room->cast<Piston>();

        auto last_dir = dir_;

        if (not piston or player().key_down(Key::action_1)) {
            if (is_far_camera()) {
                return make_scene<InspectP2Scene>();
            } else {
                return make_scene<ReadyScene>();
            }
        } else if (player().key_down(Key::left)) {
            dir_ = Piston::Direction::left;
        } else if (player().key_down(Key::right)) {
            dir_ = Piston::Direction::right;
        } else if (player().key_down(Key::up)) {
            dir_ = Piston::Direction::up;
        } else if (player().key_down(Key::down)) {
            dir_ = Piston::Direction::down;
        }

        if (dir_ not_eq last_dir) {
            piston->set_direction(dir_);
            piston->parent()->schedule_repaint();
        }

        return null_scene();
    }


private:
    RoomCoord piston_loc_;
    Piston::Direction dir_ = Piston::Direction::right;
};



} // namespace skyland
