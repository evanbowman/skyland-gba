////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
                            scene_pool::make_deferred_scene<ReadyScene>()),
          piston_loc_(piston_loc)
    {
        if (not near) {
            far_camera();
        }
    }


    ScenePtr<Scene> update(Microseconds delta) override
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
                return scene_pool::alloc<InspectP2Scene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
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
