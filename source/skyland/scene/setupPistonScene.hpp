#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "inspectP2Scene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/rooms/piston.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class SetupPistonScene : public NotificationScene
{
public:
    SetupPistonScene(Platform& pfrm, Vec2<u8> piston_loc, bool near) :
        NotificationScene(SYSTR(piston_setup)->c_str(),
            scene_pool::make_deferred_scene<ReadyScene>()),
        piston_loc_(piston_loc)
    {
        if (not near) {
            far_camera();
        }
    }


    ScenePtr<Scene> update(Platform& pfrm,
                           App& app,
                           Microseconds delta) override
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




}
