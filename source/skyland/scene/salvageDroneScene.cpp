#include "salvageDroneScene.hpp"
#include "readyScene.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void SalvageDroneScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    auto st = calc_screen_tiles(pfrm);
    StringBuffer<30> text("scuttle drone?");

    text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});
    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign("A: yes ");
    no_text_->assign("B:  no ");

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 23; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 3, 128);
}



void SalvageDroneScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    yes_text_.reset();
    no_text_.reset();
    text_.reset();

    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
SalvageDroneScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto drone_sp = drone_.upgrade()) {
            for (auto& room : (*drone_sp)->parent()->rooms()) {
                if (auto db = dynamic_cast<DroneBay*>(room.get())) {
                    auto found = db->drone();
                    if (found and (*found).get() == drone_sp->get()) {
                        medium_explosion(
                            pfrm, app, (*found)->sprite().get_position());
                        db->disconnect_drone();
                        break;
                    }
                }
            }
        }
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
