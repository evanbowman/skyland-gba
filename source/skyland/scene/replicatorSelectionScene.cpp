#include "replicatorSelectionScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



static const auto replicator_fee = 700;



void ReplicatorSelectionScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    if (not near_) {
        far_camera();
    }

    auto st = calc_screen_tiles(pfrm);
    StringBuffer<30> text(SYSTR(create_replicant)->c_str());
    text += stringify(replicator_fee);
    text += "@";

    text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    persist_ui();
}



void ReplicatorSelectionScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    const auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        pfrm.set_tile(Layer::overlay, x, st.y - 1, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 2, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 4, 0);
    }
}



ScenePtr<Scene>
ReplicatorSelectionScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (app.coins() < replicator_fee) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return scene_pool::alloc<ReadyScene>();
        }
    } else {
        auto& cursor_loc =
            near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
                  : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

        Island* island = near_ ? &app.player_island() : app.opponent_island();

        if (app.player().key_down(pfrm, Key::action_1)) {
            exit_countdown_ = milliseconds(500);
            if (auto room = island->get_room(cursor_loc)) {
                if (room->create_replicant(pfrm, app)) {
                    app.set_coins(pfrm, app.coins() - replicator_fee);
                }
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
