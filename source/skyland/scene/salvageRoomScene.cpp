#include "salvageRoomScene.hpp"
#include "globals.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void SalvageRoomScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    auto st = calc_screen_tiles(pfrm);
    StringBuffer<30> text("reall salvage?  +");

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    if (auto room = app.player_island().get_room(cursor_loc)) {
        if (auto mt = room->metaclass()) {
            if (str_cmp((*mt)->name(), "power-core") == 0) {
                // That would be suicide! You can't salvage your island's power
                // core.
                exit_countdown_ = 1;
            }
            text += to_string<10>((*mt)->cost() * 0.75f);
        } else {
            text += "0";
        }
    }

    text += "$";

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

    yes_text_->assign("A: yes ");
    no_text_->assign("B:  no ");

    for (int i = 23; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    persist_ui();
}



void SalvageRoomScene::exit(Platform& pfrm, App& app, Scene& next)
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
SalvageRoomScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return scene_pool::alloc<ReadyScene>();
        }
    } else {
        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        if (pfrm.keyboard().down_transition<Key::action_1>()) {
            if (auto room = app.player_island().get_room(cursor_loc)) {
                if (auto mt = room->metaclass()) {
                    app.coins() += (*mt)->cost() * 0.75f;
                }
                app.player_island().destroy_room(pfrm, cursor_loc);
            }
            exit_countdown_ = milliseconds(500);
        }
    }



    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
