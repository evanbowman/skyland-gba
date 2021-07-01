#include "recoverCharacterScene.hpp"
#include "globals.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



RecoverCharacterScene::RecoverCharacterScene(const Vec2<u8>& transporter_loc)
    : transporter_loc_(transporter_loc)
{
}



ScenePtr<Scene>
RecoverCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    if (pfrm.keyboard().down_transition<Key::left>()) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::right>()) {
        if (cursor_loc.x < app.player_island().terrain().size()) {
            ++cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::up>()) {
        if (cursor_loc.y > 6) {
            --cursor_loc.y;
        }
    }

    if (pfrm.keyboard().down_transition<Key::down>()) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
        }
    }

    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        if (auto room = app.opponent_island()->get_room(cursor_loc)) {
            info(pfrm, "found other room");
            if (length(room->characters())) {
                info(pfrm, "found chrs");
                if (auto origin =
                        app.player_island().get_room(transporter_loc_)) {
                    info(pfrm, "origin exists");
                    StringBuffer<32> str;
                    str += to_string<10>(transporter_loc_.x);
                    str += ",";
                    str += to_string<10>(transporter_loc_.y);
                    info(pfrm, str.c_str());
                    if (auto transporter = dynamic_cast<Transporter*>(origin)) {
                        info(pfrm, "origin is transporter");
                        transporter->recover_character(pfrm, app, cursor_loc);
                        return scene_pool::alloc<ReadyScene>();
                    }
                }
            }
        }
    }

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }


    return null_scene();
}


void RecoverCharacterScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (not app.opponent_island()) {
        return;
    }

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(17);

    auto origin = app.opponent_island()->origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



void RecoverCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    far_camera();

    auto st = calc_screen_tiles(pfrm);
    text_.emplace(pfrm, "recover character?", OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }
}



void RecoverCharacterScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);


    auto st = calc_screen_tiles(pfrm);
    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 1, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 0);
    }

    text_.reset();
}



} // namespace skyland
