#include "skyland/scene_pool.hpp"
#include "newgameScene.hpp"
#include "worldMapScene.hpp"
#include "platform/platform.hpp"
#include "graphics/overlay.hpp"
#include "skyland/worldMap.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> WorldMapScene::update(Platform& pfrm,
                                      App& app,
                                      Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        return scene_pool::alloc<NewgameScene>();
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_keyframe_ = not cursor_keyframe_;
    }


    auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];

    if (pfrm.keyboard().down_transition<Key::right>() and
        node.connections_.mask_ & WorldMap::Node::Connections::r) {
        cursor_.x += 1;
        show_map(pfrm, app.world_map());
    } else if (pfrm.keyboard().down_transition<Key::left>() and
        node.connections_.mask_ & WorldMap::Node::Connections::l) {
        cursor_.x -= 1;
        show_map(pfrm, app.world_map());
    } else if (pfrm.keyboard().down_transition<Key::up>()) {
        if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
            cursor_.x += 1;
            cursor_.y -= 1;
            show_map(pfrm, app.world_map());
        } else if (node.connections_.mask_ & WorldMap::Node::Connections::lu) {
            cursor_.x -= 1;
            cursor_.y -= 1;
            show_map(pfrm, app.world_map());
        }
    } else if (pfrm.keyboard().down_transition<Key::down>()) {
        if (node.connections_.mask_ & WorldMap::Node::Connections::ld) {
            cursor_.x -= 1;
            cursor_.y += 1;
            show_map(pfrm, app.world_map());
        } else if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
            cursor_.x += 1;
            cursor_.y += 1;
            show_map(pfrm, app.world_map());
        }
    }



    return null_scene();
}



void WorldMapScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_keyframe_);

    cursor.set_position({
            Float(28 + cursor_.x * 24),
            Float(36 + cursor_.y * 24)
        });

    pfrm.screen().draw(cursor);
}



void WorldMapScene::enter(Platform& pfrm, App& app, Scene& prev_scene)
{
    pfrm.screen().fade(0.f);

    auto view = pfrm.screen().get_view();
    view.set_center({});
    pfrm.screen().set_view(view);

    pfrm.sprite_priority(0);

    pfrm.load_overlay_texture("overlay_world_map");

    pfrm.enable_glyph_mode(true);

    auto st = calc_screen_tiles(pfrm);

    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < st.y; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 88);
        }
    }

    for (int x = 1; x < st.x - 1; ++x) {
        pfrm.set_tile(Layer::overlay, x, 0, 89);
        pfrm.set_tile(Layer::overlay, x, st.y - 1, 90);
    }

    for (int y = 1; y < st.y - 1; ++y) {
        pfrm.set_tile(Layer::overlay, 0, y, 91);
        pfrm.set_tile(Layer::overlay, st.x - 1, y, 92);
    }

    pfrm.set_tile(Layer::overlay, 0, 0, 93);
    pfrm.set_tile(Layer::overlay, st.x - 1, 0, 95);
    pfrm.set_tile(Layer::overlay, 0, st.y - 1, 96);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - 1, 94);

    heading_.emplace(pfrm, OverlayCoord{1, 1});

    heading_->assign("sky map - zone 1");

    show_map(pfrm, app.world_map());

    key_[0].emplace(pfrm, OverlayCoord{3, 13});
    key_[0]->assign("neutral");
    key_[1].emplace(pfrm, OverlayCoord{3, 15});
    key_[1]->assign("hostile");
    key_[2].emplace(pfrm, OverlayCoord{3, 17});
    key_[2]->assign("uncharted");

    pfrm.set_tile(Layer::overlay, 1, 13, 115);
    pfrm.set_tile(Layer::overlay, 1, 15, 118);
    pfrm.set_tile(Layer::overlay, 1, 17, 116);
}



static void highlight_subtree(Platform& pfrm,
                              WorldMap& map,
                              const Vec2<u8>& coord)
{
    auto& node = map.matrix_[coord.x][coord.y];
    pfrm.set_tile(Layer::overlay,
                  4 + coord.x * 3,
                  5 + coord.y * 3,
                  114 + node.type_);


    if (node.connections_.mask_ & WorldMap::Node::Connections::r) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), coord.y});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 5 + coord.y * 3, 102);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 5 + coord.y * 3, 102);
    }

    if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), u8(coord.y - 1)});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 4 + coord.y * 3, 103);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 3 + coord.y * 3, 103);
    }

    if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), u8(coord.y + 1)});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 6 + coord.y * 3, 104);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 7 + coord.y * 3, 104);
    }
}



void WorldMapScene::update_tree(Platform& pfrm, WorldMap& map)
{
    highlight_subtree(pfrm, map, cursor_);
}



void WorldMapScene::show_map(Platform& pfrm, WorldMap& map)
{
    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 3; ++y) {
            auto& node = map.matrix_[x][y];

            pfrm.set_tile(Layer::overlay,
                          4 + x * 3,
                          5 + y * 3,
                          97 + node.type_);

            if (node.connections_.mask_ & WorldMap::Node::Connections::l) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 5 + y * 3, 85);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::r) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 5 + y * 3, 85);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 4 + y * 3, 86);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 6 + y * 3, 87);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::lu) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 4 + y * 3, 87);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::ld) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 6 + y * 3, 86);
            }
        }
    }
    update_tree(pfrm, map);
}



void WorldMapScene::exit(Platform& pfrm, App&, Scene& next_scene)
{
    pfrm.sprite_priority(1);
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    pfrm.load_overlay_texture("overlay");

    pfrm.fill_overlay(0);
}



}
