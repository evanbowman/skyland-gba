#include "constructionScene.hpp"
#include "worldScene.hpp"
#include "skyland/scene_pool.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



ScenePtr<Scene> ConstructionScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    WorldScene::update(pfrm, app, delta);

    if (pfrm.keyboard().down_transition<Key::right>() and selector_ < construction_sites_.size() - 1) {
        ++selector_;
    }

    if (pfrm.keyboard().down_transition<Key::left>() and selector_ > 0) {
        --selector_;
    }

    if (pfrm.keyboard().down_transition<Key::action_2>() and not construction_sites_.empty()) {
        room_metatable().first[0].spawner().create(pfrm,
                                                   &app.player_island(),
                                                   {construction_sites_[selector_].x,
                                                    u8(construction_sites_[selector_].y - 3)});

        find_construction_sites(pfrm, app);
    }

    return null_scene();
}



void ConstructionScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (not construction_sites_.empty()) {
        auto origin = app.player_island().origin();

        origin.x += construction_sites_[selector_].x * 16;
        origin.y += (construction_sites_[selector_].y) * 16;

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_texture_index(12);
        sprite.set_size(Sprite::Size::w16_h32);


        pfrm.screen().draw(sprite);
    }
}


void ConstructionScene::find_construction_sites(Platform& pfrm, App& app)
{
    construction_sites_.clear();

    bool matrix[16][16];

    app.player_island().plot_construction_zones(matrix);

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] and y > 8) {
                construction_sites_.push_back({x, y});
            }
        }
    }

    if (construction_sites_.empty()) {
        selector_ = 0;
    } else if (selector_ >= construction_sites_.size()) {
        selector_--;
    }
}



void ConstructionScene::enter(Platform& pfrm, App& app, Scene&)
{
    find_construction_sites(pfrm, app);
}



void ConstructionScene::exit(Platform& pfrm, App& app, Scene&)
{
    // app.player_island().repaint(pfrm);
}



}
