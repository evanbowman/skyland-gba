#pragma once

#include "blind_jump/entity/entityGroup.hpp"
#include "camera.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "scene.hpp"
#include "player.hpp"
#include "opponent/enemyAI.hpp"
#include "worldMap.hpp"




namespace skyland {



class App {
public:
    App(Platform& pfrm);

    void update(Platform& pfrm, Microseconds delta);
    void render(Platform& pfrm);


    Island& player_island()
    {
        return player_island_;
    }


    void updateParallax(Microseconds delta);


    Coins& coins()
    {
        return coins_;
    }


    Coins& terrain_cost()
    {
        return terrain_cost_;
    }


    Camera& camera()
    {
        return camera_;
    }


    bool& paused()
    {
        return paused_;
    }


    std::optional<Island>& opponent_island()
    {
        return opponent_island_;
    }


    EntityList<Entity>& effects()
    {
        return effects_;
    }


    using DeferredCallback = Function<16, void(Platform&, App&)>;


    bool on_timeout(Platform& pfrm,
                    Microseconds expire_time,
                    const DeferredCallback& callback)
    {
        if (not deferred_callbacks_.push_back({callback, expire_time})) {
            warning(pfrm, "failed to enq timeout");
            return false;
        }
        return true;
    }


    Player& player()
    {
        return player_;
    }


    Opponent& opponent()
    {
        return enemy_ai_;
    }


    void init_scripts(Platform& pfrm);


    Coins& victory_coins()
    {
        return victory_coins_;
    }


    EntityList<Entity>& birbs()
    {
        return birbs_;
    }


    Microseconds& birb_counter()
    {
        return birb_counter_;
    }


    WorldMap& world_map()
    {
        return world_map_;
    }


    Vec2<u8>& current_map_location()
    {
        return current_map_location_;
    }


    int& hostile_nodes_visited()
    {
        return hostile_nodes_visited_;
    }


private:
    Island player_island_;
    Float cloud_scroll_1_;
    Float cloud_scroll_2_;
    ScenePtr<Scene> current_scene_;
    ScenePtr<Scene> next_scene_;
    Coins coins_ = 0;
    Coins terrain_cost_ = 0;
    Coins victory_coins_ = 0;
    Camera camera_;
    bool paused_ = false;
    WorldMap world_map_;
    Vec2<u8> current_map_location_ = {0, 1};
    int hostile_nodes_visited_ = 0;

    EntityList<Entity> effects_;
    EntityList<Entity> birbs_;

    Microseconds birb_counter_;

    std::optional<Island> opponent_island_;

    Buffer<std::pair<DeferredCallback, Microseconds>, 10> deferred_callbacks_;

    Player player_; // Just a null sentinel object essentially...

    EnemyAI enemy_ai_;
};



} // namespace skyland
