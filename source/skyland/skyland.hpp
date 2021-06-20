#pragma once

#include "blind_jump/entity/entityGroup.hpp"
#include "camera.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "opponent/enemyAI.hpp"
#include "platform/platform.hpp"
#include "player.hpp"
#include "scene.hpp"
#include "worldMap.hpp"
#include "boxed.hpp"
#include "dialog.hpp"



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


    Opponent& ai()
    {
        return *ai_;
    }


    template <typename T, typename ...Args>
    void swap_ai(Args&& ...args)
    {
        ai_.emplace<T>(std::forward<Args>(args)...);
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


    Float& difficulty_accumulator()
    {
        return difficulty_accumulator_;
    }


    int& zone()
    {
        return zone_;
    }


    std::optional<DialogBuffer>& dialog_buffer()
    {
        return dialog_buffer_;
    }


    bool& dialog_expects_answer()
    {
        return dialog_expects_answer_;
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

    std::optional<DialogBuffer> dialog_buffer_;
    bool dialog_expects_answer_ = false;

    Float difficulty_accumulator_ = 0.f;

    int zone_ = 0;

    EntityList<Entity> effects_;
    EntityList<Entity> birbs_;

    Microseconds birb_counter_;

    std::optional<Island> opponent_island_;

    Buffer<std::pair<DeferredCallback, Microseconds>, 10> deferred_callbacks_;

    Player player_; // Just a null sentinel object essentially...


    Boxed<Opponent, EnemyAI, 64> ai_;

};



} // namespace skyland
