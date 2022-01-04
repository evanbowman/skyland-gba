#pragma once


#include "alloc_entity.hpp"
#include "boxed.hpp"
#include "camera.hpp"
#include "coins.hpp"
#include "dialog.hpp"
#include "entity/birbs/smolBirb.hpp"
#include "flag.hpp"
#include "gamespeed.hpp"
#include "highscores.hpp"
#include "island.hpp"
#include "opponent/enemyAI.hpp"
#include "persistentData.hpp"
#include "platform/platform.hpp"
#include "playerP1.hpp"
#include "rumble.hpp"
#include "save.hpp"
#include "scene.hpp"
#include "script/lisp.hpp"
#include "timeTracker.hpp"
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


    void update_parallax(Microseconds delta);



    Coins& coins()
    {
        return persistent_data_.coins_;
    }


    Coins& terrain_cost()
    {
        return terrain_cost_;
    }


    Camera& camera()
    {
        return camera_;
    }


    GameSpeed& game_speed()
    {
        return game_speed_;
    }


    std::optional<Island>& opponent_island()
    {
        return opponent_island_;
    }


    EntityList<Entity>& effects()
    {
        return effects_;
    }


    bool& challenge_mode()
    {
        return challenge_mode_;
    }


    bool& tutorial_mode()
    {
        return tutorial_mode_;
    }


    template <typename T, typename... Args>
    EntityRef<T> alloc_entity(Platform& pfrm, Args&&... args)
    {
        if (auto e = ::skyland::alloc_entity<T>(std::forward<Args>(args)...)) {
            return e;
        }

        // If we fail to allocate an entity, try clearing out all of the special
        // effects, and allocating again. This should free up enough space...
        effects().clear();

        auto e = ::skyland::alloc_entity<T>(std::forward<Args>(args)...);
        if (not e) {
            error(pfrm, "entity pool exhausted");
        }

        return e;
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
        return *player_;
    }


    Opponent& opponent()
    {
        return *opponent_;
    }


    template <typename T, typename... Args> void swap_opponent(Args&&... args)
    {
        opponent_.emplace<T>(std::forward<Args>(args)...);
        if (opponent_island()) {
            opponent_island()->set_owner(*opponent_);
        }
    }


    template <typename T, typename... Args> void swap_player(Args&&... args)
    {
        player_.emplace<T>(std::forward<Args>(args)...);
        player_island().set_owner(*player_); // probably unnecessary
    }


    void init_scripts(Platform& pfrm);


    Coins& victory_coins()
    {
        return victory_coins_;
    }


    EntityList<SmolBirb>& birbs()
    {
        return birbs_;
    }


    Microseconds& birb_counter()
    {
        return birb_counter_;
    }


    WorldMap& world_map()
    {
        return persistent_data_.world_map_;
    }


    Vec2<u8>& current_map_location()
    {
        return persistent_data_.current_map_location_;
    }


    int& zone()
    {
        return persistent_data_.zone_;
    }


    HostInteger<s32>& score()
    {
        return persistent_data_.score_;
    }


    std::optional<DialogBuffer>& dialog_buffer()
    {
        return dialog_buffer_;
    }


    bool& dialog_expects_answer()
    {
        return dialog_expects_answer_;
    }


    bool& exit_level()
    {
        return exit_level_;
    }


    PersistentData& persistent_data()
    {
        return persistent_data_;
    }


    Rumble& rumble()
    {
        return rumble_;
    }


    int& pause_count()
    {
        return pause_count_;
    }


    TimeTracker& level_timer()
    {
        return level_timer_;
    }


    Coins& level_coins_spent()
    {
        return level_coins_spent_;
    }


    GlobalPersistentData gp_;


    static const auto pixelate_duration = milliseconds(200);

    void pixelate()
    {
        pixelate_timer_ = pixelate_duration;
    }


    void invoke_ram_script(Platform& pfrm, const char* ram_fs_path);


    void safe_invoke_ram_script(Platform& pfrm,
                                const char* ram_fs_path,
                                const char* rom_fs_fallback_path);


    lisp::Value*
    invoke_script(Platform& pfrm, const char* path, bool rom_fs_only = false);


    bool is_developer_mode();
    void set_developer_mode(bool value);


    void create_backup(Platform& pfrm);
    void delete_backup();


private:
    PersistentData persistent_data_;
    Island player_island_;
    Float cloud_scroll_1_;
    Float cloud_scroll_2_;
    ScenePtr<Scene> current_scene_;
    ScenePtr<Scene> next_scene_;
    Coins terrain_cost_ = 0;
    Coins victory_coins_ = 0;
    Coins level_coins_spent_ = 0;
    Camera camera_;
    GameSpeed game_speed_ = GameSpeed::normal;
    int pause_count_ = 0;
    Rumble rumble_;

    std::optional<DialogBuffer> dialog_buffer_;
    bool dialog_expects_answer_ = false;
    bool exit_level_ = false;
    bool challenge_mode_ = false;
    bool tutorial_mode_ = false;

    EntityList<Entity> effects_;
    EntityList<SmolBirb> birbs_;

    Microseconds birb_counter_;
    TimeTracker level_timer_;

    Microseconds pixelate_timer_ = 0;

    std::optional<Island> opponent_island_;

    Buffer<std::pair<DeferredCallback, Microseconds>, 10> deferred_callbacks_;


    Boxed<Player, PlayerP1, 100> player_;
    Boxed<Opponent, EnemyAI, 64> opponent_;


    // In the unlikely event that the game freezes for some reason, the software
    // stores a backup of the last state before entering the current level.
    DynamicMemory<save::EmergencyBackup> backup_;
};



} // namespace skyland
