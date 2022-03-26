#pragma once


#include "alloc_entity.hpp"
#include "boxed.hpp"
#include "camera.hpp"
#include "coins.hpp"
#include "customTileMapper.hpp"
#include "dialog.hpp"
#include "entity/birds/bird.hpp"
#include "flag.hpp"
#include "gamespeed.hpp"
#include "highscores.hpp"
#include "island.hpp"
#include "keyCallbackProcessor.hpp"
#include "persistentData.hpp"
#include "platform/platform.hpp"
#include "player/opponent/friendlyAI.hpp"
#include "rumble.hpp"
#include "save.hpp"
#include "scene.hpp"
#include "script/lisp.hpp"
#include "stateBit.hpp"
#include "timeStream.hpp"
#include "timeTracker.hpp"



namespace skyland
{



class App
{
public:
    App(Platform& pfrm);

    void update(Platform& pfrm, Microseconds delta);
    void render(Platform& pfrm);


    Island& player_island()
    {
        return islands_->player_;
    }


    void update_parallax(Microseconds delta);



    Coins coins() const
    {
        return persistent_data_.coins_;
    }


    void set_coins(Platform&, Coins coins);


    Coins terrain_cost(Island& island);


    Boxed<Camera, Camera, 48>& camera()
    {
        return camera_;
    }


    // The time delta represented as a float. Used to improve performance in
    // some cases, sot that we don't need to repeatedly cast the delta time to
    // float in the many areas where the code interpolates position changes
    // based on a time delta.
    Float& float_delta()
    {
        return float_delta_;
    }


    GameSpeed& game_speed()
    {
        return game_speed_;
    }


    Island* opponent_island()
    {
        if (islands_->opponent_) {
            return &*islands_->opponent_;
        }
        return nullptr;
    }


    void create_opponent_island(Platform& pfrm, int terrain_size)
    {
        reset_opponent_island(pfrm);

        islands_->opponent_.emplace(
            pfrm, Layer::map_1_ext, terrain_size, opponent());
    }


    void reset_opponent_island(Platform& pfrm)
    {
        if (islands_->opponent_) {
            islands_->opponent_->clear_rooms(pfrm, *this);
            islands_->opponent_.reset();
        }
    }


    EntityList<Entity>& effects()
    {
        return effects_;
    }


    enum class GameMode {
        adventure,
        challenge,
        tutorial,
        sandbox,
        multiplayer,
        co_op,
        skyland_forever,
    };


    GameMode& game_mode()
    {
        return game_mode_;
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
        if (not deferred_callbacks_.emplace_back(callback, expire_time)) {
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


    EntityList<Bird>& birds()
    {
        return birds_;
    }


    WorldGraph& world_graph()
    {
        return persistent_data_.world_graph_;
    }


    int& current_world_location()
    {
        return persistent_data_.current_world_location_;
    }


    int& zone()
    {
        return persistent_data_.zone_;
    }


    HostInteger<s32>& score()
    {
        return persistent_data_.score_;
    }


    s32& level_begin_score()
    {
        return level_begin_score_;
    }


    std::optional<DialogBuffer>& dialog_buffer()
    {
        return dialog_buffer_;
    }


    enum class ExitCondition {
        none,
        misc,
        victory,
        defeat,
        player_fled,
        opponent_fled,
    };


    ExitCondition& exit_condition()
    {
        return exit_condition_;
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


    // Used as a global clock for time_stream events. May be paused, sped up, or
    // rewound by various scenes.
    TimeTracker& level_timer()
    {
        return level_timer_;
    }


    // Used to keep track of how long that the player spent in the current
    // level. Monotonically incrementing.
    TimeTracker& stat_timer()
    {
        return stat_timer_;
    }


    Coins& level_coins_spent()
    {
        return level_coins_spent_;
    }



    StateBitvector& state_bits()
    {
        return state_bitvector_;
    }



    GlobalPersistentData gp_;


    static const auto pixelate_duration = milliseconds(200);

    void pixelate()
    {
        pixelate_timer_ = pixelate_duration;
    }


    lisp::Value* invoke_ram_script(Platform& pfrm, const char* ram_fs_path);


    lisp::Value*
    invoke_script(Platform& pfrm, const char* path, bool rom_fs_only = false);


    bool is_developer_mode();
    void set_developer_mode(bool value);


    void create_backup(Platform& pfrm);
    void delete_backup();


    Scene& scene()
    {
        return *current_scene_;
    }


    void setup_input(lisp::Value* msg_callback_pair)
    {
        input_setup_info_ = msg_callback_pair;
    }


    std::optional<lisp::Protected>& input_setup_info()
    {
        return input_setup_info_;
    }



    time_stream::TimeStream& time_stream()
    {
        return time_stream_;
    }


    CustomTileMapper& custom_tile_mapper()
    {
        return custom_tile_mapper_;
    }


    CustomTileMapper& custom_sprite_mapper()
    {
        return custom_sprite_mapper_;
    }


    const ScenePtr<Scene>& next_scene() const
    {
        return next_scene_;
    }


private:
    // NOTE: As islands take a lot of memory, and App is created on the stack, I
    // ended up moving them into a scratch buffer.
    struct Islands
    {
        template <typename... Args>
        Islands(Args&&... args) : player_(std::forward<Args>(args)...)
        {
        }

        Island player_;
        std::optional<Island> opponent_;
    };

    void on_remote_console_text(Platform& pfrm,
                                const Platform::RemoteConsole::Line& str);

    PersistentData persistent_data_;
    DynamicMemory<Islands> islands_;
    Float cloud_scroll_1_;
    Float cloud_scroll_2_;
    Float float_delta_ = 1.f;
    ScenePtr<Scene> current_scene_;
    ScenePtr<Scene> next_scene_;
    Coins victory_coins_ = 0;
    Coins level_coins_spent_ = 0;
    Boxed<Camera, Camera, 48> camera_;
    GameSpeed game_speed_ = GameSpeed::normal;
    int pause_count_ = 0;
    Rumble rumble_;

    enum class RemoteConsoleSyntax {
        none,
        simple_console,
        lisp,
    } remote_console_syntax_ = RemoteConsoleSyntax::none;

    s32 level_begin_score_ = 0;

    std::optional<DialogBuffer> dialog_buffer_;
    ExitCondition exit_condition_ = ExitCondition::none;
    bool launch_input_ = false;
    GameMode game_mode_ = GameMode::adventure;

    StateBitvector state_bitvector_;

    std::optional<lisp::Protected> input_setup_info_;

    EntityList<Entity> effects_;
    EntityList<Bird> birds_;

    TimeTracker level_timer_;
    TimeTracker stat_timer_;

    Microseconds pixelate_timer_ = 0;

    Buffer<std::pair<DeferredCallback, Microseconds>, 20> deferred_callbacks_;


    Boxed<Player, Player, 26 * sizeof(void*)> player_;
    Boxed<Opponent, FriendlyAI, 26 * sizeof(void*)> opponent_;


    // In the unlikely event that the game freezes for some reason, the software
    // stores a backup of the last state before entering the current level.
    DynamicMemory<save::EmergencyBackup> backup_;

    time_stream::TimeStream time_stream_;
    CustomTileMapper custom_tile_mapper_;
    CustomTileMapper custom_sprite_mapper_;
};



void write_custom_graphics(Platform& pfrm, App& app);



} // namespace skyland
