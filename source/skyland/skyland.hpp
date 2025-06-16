////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "alloc_entity.hpp"
#include "boxed.hpp"
#include "camera.hpp"
#include "coins.hpp"
#include "console.hpp"
#include "dialog.hpp"
#include "entity/birds/bird.hpp"
#include "faction.hpp"
#include "flag.hpp"
#include "gamespeed.hpp"
#include "highscores.hpp"
#include "island.hpp"
#include "keyCallbackProcessor.hpp"
#include "macrocosmEngineOpaque.hpp"
#include "persistentData.hpp"
#include "platform/platform.hpp"
#include "player/opponent/friendlyAI.hpp"
#include "rumble.hpp"
#include "save.hpp"
#include "scene.hpp"
#include "script/protected.hpp"
#include "stateBit.hpp"
#include "timeStream.hpp"
#include "timeTracker.hpp"
#include "weather/environment.hpp"



namespace skyland
{



// I used to pass App around to all of the functions that needed it, avoiding
// global variables. But adding an extra function argument is not
extern App* __app__;
#define APP (*__app__)


class App
{
public:
    App(bool clean_boot);

    void update(Time delta);
    void render();


    void _render_update_scroll();


    Island& player_island()
    {
        return world_state_->player_;
    }


    void update_parallax(Time delta);



    Coins coins() const
    {
        return persistent_data_.coins_;
    }


    void set_coins(Coins coins);


    Coins terrain_cost(Island& island);


    Boxed<Camera, Camera, 9 * sizeof(void*)>& camera()
    {
        return camera_;
    }


    // Cached time delta, converted to a fixnum.
    Fixnum& delta_fp()
    {
        return delta_fp_;
    }


    GameSpeed& game_speed()
    {
        return game_speed_;
    }


    Island* opponent_island()
    {
        if (world_state_->opponent_) {
            return &*world_state_->opponent_;
        }
        return nullptr;
    }


    template <typename F> void with_opponent_island(F&& cb)
    {
        if (opponent_island()) {
            cb(*opponent_island());
        }
    }


    void start_console();


    void create_opponent_island(int terrain_size)
    {
        reset_opponent_island();

        world_state_->opponent_.emplace(
            Layer::map_1_ext, terrain_size, opponent());
    }


    void reset_opponent_island()
    {
        if (world_state_->opponent_) {
            world_state_->opponent_->clear_rooms();
            world_state_->opponent_.reset();
        }
    }


    EntityList<Entity>& effects()
    {
        return effects_;
    }


    enum class GameMode : u8 {
        adventure,
        challenge,
        tutorial,
        sandbox,
        multiplayer,
        co_op,
        skyland_forever,
        macro,
    };


    GameMode& game_mode()
    {
        return game_mode_;
    }


    Faction& faction();


    void clear_effects_lowpriority();


    template <typename T, typename... Args>
    EntityRef<T> alloc_entity(Args&&... args)
    {
        if (auto e = ::skyland::alloc_entity<T>(std::forward<Args>(args)...)) {
            return e;
        }

        // If we fail to allocate an entity, try clearing out an entity from the
        // special effects, and allocating again. This may free up enough
        // space...
        for (int i = 0; i < 10; ++i) {
            auto e = effects().pop_last();
            if (not e) { // No last element, i.e. empty list
                break;
            }
            // The effect is one of the few somewhat important ones, do not
            // deallocate it, add it back to the front of the list.
            if (not(*e)->entity_oom_deletable()) {
                effects().push(std::move(*e));
            } else {
                // drop last element e, frees a slot
                break;
            }
            // try again...
        }

        auto e = ::skyland::alloc_entity<T>(std::forward<Args>(args)...);
        if (not e) {
            error("entity pool exhausted");
        }

        return e;
    }


    using DeferredCallback = Function<4 * sizeof(void*), void()>;


    bool on_timeout(Time expire_time, const DeferredCallback& callback)
    {
        if (not deferred_callbacks_.emplace_back(callback, expire_time)) {
            warning("failed to enq timeout");
            return false;
        }
        return true;
    }


    weather::Environment& environment();


    template <typename T, typename... Args>
    void swap_environment(Args&&... args)
    {
        environment_.emplace<T>(std::forward<Args>(args)...);
    }


    Player& player()
    {
        return *player_;
    }


    Opponent& opponent()
    {
        return *opponent_;
    }


    template <typename T, typename... Args> T& swap_opponent(Args&&... args)
    {
        opponent_.emplace<T>(std::forward<Args>(args)...);
        if (opponent_island()) {
            opponent_island()->set_owner(*opponent_);
        }
        return (T&)*opponent_;
    }


    template <typename T, typename... Args> void swap_player(Args&&... args)
    {
        player_.emplace<T>(std::forward<Args>(args)...);
        player_island().set_owner(*player_); // probably unnecessary
    }


    void init_scripts(Function<4 * sizeof(void*), void(const char*)> msg);


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


    Optional<DialogBuffer>& dialog_buffer()
    {
        return dialog_buffer_;
    }


    enum class ExitCondition : u8 {
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


    u16& pause_count()
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



    FlagPixels custom_flag_image_;
    GlobalPersistentData gp_;


    lisp::Value* invoke_ram_script(const char* ram_fs_path);


    lisp::Value* invoke_script(
        const char* path,
        bool rom_fs_only = false,
        Optional<Function<16, void(lisp::Value& err)>> err_handler = nullopt());


    bool load_file(const char* path, Vector<char>& result);


    bool is_developer_mode();
    void set_developer_mode(bool value);


    struct BackupContext
    {
        s8 next_world_location_ = -1;
    };

    void create_backup(const BackupContext& ctx);
    void store_backup();
    void delete_backup();
    bool has_backup();
    void restore_backup();

    save::EmergencyBackup* get_backup();


    Scene& scene()
    {
        return *current_scene_;
    }


    void setup_input(lisp::Value* msg_callback_pair)
    {
        input_setup_info_ = msg_callback_pair;
    }


    Optional<lisp::Protected>& input_setup_info()
    {
        return input_setup_info_;
    }



    time_stream::TimeStream& time_stream()
    {
        return time_stream_;
    }


    ScenePtr& next_scene()
    {
        return next_scene_;
    }


    KeyCallbackProcessor& key_callback_processor()
    {
        return key_callback_processor_;
    }


    using MacrocosmEngine =
        Boxed<macro::Engine, macro::Engine, 4 * sizeof(void*)>;
    Optional<MacrocosmEngine>& macrocosm()
    {
        return macrocosm_;
    }


private:
    // NOTE: As islands take a lot of memory, and App is created on the stack, I
    // ended up moving them into a scratch buffer.
    struct WorldState
    {
        template <typename... Args>
        WorldState(Args&&... args) : player_(std::forward<Args>(args)...)
        {
        }

        Island player_;
        Optional<Island> opponent_;
    };


    ////////////////////////////////////////////////////////////////////////////
    // Fields with eight-byte (?) alignment:
    TimeTracker level_timer_;
    TimeTracker stat_timer_;
    Fixnum delta_fp_ = 1.0_fixed;

    ////////////////////////////////////////////////////////////////////////////
    // Fields with four-byte alignment:
    PersistentData persistent_data_;
    DynamicMemory<WorldState> world_state_;
    Fixnum cloud_scroll_1fp_;
    Fixnum cloud_scroll_2fp_;
    ScenePtr current_scene_;
    ScenePtr next_scene_;
    Coins victory_coins_ = 0;
    Coins level_coins_spent_ = 0;
    Boxed<Camera, Camera, 9 * sizeof(void*)> camera_;
    Rumble rumble_;

    Optional<lisp::Protected> input_setup_info_;


    KeyCallbackProcessor key_callback_processor_;

    Boxed<weather::Environment, weather::ClearSkies, 8 * sizeof(void*)>
        environment_;

    Optional<DialogBuffer> dialog_buffer_;

    EntityList<Entity> effects_;
    EntityList<Bird> birds_;

    Buffer<std::pair<DeferredCallback, Time>, 20> deferred_callbacks_;


    Boxed<Player, Player, 26 * sizeof(void*)> player_;
    Boxed<Opponent, FriendlyAI, 26 * sizeof(void*)> opponent_;


    // In the unlikely event that the game freezes for some reason, the software
    // stores a backup of the last state before entering the current level.
    DynamicMemory<save::EmergencyBackup> backup_;

    time_stream::TimeStream time_stream_;

    Optional<MacrocosmEngine> macrocosm_;

    s32 level_begin_score_ = 0;


    Optional<DynamicMemory<ConsoleState>> console_state_;


    ////////////////////////////////////////////////////////////////////////////
    // Fields with two-byte alignment
    u16 pause_count_ = 0;

public:
    u16 dropped_frames_ = 0;

private:
    ////////////////////////////////////////////////////////////////////////////
    // unaligned fields
    StateBitvector state_bitvector_;
    ExitCondition exit_condition_ = ExitCondition::none;
    GameSpeed game_speed_ = GameSpeed::normal;
    bool launch_input_ = false;
    GameMode game_mode_ = GameMode::adventure;
    Faction faction_ = Faction::human;
    enum class RemoteConsoleSyntax : u8 {
        none,
        simple_console,
        lisp,
    } remote_console_syntax_ = RemoteConsoleSyntax::none;


    void record_score_diff(int diff);
};



void write_custom_graphics();



ScenePtr reject_if_friendly();



using FileLine = StringBuffer<1980>;
DynamicMemory<FileLine> get_line_from_file(const char* file_name, int line);



void parallax_background_task();



} // namespace skyland
