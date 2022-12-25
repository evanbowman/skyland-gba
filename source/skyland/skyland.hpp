////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
#include "script/lisp.hpp"
#include "stateBit.hpp"
#include "timeStream.hpp"
#include "timeTracker.hpp"
#include "weather/environment.hpp"



namespace skyland
{



class App
{
public:
    App(Platform& pfrm, bool clean_boot);

    void update(Platform& pfrm, Microseconds delta);
    void render(Platform& pfrm);


    Island& player_island()
    {
        return world_state_->player_;
    }


    void update_parallax(Microseconds delta);



    Coins coins() const
    {
        return persistent_data_.coins_;
    }


    void set_coins(Platform&, Coins coins);


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


    void start_console(Platform& pfrm);


    void create_opponent_island(Platform& pfrm, int terrain_size)
    {
        reset_opponent_island(pfrm);

        world_state_->opponent_.emplace(
            pfrm, Layer::map_1_ext, terrain_size, opponent());
    }


    void reset_opponent_island(Platform& pfrm)
    {
        if (world_state_->opponent_) {
            world_state_->opponent_->clear_rooms(pfrm, *this);
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


    template <typename T, typename... Args>
    EntityRef<T> alloc_entity(Platform& pfrm, Args&&... args)
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
            error(pfrm, "entity pool exhausted");
        }

        return e;
    }


    using DeferredCallback = Function<4 * sizeof(void*), void(Platform&, App&)>;


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


    void init_scripts(Platform& pfrm,
                      Function<4 * sizeof(void*), void(const char*)> msg);


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


    lisp::Value* invoke_ram_script(Platform& pfrm, const char* ram_fs_path);


    lisp::Value*
    invoke_script(Platform& pfrm, const char* path, bool rom_fs_only = false);


    bool is_developer_mode();
    void set_developer_mode(bool value);


    struct BackupContext
    {
        s8 next_world_location_ = -1;
    };

    void create_backup(Platform& pfrm, const BackupContext& ctx);
    void delete_backup();
    bool has_backup();
    void restore_backup(Platform& pfrm);


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


    ScenePtr<Scene>& next_scene()
    {
        return next_scene_;
    }


    KeyCallbackProcessor& key_callback_processor()
    {
        return key_callback_processor_;
    }


    using MacrocosmEngine =
        Boxed<macro::Engine, macro::Engine, 4 * sizeof(void*)>;
    std::optional<MacrocosmEngine>& macrocosm()
    {
        return macrocosm_;
    }


    rng::LinearGenerator& crane_game_rng()
    {
        return world_state_->crane_game_rng_;
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
        std::optional<Island> opponent_;
        rng::LinearGenerator crane_game_rng_;
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
    ScenePtr<Scene> current_scene_;
    ScenePtr<Scene> next_scene_;
    Coins victory_coins_ = 0;
    Coins level_coins_spent_ = 0;
    Boxed<Camera, Camera, 9 * sizeof(void*)> camera_;
    Rumble rumble_;

    std::optional<lisp::Protected> input_setup_info_;


    KeyCallbackProcessor key_callback_processor_;

    Boxed<weather::Environment, weather::ClearSkies, 8 * sizeof(void*)>
        environment_;

    std::optional<DialogBuffer> dialog_buffer_;

    EntityList<Entity> effects_;
    EntityList<Bird> birds_;

    Buffer<std::pair<DeferredCallback, Microseconds>, 20> deferred_callbacks_;


    Boxed<Player, Player, 26 * sizeof(void*)> player_;
    Boxed<Opponent, FriendlyAI, 26 * sizeof(void*)> opponent_;


    // In the unlikely event that the game freezes for some reason, the software
    // stores a backup of the last state before entering the current level.
    DynamicMemory<save::EmergencyBackup> backup_;

    time_stream::TimeStream time_stream_;

    std::optional<MacrocosmEngine> macrocosm_;

    s32 level_begin_score_ = 0;


    std::optional<DynamicMemory<ConsoleState>> console_state_;


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
    enum class RemoteConsoleSyntax : u8 {
        none,
        simple_console,
        lisp,
    } remote_console_syntax_ = RemoteConsoleSyntax::none;


    void record_score_diff(Platform& pfrm, int diff);
};



void write_custom_graphics(Platform& pfrm, App& app);



using FileLine = StringBuffer<1980>;
DynamicMemory<FileLine>
get_line_from_file(Platform& pfrm, const char* file_name, int line);



} // namespace skyland
