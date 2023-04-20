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

#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/dialog.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



// Copy-pasted from the fullscreen dialog scene, but with the base class and a
// couple of other things changed. Argh this is so bad by I'm in a hurry.



class BoxedDialogScene : public Scene
{
public:
    BoxedDialogScene(DialogBuffer buffer, bool expects_answer_y_n)
        : buffer_(std::move(buffer)), expects_answer_y_n_(expects_answer_y_n),
          data_(allocate_dynamic<Data>("dialog-data"))
    {
        goto_tutorial_ = 0;
        allow_fastforward_ = true;
    }


    void disallow_fastforward()
    {
        allow_fastforward_ = false;
    }


    void set_next_scene(DeferredScene scene)
    {
        data_->next_scene_ = scene;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    enum class DisplayMode {
        animate_in,
        busy,
        key_released_check1,
        key_released_check2,
        wait,
        done,
        animate_out,
        boolean_choice,
        choice_select_hold,
        choice_show,
        y_n_wait,
        clear,
    };


    DisplayMode display_mode() const
    {
        return display_mode_;
    }

    const char* ambience_ = nullptr;


    void display(Platform& pfrm, App& app) override;


private:
    void process_command(Platform& pfrm, App& app);

    bool advance_text(Platform& pfrm, App& app, Microseconds delta, bool sfx);

    void clear_textbox(Platform& pfrm);

    struct TextWriterState
    {
        const char* current_word_;
        Microseconds timer_;
        u8 line_;
        u8 pos_;
        u8 current_word_remaining_;
        u8 speed_ = 0;
    };

    DisplayMode display_mode_ = DisplayMode::animate_in;

    TextWriterState text_state_;

    DialogBuffer buffer_;

    u8 expects_answer_y_n_ : 1;
    u8 goto_tutorial_ : 5;
    u8 allow_fastforward_ : 1;
    u8 wait_ = 0;
    u8 choice_tries_ = 0;

    bool choice_sel_ = true;
    bool img_view_ = false;

    struct Data
    {
        struct CharacterDescription
        {
            // Yeah, the screen is only 30 tiles wide, but remember, this buffer
            // holds utf8 text!
            StringBuffer<32> name_;
            u16 image_ = 0;
        } character_;

        std::optional<Text> yes_text_;
        std::optional<Text> no_text_;
        std::optional<UIMetric> coins_;
        std::optional<Text> character_name_text_;

        DeferredScene next_scene_ = []() { return null_scene(); };
    };

    DynamicMemory<Data> data_;
};



ScenePtr<Scene> dialog_prompt(Platform& pfrm,
                              SystemString systr,
                              DeferredScene next,
                              const char* ambience = nullptr);


class BoxedDialogSceneWS : public WorldScene
{
public:
    BoxedDialogSceneWS(DialogBuffer buffer, bool expects_answer_y_n)
        : dialog_scene_(std::move(buffer), expects_answer_y_n)
    {
    }



    BoxedDialogSceneWS* cast_boxed_dialog_scene_ws() override
    {
        return this;
    }


    void display(Platform& pfrm, App& app) override
    {
        dialog_scene_.display(pfrm, app);
        WorldScene::display(pfrm, app);
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override final
    {
        WorldScene::enter(pfrm, app, prev);

        WorldScene::notransitions();

        dialog_scene_.enter(pfrm, app, prev);

        if (app.game_mode() not_eq App::GameMode::tutorial and
            pause_if_hostile_ and app.opponent_island() and
            not app.opponent().is_friendly()) {
            set_gamespeed(pfrm, app, GameSpeed::stopped);
            state_bit_store(app, StateBit::disable_autopause, true);
        }

        if (auto ws = prev.cast_world_scene()) {
            if (ws->is_far_camera()) {
                far_camera();
            }
        }

        if (is_far_camera()) {
            dialog_scene_.set_next_scene(
                scene_pool::make_deferred_scene<InspectP2Scene>());
        } else {
            dialog_scene_.set_next_scene(
                scene_pool::make_deferred_scene<ReadyScene>());
        }
    }


    void exit(Platform& pfrm, App& app, Scene& next) override final
    {
        WorldScene::enter(pfrm, app, next); // FIXME!

        dialog_scene_.exit(pfrm, app, next);

        if (autorestore_music_volume_) {
            pfrm.speaker().set_music_volume(
                Platform::Speaker::music_volume_max);
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override final
    {
        if (app.game_mode() not_eq App::GameMode::tutorial) {
            if (auto scene = WorldScene::update(pfrm, app, delta)) {
                return scene;
            }
        }

        if (dialog_scene_.display_mode() not_eq
            BoxedDialogScene::DisplayMode::boolean_choice) {

            // NOTE: the player class implements all of these key_held,
            // test_key, etc. methods. But we can't use the player
            // implementation here, because we could be in a tutorial, for
            // example, where the game implements playback with keylogging, so
            // we need to ask the platform implementation directly what the
            // keystates are, rather than asking the player class, which may be
            // pulling keystates from a data file.
            auto key_held = [&](Key k, int timer_slot, Microseconds held_time) {
                if (pfrm.keyboard().pressed(k)) {
                    hold_timers_[timer_slot] += delta;
                } else {
                    hold_timers_[timer_slot] = 0;
                }
                return hold_timers_[timer_slot] >= held_time;
            };

            auto key_held_reset = [&](int timer_slot, Microseconds decr) {
                if (hold_timers_[timer_slot] >= decr) {
                    hold_timers_[timer_slot] -= decr;
                }
            };

            int max = 0;
            for (auto v : hold_timers_) {
                if (v > max) {
                    max = v;
                }
            }
            for (auto& v : hold_timers_) {
                v = max;
            }

            auto test_key = [&](Key k, int timer_slot) {
                if (pfrm.keyboard().down_transition(k) or
                    key_held(k, timer_slot, milliseconds(500))) {
                    key_held_reset(timer_slot, milliseconds(100));
                    return true;
                }
                return false;
            };

            if (is_far_camera()) {
                if (not app.opponent_island()) {
                    near_camera();
                } else {
                    auto& cursor_loc = globals().far_cursor_loc_;
                    if (test_key(Key::right, 0)) {
                        if (cursor_loc.x <
                            app.opponent_island()->terrain().size()) {
                            ++cursor_loc.x;
                            camera_update_timer_ = milliseconds(500);
                        }
                    }
                    if (test_key(Key::left, 1)) {
                        if (cursor_loc.x > 0) {
                            --cursor_loc.x;
                            camera_update_timer_ = milliseconds(500);
                        } else {
                            near_camera();
                            globals().near_cursor_loc_.y = cursor_loc.y;
                            camera_update_timer_ = milliseconds(500);
                        }
                    }
                    if (test_key(Key::up, 2) and cursor_loc.y > 5) {
                        --cursor_loc.y;
                        camera_update_timer_ = milliseconds(500);
                    }
                    if (test_key(Key::down, 3) and cursor_loc.y < 14) {
                        ++cursor_loc.y;
                        camera_update_timer_ = milliseconds(500);
                    }
                }
            } else {
                auto& cursor_loc = globals().near_cursor_loc_;
                if (test_key(Key::right, 0)) {
                    if (cursor_loc.x < app.player_island().terrain().size()) {
                        ++cursor_loc.x;
                        camera_update_timer_ = milliseconds(500);
                    } else {
                        far_camera();
                        globals().far_cursor_loc_.y = cursor_loc.y;
                        camera_update_timer_ = milliseconds(500);
                    }
                }
                if (test_key(Key::left, 1)) {
                    if (cursor_loc.x > 0) {
                        --cursor_loc.x;
                        camera_update_timer_ = milliseconds(500);
                    }
                }
                if (test_key(Key::up, 2) and cursor_loc.y > 5) {
                    --cursor_loc.y;
                    camera_update_timer_ = milliseconds(500);
                }
                if (test_key(Key::down, 3) and cursor_loc.y < 14) {
                    ++cursor_loc.y;
                    camera_update_timer_ = milliseconds(500);
                }
            }
        }

        cursor_anim_timer_ += delta;
        if (cursor_anim_timer_ > milliseconds(400)) {
            cursor_anim_timer_ -= milliseconds(400);
            cursor_anim_frame_ = not cursor_anim_frame_;
        }

        app.environment().update(pfrm, app, delta);

        return dialog_scene_.update(pfrm, app, delta);
    }


    Microseconds hold_timers_[4] = {0};
    Microseconds cursor_anim_timer_ = 0;

    bool pause_if_hostile_ = true;
    bool autorestore_music_volume_ = false;

    u8 cursor_anim_frame_ = 0;

private:
    BoxedDialogScene dialog_scene_;
};



} // namespace skyland
