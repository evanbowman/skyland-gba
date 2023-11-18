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

#include "fadeInScene.hpp"
#include "modules/datetimeModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "version.hpp"



namespace skyland
{



static constexpr const char* lang_file = "/lang.txt";



void init_clouds();



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class LanguageSelectScene : public Scene
{
private:
    using LanguageOptions =
        Buffer<std::pair<StringBuffer<48>, StringBuffer<48>>, 16>;
    DynamicMemory<LanguageOptions> opts_;

    int sel_ = 0;

    Buffer<Text, 8> text_opts_;

    bool clean_boot_;

public:
    LanguageSelectScene(bool clean_boot)
        : opts_(load_language_options()), clean_boot_(clean_boot)
    {
    }


    void enter(App& app, Scene& prev) override
    {
        if (opts_->size() > 1) {
            u8 row = 3;
            for (auto& opt : *opts_) {
                text_opts_.emplace_back(opt.first.c_str(),
                                        OverlayCoord{3, row});
                row += 2;
            }
        }
    }


    void exit(App& app, Scene& prev) override
    {
        text_opts_.clear();
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override
    {
        auto show_cursor = [&] {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 1, y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 3 + sel_ * 2, 396);
        };

        show_cursor();

        if (key_down<Key::up>()) {
            if (sel_ > 0) {
                --sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (key_down<Key::down>()) {
            if (sel_ < (int)opts_->size()) {
                ++sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (opts_->empty() or opts_->size() == 1 or
                   key_down<Key::action_1>()) {
            if (opts_->size() > 1) {
                auto path = (*opts_)[sel_].second.c_str();
                systemstring_bind_file(path);
                flash_filesystem::store_file_data(
                    lang_file, path, str_len(path));
            }
            auto has_clock = PLATFORM.system_clock().initial_time();
            if (clean_boot_ and has_clock) {
                auto next = scene_pool::alloc<DatetimeModule>();
                next->next_scene_ =
                    scene_pool::make_deferred_scene<IntroCreditsScene>();
                return next;
            } else {
                if (PLATFORM.device_name() == "MacroDesktopDemo") {
                    app.gp_.stateflags_.set(
                        GlobalPersistentData::freebuild_unlocked, true);
                    return scene_pool::alloc<MacrocosmFreebuildModule>();
                }

                return scene_pool::alloc<IntroCreditsScene>();
            }
        }

        return null_scene();
    }


private:
    static DynamicMemory<LanguageOptions> load_language_options()
    {
        auto result = allocate_dynamic<LanguageOptions>("lang-table");

        auto cp = PLATFORM.load_file_contents("strings", "lang.txt");

        std::pair<StringBuffer<48>, StringBuffer<48>> current;
        int parse_state = 0;
        utf8::scan(
            [&](utf8::Codepoint cp, const char* raw, int) {
                if (cp == '=') {
                    parse_state = 1;
                } else if (cp == '\n') {
                    parse_state = 0;
                    result->emplace_back(current);
                    current.first.clear();
                    current.second.clear();
                } else {
                    if (parse_state == 0) {
                        current.first += raw;
                    } else {
                        current.second += raw;
                    }
                }
            },
            cp,
            str_len(cp));

        return result;
    }
};



class BootScene : public Scene
{
public:
    bool clean_boot_;


    BootScene(bool clean_boot) : clean_boot_(clean_boot)
    {
    }



    static void init()
    {
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM.system_call("vsync", 0);
        PLATFORM.enable_glyph_mode(true);
        PLATFORM.load_overlay_texture("overlay");
        PLATFORM.load_tile1_texture("boot_img_flattened");


        __draw_image(1, 0, 3, 30, 12, Layer::map_1);

        PLATFORM.screen().schedule_fade(0.f);

        const auto st = calc_screen_tiles();

        for (int x = 0; x < st.x; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 0, 112);
            PLATFORM.set_tile(Layer::overlay, x, 1, 112);
            PLATFORM.set_tile(Layer::overlay, x, 2, 112);

            PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 4, 112);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 5, 112);
        }

        __draw_image(1, 0, 3, 30, 12, Layer::map_1);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM.screen().schedule_fade(0.f);

        Text text({4, 1});
        text.append(
            "_\\\\ SKYLAND engine //_",
            FontColors{ColorConstant::silver_white, ColorConstant::rich_black});
        text.__detach();

        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.speaker().start();
        PLATFORM.speaker().play_sound("click_digital_1", 1);


        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        PLATFORM.sleep(2);
        Text version(vn.c_str(), {1, u8(st.y - 4)});
        version.__detach();

        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.sleep(1);
    }


    static void message(const char* text, bool log = true)
    {
        const auto st = calc_screen_tiles();

        PLATFORM.system_call("vsync", 0);
        Text msg(text, {1, u8(st.y - 2)});
        auto len = msg.len();
        for (int x = 0; x < st.x - len; ++x) {
            msg.append(" ");
        }
        msg.__detach();

        if (log) {
            info(text);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }


    ScenePtr<Scene> update(App& app, Microseconds delta)
    {
        PLATFORM.load_background_texture(
            app.environment().background_texture());

        const auto st = calc_screen_tiles();

        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        Text boot("booting...", {1, u8(st.y - 2)});
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        app.init_scripts([&](const char* text) { message(text); });

        message("reticulating splines...", false);
        skyland::achievements::init(app);

        message("lisp gc sweep...");
        lisp::gc();

        PLATFORM.fill_overlay(0);
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.sleep(10);

        // Special case: if we're connected to a networked multiplayer peer upon
        // boot, then we've started a multiboot game, and should progress
        // immediately to a co op match after starting up.
        if (PLATFORM.network_peer().is_connected()) {
            state_bit_store(app, StateBit::multiboot, true);
            globals().room_pools_.create("room-mem");
            globals().entity_pools_.create("entity-mem");
            app.time_stream().enable_pushes(false);
            app.invoke_script("/scripts/config/rooms.lisp");
            app.invoke_script("/scripts/config/damage.lisp");
            app.invoke_script("/scripts/config/timing.lisp");
            init_clouds();
            PLATFORM.load_tile0_texture(
                app.environment().player_island_texture());
            PLATFORM.load_tile1_texture(
                app.environment().opponent_island_texture());
            PLATFORM.load_sprite_texture(app.environment().sprite_texture());
            PLATFORM.load_background_texture(
                app.environment().background_texture());
            PLATFORM.system_call("v-parallax", (void*)true);
            SkylandForever::init(app, 1, rng::get(rng::critical_state));
            app.persistent_data().score_.set(0);
            app.set_coins(std::max(0, app.coins() - 1000));
            app.swap_player<CoOpTeam>();
            app.game_mode() = App::GameMode::co_op;

            for (auto& room : app.player_island().rooms()) {
                network::packet::RoomConstructed packet;
                packet.metaclass_index_.set(room->metaclass_index());
                packet.x_ = room->position().x;
                packet.y_ = room->position().y;
                network::transmit(packet);
            }

            return scene_pool::alloc<FadeInScene>();
        }

        if (not flash_filesystem::file_exists(lang_file) or clean_boot_) {
            info("lang selection...");
            return scene_pool::alloc<LanguageSelectScene>(clean_boot_);
        } else {
            message("bind strings file...");
            Vector<char> data;
            if (flash_filesystem::read_file_data(lang_file, data)) {
                StringBuffer<48> path;
                for (char c : data) {
                    path.push_back(c);
                }
                systemstring_bind_file(path.c_str());
            }
            return scene_pool::alloc<IntroCreditsScene>();
        }
    }
};



} // namespace skyland
