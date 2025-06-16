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

#include "fadeInScene.hpp"
#include "modules/colorProfileModule.hpp"
#include "modules/datetimeModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/latency.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "skyland/scene/desktopOS.hpp"
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


    void enter(Scene& prev) override
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


    void exit(Scene& prev) override
    {
        text_opts_.clear();
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
    }


    ScenePtr update(Time delta) override
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
            if (sel_ < (int)opts_->size() - 1) {
                ++sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (opts_->empty() or opts_->size() == 1 or
                   key_down<Key::action_1>()) {
            if (opts_->size() > 1) {
                auto path = (*opts_)[sel_].second.c_str();
                systemstring_bind_file(path);
                flash_filesystem::store_file_data(
                    lang_file, path, strlen(path));
            }

            if (PLATFORM.device_name() == "MacroDesktopDemo") {
                APP.gp_.stateflags_.set(
                    GlobalPersistentData::freebuild_unlocked, true);
                return make_scene<MacrocosmFreebuildModule>();
            }

            return make_scene<IntroCreditsScene>();
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
                return true;
            },
            cp,
            strlen(cp));

        return result;
    }
};



class BootScene : public Scene
{
public:
    bool clean_boot_;

    static constexpr const auto amber_color = custom_color(0xfce165);
    static constexpr const auto back_color = custom_color(0x00210f);


    bool diagnostic_ = false;


    BootScene(bool clean_boot) : clean_boot_(clean_boot)
    {
    }



    static void init()
    {
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM_EXTENSION(force_vsync);
        PLATFORM.enable_glyph_mode(true);
        PLATFORM.load_overlay_texture("overlay");
        PLATFORM.load_tile1_texture("boot_img_flattened");

        auto fc = FontColors{amber_color, back_color};

        for (u8 x = 0; x < 30; ++x) {
            for (u8 y = 0; y < 3; ++y) {
                Text::print(" ", {x, y}, fc);
            }
            for (u8 y = 15; y < 20; ++y) {
                Text::print(" ", {x, y}, fc);
            }
        }


        __draw_image(1, 0, 0, 30, 12, Layer::map_1);

        PLATFORM.screen().schedule_fade(0.f);

        const auto st = calc_screen_tiles();

        __draw_image(1, 0, 3, 30, 12, Layer::map_1);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM.screen().schedule_fade(
            1.f, back_color, true, false, true, false);

        Text::print("(R)", {19, 5}, fc);



        const char* lines[] = {
            "     ",
            " .__________________________.",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            " .__________________________.",
            "                             ",
            " Cartridge Operating System ",
        };

        Text::print("(hold select for boot menu)",
                    {1, 8},
                    FontColors{custom_color(0xd9b059), back_color});

        int i = 0;
        for (auto& l : lines) {
            Text::print(l, {0, u8(1 + i)}, fc);
            ++i;
        }

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
        Text version({1, u8(st.y - 4)});
        version.append(vn.c_str(), FontColors{amber_color, back_color});
        version.__detach();

        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.sleep(1);
    }


    static void message(const char* text, bool log = true)
    {
        const auto st = calc_screen_tiles();

        auto fc = FontColors{amber_color, back_color};

        if (state_bit_load(StateBit::verbose_boot)) {

            for (int x = 0; x < 30; ++x) {
                // Clean boot select hint
                PLATFORM.set_tile(Layer::overlay, x, 8, 0);
            }

            Text::print(format("Mem: [%/%]",
                               scratch_buffers_in_use() * 2,
                               scratch_buffer_count * 2)
                            .c_str(),
                        {2, 8},
                        fc);

            auto stat = flash_filesystem::statistics();

            Text::print(
                format("Disk: [%/%]",
                       stat.bytes_used_ / 1024,
                       (stat.bytes_used_ + stat.bytes_available_) / 1024)
                    .c_str(),
                {16, 8},
                fc);

            u32 mstack = 0;
            if (auto s = PLATFORM.get_extensions().get_stack_usage) {
                mstack = s();
            }

            Text::print(format("Stk: [%]", mstack).c_str(), {2, 10}, fc);

            for (int i = 16; i < 30; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, 10, 0);
            }
            Text::print(
                format("Lisp: [%]", lisp::value_pool_info().first).c_str(),
                {16, 10},
                fc);
        }

        PLATFORM.screen().clear();

        Text msg({1, u8(st.y - 2)});
        msg.append(text, fc);
        auto len = msg.len();
        for (int x = 0; x < st.x - len; ++x) {
            msg.append(" ", fc);
        }
        msg.__detach();

        if (log) {
            info(text);
        }

        PLATFORM.screen().display();
    }


    ScenePtr update(Time delta)
    {
        TIMEPOINT(t1);

        PLATFORM.load_background_texture(
            APP.environment().background_texture());

        message("booting...", false);

        APP.init_scripts([&](const char* text) { message(text); });

        message("reticulating splines...", false);
        skyland::achievements::init();

        message("lisp gc sweep...");
        lisp::gc();

        message("ready!", false);

        TIMEPOINT(t2);

        info(format("boot took %", t2 - t1));

        if (auto cm = PLATFORM.get_extensions().apply_color_correction) {
            auto col = ColorProfileModule::load_current_profile();
            if (col.length()) {
                cm(col.c_str());
            }
        }

        PLATFORM.keyboard().poll();

        if (PLATFORM.keyboard().pressed<Key::select>()) {
            auto fc = FontColors{amber_color, back_color};
            auto fc_inv = FontColors{back_color, amber_color};

            int opt = 0;
            for (int x = 0; x < 30; ++x) {
                for (int y = 7; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 0);
                }
            }
            Text::print("Select Boot Mode:", {2, 8}, fc);
            Text::print(" .__________________________.", {0, 17}, fc);

            auto redraw = [&] {
                Text::print(
                    "- Enter Skyland  ", {3, 10}, opt == 0 ? fc_inv : fc);
                Text::print(
                    "- Nimbus GUI     ", {3, 12}, opt == 1 ? fc_inv : fc);
            };

            redraw();

            Time autoboot_timer = seconds(15);
            bool any_button_pressed = false;

            while (true) {
                PLATFORM.screen().clear();
                PLATFORM.keyboard().poll();
                PLATFORM.screen().display();

                if (key_down<Key::up>() or key_down<Key::down>()) {
                    opt = not opt;
                    redraw();
                    PLATFORM.speaker().play_sound("cursor_tick", 0);
                    any_button_pressed = true;
                }

                if (not any_button_pressed) {
                    autoboot_timer -= PLATFORM.delta_clock().reset();
                    Text::print(format("autoboot in % seconds...",
                                       autoboot_timer / seconds(1))
                                    .c_str(),
                                {2, 16},
                                fc);
                } else {
                    for (int x = 0; x < 30; ++x) {
                        PLATFORM.set_tile(Layer::overlay, x, 16, 0);
                    }
                }


                if ((not any_button_pressed and autoboot_timer <= 0) or
                    key_down<Key::action_1>()) {
                    if (opt == 1) {
                        PLATFORM.fill_overlay(0);
                        PLATFORM.screen().schedule_fade(1.f);
                        PLATFORM.screen().clear();
                        PLATFORM.screen().display();
                        PLATFORM.sleep(10);
                        return boot_desktop_os();
                    } else {
                        break;
                    }
                }

                PLATFORM_EXTENSION(feed_watchdog);
            }
        }

        PLATFORM.fill_overlay(0);
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.sleep(10);

        // Special case: if we're connected to a networked multiplayer peer upon
        // boot, then we've started a multiboot game, and should progress
        // immediately to a co op match after starting up.
        if (PLATFORM.network_peer().is_connected()) {
            state_bit_store(StateBit::multiboot, true);
            globals().room_pools_.create("room-mem");
            globals().entity_pools_.create("entity-mem");
            APP.time_stream().enable_pushes(false);
            APP.invoke_script("/scripts/config/rooms.lisp");
            APP.invoke_script("/scripts/config/damage.lisp");
            APP.invoke_script("/scripts/config/timing.lisp");
            init_clouds();
            PLATFORM.load_tile0_texture(
                APP.environment().player_island_texture());
            PLATFORM.load_tile1_texture(
                APP.environment().opponent_island_texture());
            PLATFORM.load_sprite_texture(APP.environment().sprite_texture());
            PLATFORM.load_background_texture(
                APP.environment().background_texture());
            PLATFORM_EXTENSION(vertical_parallax_enable, true);
            SkylandForever::init(1, rng::get(rng::critical_state));
            APP.persistent_data().score_.set(0);
            APP.set_coins(std::max(0, APP.coins() - 1000));
            APP.swap_player<CoOpTeam>();
            APP.game_mode() = App::GameMode::co_op;

            for (auto& room : APP.player_island().rooms()) {
                network::packet::RoomConstructed packet;
                packet.metaclass_index_.set(room->metaclass_index());
                packet.x_ = room->position().x;
                packet.y_ = room->position().y;
                network::transmit(packet);
            }

            return make_scene<FadeInScene>();
        }

        if (not flash_filesystem::file_exists(lang_file) or clean_boot_) {
            info("lang selection...");
            return make_scene<LanguageSelectScene>(clean_boot_);
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

            return make_scene<IntroCreditsScene>();
        }
    }
};



} // namespace skyland
