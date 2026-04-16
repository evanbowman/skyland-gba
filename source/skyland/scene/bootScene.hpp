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
#include "modules/datetimeModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/latency.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "skyland/scene/desktopOS.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "skyland/scene/modules/regressionModule.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/settings.hpp"
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

public:
    LanguageSelectScene() : opts_(load_language_options())
    {
    }


    void enter(Scene& prev) override
    {
        if (opts_->size() > 1) {
            u8 row = 4;
            for (auto& opt : *opts_) {
                text_opts_.emplace_back(opt.first.c_str(),
                                        OverlayCoord{3, row});
                row += 2;
            }
        }
        show_prompt();
    }


    void exit(Scene& prev) override
    {
        text_opts_.clear();
        PLATFORM.fill_overlay(0);
    }


    void show_prompt()
    {
        auto path = (*opts_)[sel_].second.c_str();
        systemstring_bind_file(path);
        for (u8 x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 1, 0);
        }
        Text::print(SYS_CSTR(choose_language), {1, 1});
    }


    ScenePtr update(Time delta) override
    {
        auto show_cursor = [&] {
            for (int y = 3; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 1, y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 4 + sel_ * 2, 396);
        };

        show_cursor();

        if (button_down<Button::up>()) {
            if (sel_ > 0) {
                --sel_;
                show_prompt();
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (button_down<Button::down>()) {
            if (sel_ < (int)opts_->size() - 1) {
                ++sel_;
                show_prompt();
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (opts_->empty() or opts_->size() == 1 or
                   button_down<Button::action_1>()) {
            if (opts_->size() > 1) {
                auto path = (*opts_)[sel_].second.c_str();
                swap_language(path);
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
        auto result = allocate<LanguageOptions>("lang-table");

        auto opts = APP.invoke_script("/strings/lang.lisp");
        lisp::l_foreach(opts, [&](lisp::Value* kvp) {
            if (kvp->type() not_eq lisp::Value::Type::cons) {
                PLATFORM.fatal("lang.lisp: "
                               "invalid language opt format, expected a-list");
            }
            std::pair<StringBuffer<48>, StringBuffer<48>> current;
            auto ui_name = kvp->cons().car();
            auto internal_name = kvp->cons().cdr();
            if (ui_name->type() not_eq lisp::Value::Type::string or
                internal_name->type() not_eq lisp::Value::Type::string) {
                PLATFORM.fatal("lang.lisp: pair should contain strings!");
            }

            result->push_back(
                {ui_name->string().value(), internal_name->string().value()});
        });
        return result;
    }
};



void setup_pools();



lisp::Value* save_obj_file_disk(int argc);



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
        PLATFORM.screen().schedule_fade(1.f,
                                        {.color = ColorConstant::silver_white});
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


        PLATFORM.screen().schedule_fade(1.f,
                                        {.color = back_color,
                                         .include_sprites = true,
                                         .include_overlay = false,
                                         .include_background = true,
                                         .include_tiles = false});

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
        if (PLATFORM.device_name() not_eq "PC") {
            // NOTE: the pc port loads so much faster than the gba edition that
            // the boot screen doesn't really even have time to display.
            PLATFORM.speaker().play_sound("click_digital_1", 1);
        }



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

        message("conversion, software vn. 7.0", false);

        APP.init_scripts([&](const char* text) { message(text); });

        message("reticulating splines...", false);
        skyland::achievements::init();

        message("lisp gc sweep...");
        lisp::gc();

        message("ready!", false);

        TIMEPOINT(t2);

        APP.set_initialized();
        auto dt = PLATFORM.delta_clock().reset();
        info(format("boot took % (~% seconds)",
                    dt,
                    (dt + milliseconds(500)) / seconds(1)));

        settings::apply();

        PLATFORM.input().poll();

        if (PLATFORM.input().pressed<Button::select>()) {
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
                PLATFORM.input().poll();
                PLATFORM.screen().display();

                if (button_down<Button::up>() or button_down<Button::down>()) {
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
                    button_down<Button::action_1>()) {
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

        if (auto match = PLATFORM.get_extensions().has_startup_opt) {
            if (match("--regression") or match("--validate-scripts")) {
                setup_pools();
                TitleScreenScene::run_init_scripts(false);
                return make_scene<RegressionModule>();
            } else if (match("--init-locale")) {
                if (not match("--output")) {
                    error("error: --init-locale is specified, "
                          "but not --output=<dir>");
                    PLATFORM_EXTENSION(quit);
                    return null_scene();
                }
                lisp::Protected callback(L_NIL);
                callback = APP.invoke_script("/strings/extract_strings.lisp");
                lisp::push_op(lisp::make_function([](int argc) {
                    L_EXPECT_ARGC(argc, 3);
                    L_EXPECT_OP(0, function);
                    L_EXPECT_OP(1, cons);
                    L_EXPECT_OP(2, string);
                    lisp::Protected merge_fn(lisp::get_op0());
                    lisp::Protected result_arg(lisp::get_op1());
                    auto match = PLATFORM.get_extensions().has_startup_opt;
                    auto path_prefix = match("--output");
                    auto output_path =
                        format<256>("%/%", path_prefix, L_LOAD_STRING(2));
                    if (auto read =
                            PLATFORM.get_extensions().read_external_file) {
                        Vector<char> existing;
                        read(output_path.c_str(), existing);
                        if (existing.size()) {
                            lisp::VectorCharSequence cs(existing);
                            lisp::read(cs, 0);
                            lisp::Protected result(lisp::get_op0());
                            lisp::pop_op(); // read result
                            lisp::eval(result);
                            result = lisp::get_op0();
                            lisp::pop_op(); // eval result

                            lisp::push_op(result);
                            lisp::push_op(result_arg);
                            lisp::safecall(merge_fn, 2);
                            result_arg = lisp::get_op0();
                            lisp::pop_op();
                        }
                    }
                    Vector<char> result;
                    result.push_back('\'');
                    result.push_back('(');
                    lisp::l_foreach(result_arg, [&](lisp::Value* v) {
                        lisp::_Printer<Vector<char>> p;
                        lisp::format(v, p);
                        auto it = p.data_.begin();
                        if (p.data_.size() and p.data_[0] == '\'') {
                            // Skip over the quote character appended by
                            // lisp::format, as we're inserting into an already
                            // quoted list.
                            //
                            // FIXME: if lisp::format could insert newlines, we
                            // wouldn't need to do any of this...
                            ++it;
                        }
                        for (; it not_eq p.data_.end(); ++it) {
                            result.push_back(*it);
                        }
                        result.push_back('\n');
                    });
                    result.push_back(')');
                    result.push_back('\n');
                    if (auto write =
                            PLATFORM.get_extensions().write_external_file) {
                        write(output_path.c_str(), result);
                    }
                    return L_NIL;
                }));
                lisp::safecall(callback, 1);
                lisp::pop_op(); // result
                PLATFORM_EXTENSION(quit);
            } else if (match("--compile-packages")) {
                if (not match("--output")) {
                    error("error: --compile-packages=<dir> is specified, "
                          "but not --output=<dir>");
                    PLATFORM_EXTENSION(quit);
                }
                auto input_path = match("--compile-packages");
                auto output_path = match("--output");
                lisp::Protected callback(L_NIL);
                callback = APP.invoke_script("/packages/build.lisp");
                lisp::push_op(lisp::make_function(save_obj_file_disk));
                lisp::push_op(lisp::make_function([](int argc) {
                    L_EXPECT_OP(0, string);
                    auto path = L_LOAD_STRING(0);
                    Vector<char> contents;
                    PLATFORM_EXTENSION(read_external_file, path, contents);
                    lisp::VectorCharSequence seq(contents);
                    return lisp::dostring(seq, [](lisp::Value& error) {
                        info(format("error: %", &error));
                    });
                }));
                lisp::ListBuilder input_paths;
                PLATFORM_EXTENSION(walk_external_fs,
                                   input_path,
                                   [&](const char* path, u32 size) {
                                       lisp::Protected str(
                                           lisp::make_string(path));
                                       input_paths.push_back(str);
                                   });
                lisp::push_op(input_paths.result());
                lisp::push_op(lisp::make_string(output_path));
                lisp::safecall(callback, 4);
                lisp::pop_op(); // result
                PLATFORM_EXTENSION(quit);
            }
        }

        if (not flash_filesystem::file_exists(lang_file) or clean_boot_) {
            info("lang selection...");
            return make_scene<LanguageSelectScene>();
        } else {
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
