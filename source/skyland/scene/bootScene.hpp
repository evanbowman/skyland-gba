#pragma once

#include "modules/datetimeModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "version.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "fadeInScene.hpp"



namespace skyland
{



static constexpr const char* lang_file = "/lang.txt";



void init_clouds(Platform& pfrm);



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
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
    LanguageSelectScene(Platform& pfrm, bool clean_boot)
        : opts_(load_language_options(pfrm)), clean_boot_(clean_boot)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        if (opts_->size() > 1) {
            u8 row = 3;
            for (auto& opt : *opts_) {
                text_opts_.emplace_back(
                    pfrm, opt.first.c_str(), OverlayCoord{3, row});
                row += 2;
            }
        }
    }


    void exit(Platform& pfrm, App& app, Scene& prev) override
    {
        text_opts_.clear();
        for (int y = 0; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, 1, y, 0);
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        auto show_cursor = [&] {
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 3 + sel_ * 2, 396);
        };

        show_cursor();

        if (key_down<Key::up>(pfrm)) {
            if (sel_ > 0) {
                --sel_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        } else if (key_down<Key::down>(pfrm)) {
            if (sel_ < (int)opts_->size()) {
                ++sel_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        } else if (opts_->empty() or opts_->size() == 1 or
                   key_down<Key::action_1>(pfrm)) {
            if (opts_->size() > 1) {
                auto path = (*opts_)[sel_].second.c_str();
                systemstring_bind_file(path);
                flash_filesystem::store_file_data(
                    pfrm, lang_file, path, str_len(path));
            }
            auto has_clock = pfrm.system_clock().initial_time();
            if (clean_boot_ and has_clock) {
                auto next = scene_pool::alloc<DatetimeModule>();
                next->next_scene_ =
                    scene_pool::make_deferred_scene<IntroCreditsScene>();
                return next;
            } else {
                if (pfrm.device_name() == "MacroDesktopDemo") {
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
    static DynamicMemory<LanguageOptions> load_language_options(Platform& pfrm)
    {
        auto result = allocate_dynamic<LanguageOptions>("lang-table");

        auto cp = pfrm.load_file_contents("strings", "lang.txt");

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



    static void init(Platform& pfrm)
    {
        pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);
        pfrm.screen().clear();
        pfrm.screen().display();


        pfrm.system_call("vsync", 0);
        pfrm.enable_glyph_mode(true);
        pfrm.load_overlay_texture("overlay");
        pfrm.load_tile1_texture("boot_img_flattened");


        __draw_image(pfrm, 1, 0, 3, 30, 12, Layer::map_1);

        pfrm.screen().schedule_fade(0.f);

        const auto st = calc_screen_tiles(pfrm);

        for (int x = 0; x < st.x; ++x) {
            pfrm.set_tile(Layer::overlay, x, 0, 112);
            pfrm.set_tile(Layer::overlay, x, 1, 112);
            pfrm.set_tile(Layer::overlay, x, 2, 112);

            pfrm.set_tile(Layer::overlay, x, st.y - 1, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 2, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 3, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 4, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 5, 112);
        }

        __draw_image(pfrm, 1, 0, 3, 30, 12, Layer::map_1);
        pfrm.screen().clear();
        pfrm.screen().display();


        pfrm.screen().schedule_fade(0.f);

        Text text(pfrm, {4, 1});
        text.append(
            "_\\\\ SKYLAND engine //_",
            FontColors{ColorConstant::silver_white, ColorConstant::rich_black});
        text.__detach();

        pfrm.screen().clear();
        pfrm.screen().display();

        pfrm.speaker().start();
        pfrm.speaker().play_sound("click_digital_1", 1);


        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        pfrm.sleep(2);
        Text version(pfrm, vn.c_str(), {1, u8(st.y - 4)});
        version.__detach();

        pfrm.screen().clear();
        pfrm.screen().display();

        pfrm.sleep(1);
    }


    static void message(Platform& pfrm, const char* text, bool log = true)
    {
        const auto st = calc_screen_tiles(pfrm);

        pfrm.system_call("vsync", 0);
        Text msg(pfrm, text, {1, u8(st.y - 2)});
        auto len = msg.len();
        for (int x = 0; x < st.x - len; ++x) {
            msg.append(" ");
        }
        msg.__detach();

        if (log) {
            info(pfrm, text);
        }

        pfrm.screen().clear();
        pfrm.screen().display();
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        pfrm.load_background_texture(app.environment().background_texture());

        const auto st = calc_screen_tiles(pfrm);

        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        Text boot(pfrm, "booting...", {1, u8(st.y - 2)});
        pfrm.screen().clear();
        pfrm.screen().display();


        app.init_scripts(pfrm, [&](const char* text) { message(pfrm, text); });

        message(pfrm, "reticulating splines...", false);
        skyland::achievements::init(pfrm, app);

        message(pfrm, "lisp gc sweep...");
        lisp::gc();

        pfrm.fill_overlay(0);
        pfrm.screen().schedule_fade(1.f);
        pfrm.screen().clear();
        pfrm.screen().display();
        pfrm.sleep(10);

        // Special case: if we're connected to a networked multiplayer peer upon
        // boot, then we've started a multiboot game, and should progress
        // immediately to a co op match after starting up.
        if (pfrm.network_peer().is_connected()) {
            state_bit_store(app, StateBit::multiboot, true);
            globals().room_pools_.create("room-mem");
            globals().entity_pools_.create("entity-mem");
            app.time_stream().enable_pushes(false);
            app.invoke_script(pfrm, "/scripts/config/rooms.lisp");
            app.invoke_script(pfrm, "/scripts/config/damage.lisp");
            app.invoke_script(pfrm, "/scripts/config/timing.lisp");
            init_clouds(pfrm);
            pfrm.load_tile0_texture(app.environment().player_island_texture());
            pfrm.load_tile1_texture(app.environment().opponent_island_texture());
            pfrm.load_sprite_texture(app.environment().sprite_texture());
            pfrm.load_background_texture(app.environment().background_texture());
            pfrm.system_call("v-parallax", (void*)true);
            SkylandForever::init(pfrm, app, 1, rng::get(rng::critical_state));
            app.persistent_data().score_.set(0);
            app.set_coins(pfrm, std::max(0, app.coins() - 1000));
            app.swap_player<CoOpTeam>();
            app.game_mode() = App::GameMode::co_op;

            for (auto& room : app.player_island().rooms()) {
                network::packet::RoomConstructed packet;
                packet.metaclass_index_.set(room->metaclass_index());
                packet.x_ = room->position().x;
                packet.y_ = room->position().y;
                network::transmit(pfrm, packet);
            }

            return scene_pool::alloc<FadeInScene>();
        }

        if (not flash_filesystem::file_exists(pfrm, lang_file) or clean_boot_) {
            return scene_pool::alloc<LanguageSelectScene>(pfrm, clean_boot_);
        } else {
            Vector<char> data;
            if (flash_filesystem::read_file_data(pfrm, lang_file, data)) {
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
