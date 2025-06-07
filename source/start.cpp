////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "ext_workram_data.hpp"
#include "globals.hpp"
#include "localization.hpp"
#include "memory/malloc.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "qr.hpp"
#include "rot13.hpp"
#include "skyland/achievement.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/bootScene.hpp"
#include "skyland/skyland.hpp"



void operator delete(void*, unsigned int)
{
    Platform::instance().fatal("delete called");
}



namespace skyland
{



static inline int boot_init()
{
    systemstring_bind_file("english");

    BootScene::init();

    BootScene::message("mount flash filesystem...");

    bool clean_boot = false;

    flash_filesystem::InitConfig c;
    c.offset_ = 8;

    auto stat = flash_filesystem::initialize(c);
    if (stat == flash_filesystem::InitStatus::initialized) {
        const char* user_init_file = ";;;\n"
                                     ";;; init.lisp\n"
                                     ";;;\n"
                                     ";;; The game will run this\n"
                                     ";;; script upon entering\n"
                                     ";;; a game session.\n"
                                     ";;; Create scripts in the\n"
                                     ";;; mods dir, and load them\n"
                                     ";;; here.\n"
                                     ";;;\n"
                                     "(port 1)\n";

        flash_filesystem::store_file_data("/mods/init.lisp",
                                          user_init_file,
                                          strlen(user_init_file),
                                          {.use_compression_ = true});

        clean_boot = true;
    }


    BootScene::message("initializing initializer...");

    scene_pool::pool_ = &globals().scene_pool_;

    return clean_boot;
}



EXT_WORKRAM_DATA u8 frame_count = 0;
EXT_WORKRAM_DATA u8 last_fps1 = 0;
EXT_WORKRAM_DATA u8 last_fps2 = 0;
EXT_WORKRAM_DATA Time frame_total = 0;



void draw_approx_fps(Time delta)
{
    frame_total += delta;

    ++frame_count;

    if (frame_total >= seconds(1)) {
        frame_total -= seconds(1);

        last_fps1 = frame_count / 10;
        last_fps2 = frame_count % 10;

        frame_count = 0;
    }

    auto c = PLATFORM.screen().get_view().int_center();
    auto cx = Fixnum::from_integer(c.x);
    auto cy = Fixnum::from_integer(c.y);

    Sprite spr;
    spr.set_size(Sprite::Size::w8_h8);
    spr.set_priority(0);
    spr.set_tidx_8x8(29, last_fps1);
    spr.set_position({cx + 2.0_fixed, cy + 2.0_fixed});
    PLATFORM.screen().draw(spr);

    spr.set_tidx_8x8(29, last_fps2);
    spr.set_position({cx + 6.0_fixed, cy + 2.0_fixed});
    PLATFORM.screen().draw(spr);
}



} // namespace skyland



void start(Platform& pfrm)
{
    using namespace skyland;


    malloc_compat::Heap heap;

    const bool clean_boot = boot_init();

    BootScene::message("start application...");

    auto app = allocate_dynamic<App>("app-data", clean_boot);

    if (app->is_developer_mode()) {
        state_bit_store(StateBit::verbose_boot, true);
    }

    while (PLATFORM.is_running()) {
        PLATFORM.keyboard().poll();

        PLATFORM_EXTENSION(feed_watchdog);

        auto dt = PLATFORM.delta_clock().reset();
        app->update(dt);
        PLATFORM.screen().clear();

        if (state_bit_load(StateBit::show_fps)) {
            draw_approx_fps(dt);
        }

        app->render();
        PLATFORM.screen().display();
    }
}
