////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


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
    systemstring_bind_file("english.txt");

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
                                     "\n(key-bind \"du\" 'repl)\n"
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



static inline void main_loop()
{
    malloc_compat::Heap heap;

    const bool clean_boot = boot_init();

    BootScene::message("start application...");

    auto app = allocate_dynamic<App>("app-data", clean_boot);

    while (PLATFORM.is_running()) {
        PLATFORM.keyboard().poll();

        PLATFORM.system_call("feed-watchdog", nullptr);

        app->update(PLATFORM.delta_clock().reset());
        PLATFORM.screen().clear();
        app->render();
        PLATFORM.screen().display();
    }
}



} // namespace skyland



void start(Platform& pfrm)
{
    return skyland::main_loop();
}
