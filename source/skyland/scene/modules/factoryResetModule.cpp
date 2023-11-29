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


#include "factoryResetModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene> FactoryResetModule::update(Microseconds delta)
{
    if (not text_) {
        PLATFORM.screen().fade(0.9f);
        PLATFORM.screen().fade(1.f);
        text_.emplace();
        text_->assign(SYSTR(factory_reset)->c_str(), {1, 1}, {28, 8});
    }

    if (APP.player().key_down(Key::action_2)) {
        text_.reset();
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    if (APP.player().key_pressed(Key::select) and
        APP.player().key_down(Key::action_1)) {
        ++key_count_;
        if (key_count_ == 5) {
            flash_filesystem::destroy();
            PLATFORM.restart();
        }
    }

    return null_scene();
}



FactoryResetModule::Factory FactoryResetModule::factory_;



} // namespace skyland
