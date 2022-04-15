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


#include "developerModeModule.hpp"
#include "containers/vector.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/dlc.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{


ScenePtr<Scene>
DeveloperModeModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    // TODO: menu
    app.set_developer_mode(not app.is_developer_mode());

    save::store_global_data(pfrm, app.gp_);

    return scene_pool::alloc<TitleScreenScene>(3);
}



DeveloperModeModule::Factory DeveloperModeModule::factory_;



} // namespace skyland
