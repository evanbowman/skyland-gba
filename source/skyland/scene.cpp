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


#include "skyland/scene.hpp"
#include "platform/platform.hpp"



namespace skyland
{



ScenePtr<Scene> Scene::update(App&, Microseconds delta)
{
    return null_scene();
}



void Scene::display(App&)
{
}



void Scene::enter(App&, Scene& prev_scene)
{
}



void Scene::exit(App&, Scene& next_scene)
{
}



WorldScene* Scene::cast_world_scene()
{
    return nullptr;
}



macro::MacrocosmScene* Scene::cast_macrocosm_scene()
{
    return nullptr;
}



ConstructionScene* Scene::cast_construction_scene()
{
    return nullptr;
}



BoxedDialogSceneWS* Scene::cast_boxed_dialog_scene_ws()
{
    return nullptr;
}



MultiplayerCoOpAwaitLockScene* Scene::cast_co_op_await_lock_scene()
{
    return nullptr;
}



MultiplayerCoOpAwaitChrLockScene* Scene::cast_co_op_await_chr_lock_scene()
{
    return nullptr;
}



static void script_unsupported_error()
{
    Platform::fatal("current menu does not implement scripting");
}



void Scene::gui_add_node(const char* parent_id,
                         const char* id,
                         const char* type)
{
    script_unsupported_error();
}



void Scene::gui_delete_node(const char* id)
{
    script_unsupported_error();
}



void Scene::gui_set_attr(const char* id, const char* attr, lisp::Value* val)
{
    script_unsupported_error();
}



} // namespace skyland
