////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "skyland/scene.hpp"
#include "platform/platform.hpp"



namespace skyland
{



ScenePtr Scene::update(Time delta)
{
    return null_scene();
}



void Scene::display()
{
}



void Scene::enter(Scene& prev_scene)
{
}



void Scene::exit(Scene& next_scene)
{
}



bool Scene::displays_minimap()
{
    return false;
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
