////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#include "skyland/scene.hpp"
#include "platform/platform.hpp"



namespace skyland
{



ScenePtr<Scene> Scene::update(Microseconds delta)
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
