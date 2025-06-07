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

#include "function.hpp"
#include "memory/uniquePtr.hpp"
#include "number/numeric.hpp"
#include "script/value.hpp"



class Platform;


namespace skyland
{


class App;
class Scene;


template <typename T> using UniqueScenePtr = UniquePtr<T, void (*)(Scene*)>;
using ScenePtr = UniqueScenePtr<Scene>;


ScenePtr null_scene();



class WorldScene;
class ConstructionScene;
class BoxedDialogSceneWS;
class MultiplayerCoOpAwaitLockScene;
class MultiplayerCoOpAwaitChrLockScene;
namespace macro
{
class MacrocosmScene;
}



class Scene
{
public:
    virtual ~Scene(){};


    virtual ScenePtr update(Time delta);


    virtual void display();


    virtual bool displays_minimap();


    virtual void enter(Scene& prev_scene);


    virtual void exit(Scene& next_scene);


    // Yeah, I should be using a visitor.
    virtual WorldScene* cast_world_scene();


    virtual macro::MacrocosmScene* cast_macrocosm_scene();


    virtual ConstructionScene* cast_construction_scene();


    virtual BoxedDialogSceneWS* cast_boxed_dialog_scene_ws();


    virtual MultiplayerCoOpAwaitLockScene* cast_co_op_await_lock_scene();


    virtual MultiplayerCoOpAwaitChrLockScene* cast_co_op_await_chr_lock_scene();


    // NOTE: gui nodes: one scene subclass in particular supports scripting menu
    // logic with an xml-styled DOM.
    virtual void
    gui_add_node(const char* parent_id, const char* id, const char* type);


    virtual void gui_delete_node(const char* id);


    virtual void
    gui_set_attr(const char* id, const char* attr, lisp::Value* val);
};


ScenePtr initial_scene(bool clean_boot);


using DeferredScene = Function<sizeof(void*) * 4, ScenePtr()>;



} // namespace skyland
