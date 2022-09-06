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


#pragma once

#include "function.hpp"
#include "memory/uniquePtr.hpp"
#include "number/numeric.hpp"



class Platform;


namespace skyland
{


class App;
class Scene;


template <typename T> using ScenePtr = UniquePtr<T, void (*)(Scene*)>;


ScenePtr<Scene> null_scene();



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


    virtual ScenePtr<Scene> update(Platform&, App&, Microseconds delta)
    {
        return null_scene();
    }


    virtual void display(Platform&, App&)
    {
    }


    virtual void enter(Platform&, App&, Scene& prev_scene){};


    virtual void exit(Platform&, App&, Scene& next_scene){};


    // Yeah, I should be using a visitor.
    virtual WorldScene* cast_world_scene()
    {
        return nullptr;
    }


    virtual macro::MacrocosmScene* cast_macrocosm_scene()
    {
        return nullptr;
    }


    virtual ConstructionScene* cast_construction_scene()
    {
        return nullptr;
    }


    virtual BoxedDialogSceneWS* cast_boxed_dialog_scene_ws()
    {
        return nullptr;
    }


    virtual MultiplayerCoOpAwaitLockScene* cast_co_op_await_lock_scene()
    {
        return nullptr;
    }


    virtual MultiplayerCoOpAwaitChrLockScene* cast_co_op_await_chr_lock_scene()
    {
        return nullptr;
    }
};


ScenePtr<Scene> initial_scene(bool clean_boot);


using DeferredScene = Function<sizeof(void*) * 4, ScenePtr<Scene>()>;



} // namespace skyland
