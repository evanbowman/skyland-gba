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
#include "number/numeric.hpp"
#include <memory>


class Platform;


namespace skyland
{


class App;
class Scene;


template <typename T> using ScenePtr = std::unique_ptr<T, void (*)(Scene*)>;


ScenePtr<Scene> null_scene();


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
};


ScenePtr<Scene> initial_scene();


using DeferredScene = Function<16, ScenePtr<Scene>()>;



} // namespace skyland
