////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class HideRoomsScene : public Scene
{
public:
    HideRoomsScene(DeferredScene next) : next_(next)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


private:
    void repaint(Platform& pfrm, App& app);


    struct Data
    {
        Buffer<u16, 100> room_classes_;
    };


    std::optional<DynamicMemory<Data>> data_;


    Buffer<Text, 3> names_;
    Buffer<Text, 3> hidden_;
    int index_ = 0;


    DeferredScene next_;
    bool changed_ = false;
};



} // namespace skyland
