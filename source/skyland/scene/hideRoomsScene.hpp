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


    void enter(Scene& prev) override;
    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


private:
    void repaint();


    struct Data
    {
        Buffer<u16, 100> room_classes_;
    };


    Optional<DynamicMemory<Data>> data_;


    Buffer<Text, 3> names_;
    Buffer<Text, 3> hidden_;
    int index_ = 0;


    DeferredScene next_;
    bool changed_ = false;
};



} // namespace skyland
