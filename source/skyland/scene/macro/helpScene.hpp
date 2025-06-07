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


#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene.hpp"



namespace skyland::macro
{



class HelpScene : public Scene
{
public:
    void enter(Scene&) override;
    void exit(Scene&) override;


    ScenePtr update(Time delta) override;


    void show_page(int pg);


private:
    int page_ = 0;

    static const int page_count = 8;

    Optional<Text> heading_;
    Optional<TextView> tv_;
};



} // namespace skyland::macro
