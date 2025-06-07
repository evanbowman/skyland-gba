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
#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland
{



class CreateFileScene : public Scene
{
public:
    CreateFileScene(const char* ram_file_path);


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    void render_keyboard();


private:
    StringBuffer<64> file_path_;
    Vec2<int> keyboard_cursor_;

    Optional<Text> title_text_;
    Optional<Text> entry_;

    StringBuffer<28> path_;
};



} // namespace skyland
