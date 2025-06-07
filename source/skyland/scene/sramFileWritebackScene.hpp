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

#include "containers/vector.hpp"
#include "graphics/overlay.hpp"
#include "modules/userContext.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



// The user edited a ROM file. Obviously, we cannot write the modified file back
// to ROM, but we should notify the user that we are about to save a file to the
// SRAM filesystem instead, and give him/her the option to cancel.



namespace skyland
{



class SramFileWritebackScene : public Scene
{
public:
    SramFileWritebackScene(const char* path,
                           Vector<char>&& text_buffer,
                           UserContext&& user_context);


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


private:
    StringBuffer<64> path_;
    Vector<char> text_buffer_;

    UserContext user_context_;


    Optional<TextView> menu_text_;
};



} // namespace skyland
