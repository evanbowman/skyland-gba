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


    ScenePtr<Scene> update(Microseconds delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


private:
    StringBuffer<64> path_;
    Vector<char> text_buffer_;

    UserContext user_context_;


    std::optional<TextView> menu_text_;
};



} // namespace skyland
