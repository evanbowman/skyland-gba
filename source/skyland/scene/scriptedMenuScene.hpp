////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ScriptedMenuScene : public ActiveWorldScene
{
public:
    ScriptedMenuScene(const char* script_name);


    void enter(App&, Scene& prev) override;
    void exit(App&, Scene& next) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


    void gui_add_node(const char* parent_id,
                      const char* id,
                      const char* type) override;


    void gui_delete_node(const char* id) override;


    void
    gui_set_attr(const char* id, const char* attr, lisp::Value* v) override;


private:
    StringBuffer<32> menu_name_;

    std::optional<lisp::Protected> model_;

    bool needs_repaint_ = false;

    void repaint_model();
};



} // namespace skyland
