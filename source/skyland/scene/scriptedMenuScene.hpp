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
#include "script/lisp.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ScriptedMenuScene : public ActiveWorldScene
{
public:
    ScriptedMenuScene(const char* script_name);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    void gui_add_node(const char* parent_id,
                      const char* id,
                      const char* type) override;


    void gui_delete_node(const char* id) override;


    void
    gui_set_attr(const char* id, const char* attr, lisp::Value* v) override;


private:
    StringBuffer<32> menu_name_;

    Optional<lisp::Protected> model_;

    bool needs_repaint_ = false;

    void repaint_model();
};



} // namespace skyland
