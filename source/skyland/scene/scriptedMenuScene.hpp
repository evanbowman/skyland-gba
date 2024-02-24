////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> update(Time delta) override;


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
