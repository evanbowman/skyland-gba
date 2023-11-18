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


#include "scriptedMenuScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScriptedMenuScene::ScriptedMenuScene(const char* script_name)
    : menu_name_(script_name)
{
}



namespace xml
{
struct Node
{
    Node* next_ = nullptr;
    Node* children_ = nullptr;
};


struct Model
{
    Node* parse(const Vector<char>& file)
    {
        auto iter = file.begin();
        (void)iter;

        return nullptr;
    }
};
} // namespace xml



void ScriptedMenuScene::enter(App& app, Scene& prev)
{
    ActiveWorldScene::enter(app, prev);

    StringBuffer<96> path;
    path = "/scripts/misc/gui/";
    path += menu_name_;
    path += ".xml";

    Vector<char> file;
    if (app.load_file(path.c_str(), file)) {

        xml::Model model;
        auto root = model.parse(file);
        (void)root;
    }
}



void ScriptedMenuScene::exit(App& app, Scene& next)
{
    ActiveWorldScene::exit(app, next);

    invoke_hook(app, "on-menu-exit");
}



ScenePtr<Scene> ScriptedMenuScene::update(App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(app, delta)) {
        return new_scene;
    }

    return null_scene();
}



void ScriptedMenuScene::display(App&)
{
}



} // namespace skyland
