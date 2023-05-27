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


#include "adventureLogScene.hpp"



namespace skyland
{



void AdventureLogScene::enter(Platform&, App&, Scene& prev)
{
}



void AdventureLogScene::exit(Platform&, App&, Scene& next)
{
}



StringBuffer<128> AdventureLogScene::format_logentry(int entry)
{
    StringBuffer<128> result;

    if (auto v = load_logentry(entry)) {
        // TODO: render the logentry as text...
    }

    return result;
}



lisp::Value* AdventureLogScene::load_logentry(int entry)
{
    const auto num_entries = logentry_count();
    return lisp::get_list(lisp::get_var("adventure-log"),
                          (num_entries - 1) - entry);
}



int AdventureLogScene::logentry_count()
{
    return lisp::length(lisp::get_var("adventure-log"));
}



ScenePtr<Scene> AdventureLogScene::update(Platform&, App&, Microseconds delta)
{
    return null_scene();
}



} // namespace skyland
