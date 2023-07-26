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

#include "graphics/overlay.hpp"
#include "skyland/captain.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class CaptainSelectScene : public Scene
{
public:
    void enter(Platform&, App&, Scene&) override;
    void exit(Platform&, App&, Scene&) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void show_page(Platform&, App&);


    void describe_only(CaptainAbility);
    void describe_all();


    void set_next_scene(DeferredScene next)
    {
        next_ = next;
    }


private:
    Buffer<CaptainAbility, 10> options_;
    std::optional<DeferredScene> next_;
    std::optional<TextView> tv_;
    int index_ = 0;
    bool describe_only_;
};



} // namespace skyland
