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



#include "confetti.hpp"
#include "graphics/overlay.hpp"
#include "worldScene.hpp"



namespace skyland
{



class LevelCompleteOptionsScene : public WorldScene
{
public:
    LevelCompleteOptionsScene(
        bool fade_in = false,
        std::optional<DynamicMemory<ConfettiBuffer>> confetti = {})
        : confetti_(std::move(confetti))
    {
        if (fade_in) {
            state_ = State::fade_in;
        }
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override;


    void enter(App& app, Scene& prev) override;
    void exit(App& app, Scene& prev) override;


    void display(App& app) override;


private:
    void show_cursor();


    enum class State {
        select,
        fade_out,
        fade_resume,
        fade_in
    } state_ = State::select;

    int cursor_ = 0;

    std::optional<DynamicMemory<ConfettiBuffer>> confetti_;

    Buffer<Text, 3> options_;
    Microseconds timer_ = 0;
};



} // namespace skyland
