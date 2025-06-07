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
        Optional<DynamicMemory<ConfettiBuffer>> confetti = {})
        : confetti_(std::move(confetti))
    {
        if (fade_in) {
            state_ = State::fade_in;
        }
    }


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& prev) override;


    void display() override;


private:
    void show_cursor();


    enum class State {
        select,
        fade_out,
        fade_resume,
        fade_in
    } state_ = State::select;

    int cursor_ = 0;

    Optional<DynamicMemory<ConfettiBuffer>> confetti_;

    Buffer<Text, 3> options_;
    Time timer_ = 0;
};



} // namespace skyland
