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

#include "number/numeric.hpp"



class Platform;



namespace skyland
{



// This sound class keeps track of the last time you played the sound, to
// mitigate clipping from overlapping sound effects.



class Sound
{
public:
    Sound(const char* name);
    Sound(const Sound& other) = delete;
    ~Sound();


    void play(int priority, Time max_overlap = milliseconds(200));


    static void update_all(Time delta);


    Time last_played() const
    {
        return last_played_;
    }


    void reset()
    {
        last_played_ = 0;
    }


private:
    void update(Time delta)
    {
        last_played_ += delta;
    }


    Sound* next_;
    const char* name_;
    u32 last_played_;
};



} // namespace skyland
