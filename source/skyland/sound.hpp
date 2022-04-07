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


    void play(Platform& pfrm,
              int priority,
              Microseconds max_overlap = milliseconds(200));


    static void update_all(Microseconds delta);


    Microseconds last_played() const
    {
        return last_played_;
    }


    void reset()
    {
        last_played_ = 0;
    }


private:
    void update(Microseconds delta)
    {
        last_played_ += delta;
    }


    Sound* next_;
    const char* name_;
    u32 last_played_;
};



} // namespace skyland
