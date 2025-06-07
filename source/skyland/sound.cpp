////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sound.hpp"
#include "platform/platform.hpp"



namespace skyland
{



static Sound* __sounds = nullptr;



Sound::Sound(const char* name) : next_(__sounds), name_(name), last_played_(0)
{
    __sounds = this;
}



void Sound::update_all(Time delta)
{
    auto list = __sounds;

    while (list) {
        list->update(delta);
        list = list->next_;
    }
}



void Sound::play(int priority, Time max_overlap)
{
    if ((u32)max_overlap < last_played_) {
        PLATFORM.speaker().play_sound(name_, priority);
        last_played_ = 0;
    }
}



Sound::~Sound()
{
    auto list = __sounds;
    Sound* prev = nullptr;

    while (list) {
        const bool found_self = list == this;
        if (found_self) {
            if (prev) {
                // Unlink ourself from the list.
                prev->next_ = next_;
            } else {
                // We're the first element of the list!
                __sounds = next_;
            }
            return;
        }

        prev = list;
        list = list->next_;
    }
}



} // namespace skyland
