#pragma once

#include "number/numeric.hpp"



class Platform;



namespace skyland {



// This sound class keeps track of the last time you played the sound, to
// mitigate clipping from overlapping sound effects.



class Sound {
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


private:

    void update(Microseconds delta)
    {
        last_played_ += delta;
    }


    Sound* next_;
    const char* name_;
    u32 last_played_;
};



}
