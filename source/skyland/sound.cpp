#include "sound.hpp"
#include "platform/platform.hpp"



namespace skyland {



static Sound* __sounds = nullptr;



Sound::Sound(const char* name) : next_(__sounds), name_(name), last_played_(0)
{
    __sounds = this;
}



void Sound::update_all(Microseconds delta)
{
    auto list = __sounds;

    while (list) {
        list->update(delta);
        list = list->next_;
    }
}



void Sound::play(Platform& pfrm, int priority, Microseconds max_overlap)
{
    if ((u32)max_overlap < last_played_) {
        pfrm.speaker().play_sound(name_, priority);
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
