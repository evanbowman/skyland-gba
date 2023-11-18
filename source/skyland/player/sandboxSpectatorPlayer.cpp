#include "sandboxSpectatorPlayer.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void SandboxSpectatorPlayer::update(App& app, Microseconds delta)
{
    EnemyAI::update(app, delta);

    for (int i = 0; i < static_cast<int>(Key::count); ++i) {
        if (PLATFORM.keyboard().pressed(static_cast<Key>(i))) {
            key_held_timers_[i] += delta;
        } else {
            key_held_timers_[i] = 0;
        }
    }

    if (PLATFORM.keyboard()
            .down_transition<Key::up, Key::down, Key::left, Key::right>()) {
        app.camera()->reset_default(app);
    }
}



bool SandboxSpectatorPlayer::key_down(Key k)
{
    return PLATFORM.keyboard().down_transition(k);
}



bool SandboxSpectatorPlayer::key_up(Key k)
{
    return PLATFORM.keyboard().up_transition(k);
}



bool SandboxSpectatorPlayer::key_pressed(Key k)
{
    return PLATFORM.keyboard().pressed(k);
}



bool SandboxSpectatorPlayer::key_held(Key k, Microseconds duration)
{
    return key_held_timers_[static_cast<int>(k)] >= duration;
}



void SandboxSpectatorPlayer::key_held_reset(Key k, Microseconds decrement)
{
    key_held_timers_[static_cast<int>(k)] -= decrement;
}



void SandboxSpectatorPlayer::key_held_distribute(const Key* include_list)
{
    // If any key in the include_list is pressed, distribute that key press to
    // all keys in the include list.

    int max = 0;

    auto l = include_list;

    while (*l not_eq Key::null) {
        if (key_held_timers_[static_cast<int>(*l)] > max) {
            max = key_held_timers_[static_cast<int>(*l)];
        }
        ++l;
    }

    l = include_list;

    while (*l not_eq Key::null) {
        if (PLATFORM.keyboard().pressed(*l)) {
            key_held_timers_[static_cast<int>(*l)] = max;
        }
        ++l;
    }
}



} // namespace skyland
