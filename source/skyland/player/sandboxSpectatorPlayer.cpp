#include "sandboxSpectatorPlayer.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void SandboxSpectatorPlayer::update(Time delta)
{
    EnemyAI::update(delta);

    for (int i = 0; i < static_cast<int>(Button::count); ++i) {
        if (PLATFORM.input().pressed(static_cast<Button>(i))) {
            button_held_timers_[i] += delta;
        } else {
            button_held_timers_[i] = 0;
        }
    }

    if (PLATFORM.input()
            .down_transition<Button::up, Button::down, Button::left, Button::right>()) {
        APP.camera()->reset_default();
    }
}



bool SandboxSpectatorPlayer::button_down(Button k)
{
    return PLATFORM.input().down_transition(k);
}



bool SandboxSpectatorPlayer::button_up(Button k)
{
    return PLATFORM.input().up_transition(k);
}



bool SandboxSpectatorPlayer::button_pressed(Button k)
{
    return PLATFORM.input().pressed(k);
}



bool SandboxSpectatorPlayer::button_held(Button k, Time duration)
{
    return button_held_timers_[static_cast<int>(k)] >= duration;
}



void SandboxSpectatorPlayer::button_held_reset(Button k, Time decrement)
{
    button_held_timers_[static_cast<int>(k)] -= decrement;
}



void SandboxSpectatorPlayer::button_held_distribute(const Button* include_list)
{
    // If any button in the include_list is pressed, distribute that button press to
    // all buttons in the include list.

    int max = 0;

    auto l = include_list;

    while (*l not_eq Button::null) {
        if (button_held_timers_[static_cast<int>(*l)] > max) {
            max = button_held_timers_[static_cast<int>(*l)];
        }
        ++l;
    }

    l = include_list;

    while (*l not_eq Button::null) {
        if (PLATFORM.input().pressed(*l)) {
            button_held_timers_[static_cast<int>(*l)] = max;
        }
        ++l;
    }
}



} // namespace skyland
