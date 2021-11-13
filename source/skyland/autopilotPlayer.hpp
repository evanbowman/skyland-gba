#pragma once

#include "platform/platform.hpp"
#include "player.hpp"
#include "script/lisp.hpp"



namespace skyland {



class AutopilotPlayer : public Player {
public:
    AutopilotPlayer(lisp::Value* keys_list);


    void update(Platform&, App&, Microseconds delta) override;


    bool key_down(Platform&, Key k) override;


    bool key_up(Platform&, Key k) override;


    bool key_pressed(Platform&, Key k) override;


private:
    lisp::Protected keys_list_;

    Microseconds next_key_timeout_ = 0;
    std::optional<Key> next_timeout_key_;


    Microseconds key_tap_timeout_ = 0;


    Platform::Keyboard::KeyStates prev_;
    Platform::Keyboard::KeyStates states_;

    Platform::Keyboard::KeyStates taps_;
};



} // namespace skyland
