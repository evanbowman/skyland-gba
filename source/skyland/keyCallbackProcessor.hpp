#pragma once


#include "platform/key.hpp"
#include "platform/platform.hpp"
#include "function.hpp"



namespace skyland {



class App;



class KeyCallbackProcessor {
public:

    struct MatchSeq {
        Key seq_[8] = {
            Key::count,
            Key::count,
            Key::count,
            Key::count,
            Key::count,
            Key::count,
            Key::count,
            Key::count,
        };
    };


    using Callback = Function<16, void(Platform&, App&)>;


    struct Binding {
        MatchSeq key_seq_;
        Callback callback_;
    };


    void push_binding(const Binding& binding)
    {
        bindings_.push_back(binding);
    }


    void update(Platform& pfrm)
    {
        if (seek_state() == 7) {
            return;
        }

        Key found = Key::count;
        for (int i = 0; i < (int)Key::count; ++i) {
            auto k = (Key)i;
            if (pfrm.keyboard().down_transition(k)) {
                found = k;
                break;
            }
        }

        if (found == Key::count or
            found == Key::start or
            found == Key::alt_1 or
            found == Key::alt_2) {
            return;
        }

        for (auto it = possibilities_.begin(); it not_eq possibilities_.end();) {
            if (bindings_[*it].key_seq_.seq_[seek_state_] == found) {
                ++it;
            } else {
                it = possibilities_.erase(it);
            }
        }
        ++seek_state_;
    }


    void reset()
    {
        seek_state_ = 0;
        for (u32 i = 0; i < bindings_.size(); ++i) {
            possibilities_.push_back(i);
        }
    }


    int seek_state() const
    {
        return seek_state_;
    }


    Binding* match()
    {
        for (u8 p : possibilities_) {
            Binding& binding = bindings_[p];
            if (binding.key_seq_.seq_[seek_state_] == Key::count) {
                return &binding;
            }
        }
        return nullptr;
    }


private:
    int seek_state_ = 0;

    static const int binding_max = 32;

    Buffer<u8, binding_max> possibilities_;
    Buffer<Binding, binding_max> bindings_;
};



extern KeyCallbackProcessor key_callback_processor;



}
