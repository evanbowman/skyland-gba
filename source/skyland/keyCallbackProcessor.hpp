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


#include "function.hpp"
#include "platform/key.hpp"
#include "platform/platform.hpp"



namespace skyland
{



class App;



class KeyCallbackProcessor
{
public:
    static const int seq_max = 11;


    struct MatchSeq
    {
        Key seq_[seq_max] = {
            Key::count,
            Key::count,
            Key::count,
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


    using Callback = Function<2 * sizeof(void*), void()>;


    struct Binding
    {
        MatchSeq key_seq_;
        Callback callback_;
    };


    void push_binding(const Binding& binding)
    {
        bindings_.emplace_back(binding);
    }


    void update()
    {
        if (seek_state() == seq_max - 1) {
            return;
        }

        Key found = Key::count;
        for (int i = 0; i < (int)Key::count; ++i) {
            auto k = (Key)i;
            if (PLATFORM.keyboard().down_transition(k)) {
                found = k;
                break;
            }
        }

        if (found == Key::count or found == Key::start or found == Key::alt_1 or
            found == Key::alt_2) {
            return;
        }

        for (auto it = possibilities_.begin();
             it not_eq possibilities_.end();) {
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
        possibilities_.clear();
        for (u32 i = 0; i < bindings_.size(); ++i) {
            possibilities_.push_back(i);
        }
    }


    void clear()
    {
        bindings_.clear();
        reset();
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


    u32 possibilities()
    {
        return possibilities_.size();
    }


private:
    int seek_state_ = 0;

    static const int binding_max = 8;

    Buffer<u8, binding_max> possibilities_;
    Buffer<Binding, binding_max> bindings_;
};



} // namespace skyland
