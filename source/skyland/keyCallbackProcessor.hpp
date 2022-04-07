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


    using Callback = Function<16, void(Platform&, App&)>;


    struct Binding
    {
        MatchSeq key_seq_;
        Callback callback_;
    };


    void push_binding(const Binding& binding)
    {
        bindings_.emplace_back(binding);
    }


    void update(Platform& pfrm)
    {
        if (seek_state() == seq_max - 1) {
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

    static const int binding_max = 32;

    Buffer<u8, binding_max> possibilities_;
    Buffer<Binding, binding_max> bindings_;
};



extern KeyCallbackProcessor key_callback_processor;



} // namespace skyland
