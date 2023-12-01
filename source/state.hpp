////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "graphics/overlay.hpp"
#include "memory/uniquePtr.hpp"
#include "number/numeric.hpp"
#include "string.hpp"


class Platform;
class Game;
class State;


using StatePtr = UniquePtr<State, void (*)(State*)>;
using DeferredState = Function<16, StatePtr()>;

class State
{
public:
    virtual void enter(Game&, State& prev_state);
    virtual void exit(Game&, State& next_state);

    // Returns a new state, if we're transitioning to another state, otherwise,
    // if the next state will be the same state, returns an empty state
    // pointer.

    virtual StatePtr update(Platform& platform, Game& game, Microseconds delta);

    State()
    {
    }

    State(const State&) = delete;

    virtual ~State()
    {
    }

    static StatePtr initial(Game&);
};


StatePtr null_state();


using NotificationStr = StringBuffer<70>;
void push_notification(State* state, const NotificationStr& string);


// Yeah, this breaks encapsulation. But this is an edge case, where the boss
// needs to display its own health, but due to state changes outside of an
// individual entity's control, it doesn't make sense for the enemy itself to
// own the GUI's health bar.
void show_boss_health(Game& game, int health_bar, Float percentage);

void hide_boss_health(Game& game);
