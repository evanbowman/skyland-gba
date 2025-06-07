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


#include "memory/buffer.hpp"
#include "number/fixnum.hpp"
#include "numeric.hpp"



namespace rng
{

using Value = s32;
using LinearGenerator = Value;

// NOTE: This state should be used for level generation, and for the internal
// state machines for enemies. This state needs to be tightly synchronized
// between multiplayer peers, so do not use this generator for visual effects,
// game state changes only!
extern LinearGenerator critical_state;

// NOTE: use the utility state whenever you need a random value, and you don't
// care whether the value is synchronized across multiplayer games.
extern LinearGenerator utility_state;


Value get(LinearGenerator& gen);


template <Value N> Value choice(LinearGenerator& gen)
{
    return get(gen) % N;
}


inline Value choice(Value n, LinearGenerator& gen)
{
    return get(gen) % n;
}



template <Value N> bool chance(LinearGenerator& gen)
{
    return choice<N>(gen) == 0;
}


inline bool chance(Value n, LinearGenerator& gen)
{
    return choice(n, gen) == 0;
}


template <u32 offset> Fixnum sample(Fixnum n, LinearGenerator& gen)
{
    if (choice<2>(gen)) {
        return n + Fixnum(choice<offset>(gen));

    } else {
        return n - Fixnum(choice<offset>(gen));
    }
}


template <u32 offset> int sample(int n, LinearGenerator& gen)
{
    if (choice<2>(gen)) {
        return n + choice<offset>(gen);

    } else {
        return n - choice<offset>(gen);
    }
}


template <u32 offset>
Vec2<Fixnum> sample(const Vec2<Fixnum>& position, LinearGenerator& gen)
{
    auto result = position;

    result.x = sample<offset>(result.x, gen);
    result.y = sample<offset>(result.y, gen);

    return result;
}


template <typename T, u32 size>
void shuffle(Buffer<T, size>& buffer, LinearGenerator& gen)
{
    int i;
    const int n = buffer.size();
    for (i = n - 1; i > 0; --i) {
        std::swap(buffer[i], buffer[get(gen) % (i + 1)]);
    }
}


} // namespace rng
