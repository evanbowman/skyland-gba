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
