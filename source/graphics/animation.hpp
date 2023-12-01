////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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

#include "number/numeric.hpp"
#include "sprite.hpp"


template <TextureIndex InitialTexture, u32 Length, Microseconds Interval>
class Animation
{
public:
    Animation() : timer_(0)
    {
    }

    Animation(Microseconds timer) : timer_(timer)
    {
    }

    bool done(Sprite& sprite) const
    {
        return sprite.get_texture_index() == (InitialTexture + (Length - 1));
    }

    bool at_beginning(Sprite& sprite) const
    {
        return sprite.get_texture_index() == InitialTexture;
    }

    constexpr TextureIndex initial_texture()
    {
        return InitialTexture;
    }

    void bind(Sprite& sprite)
    {
        sprite.set_texture_index(InitialTexture);
        timer_ = 0;
    }

    bool advance(Sprite& sprite, Microseconds dt)
    {
        timer_ += dt;
        const bool ret = [&] {
            if (timer_ > Interval) {
                timer_ -= Interval;
                if (not Animation::done(sprite)) {
                    sprite.set_texture_index(sprite.get_texture_index() + 1);
                } else {
                    // Note: all animations wrap.
                    sprite.set_texture_index(InitialTexture);
                }
                return true;
            }
            return false;
        }();
        return ret;
    }

    bool reverse(Sprite& sprite, Microseconds dt)
    {
        timer_ += dt;
        const bool ret = [&] {
            if (timer_ > Interval) {
                timer_ -= Interval;
                const auto current = sprite.get_texture_index();
                if (current >= InitialTexture) {
                    sprite.set_texture_index(current - 1);
                } else {
                    // Note: all animations wrap.
                    sprite.set_texture_index(InitialTexture + Length - 1);
                }
                return true;
            }
            return false;
        }();
        return ret;
    }

private:
    Microseconds timer_;
};
