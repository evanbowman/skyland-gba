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


#include "sprite.hpp"


Sprite::Sprite()
    : alpha_(Alpha::opaque), size_(Size::w32_h32), flip_x_(false),
      flip_y_(false)
{
    priority_ = 1;
    palette_ = 0;
}


void Sprite::set_scale(const Scale& scale)
{
    scale_ = scale;
}


void Sprite::set_rotation(Rotation rot)
{
    rot_ = rot;
}


void Sprite::set_position(const Vec2<Fixnum>& position)
{
    position_ = position;
}


void Sprite::set_origin(const Vec2<s16>& origin)
{
    origin_ = origin;
}


void Sprite::set_texture_index(TextureIndex texture_index)
{
    texture_index_ = texture_index;
}


void Sprite::set_flip(const Vec2<bool>& flip)
{
    flip_x_ = flip.x;
    flip_y_ = flip.y;
}


void Sprite::set_alpha(Alpha alpha)
{
    alpha_ = alpha;
}


void Sprite::set_mix(const ColorMix& mix)
{
    mix_ = mix;
}


void Sprite::set_size(Size size)
{
    size_ = size;
}


Sprite::Rotation Sprite::get_rotation() const
{
    return rot_;
}


Sprite::Scale Sprite::get_scale() const
{
    return scale_;
}


const Vec2<Fixnum>& Sprite::get_position() const
{
    return position_;
}


const Vec2<s16>& Sprite::get_origin() const
{
    return origin_;
}


TextureIndex Sprite::get_texture_index() const
{
    return texture_index_;
}


Vec2<bool> Sprite::get_flip() const
{
    return {flip_x_, flip_y_};
}


Sprite::Alpha Sprite::get_alpha() const
{
    return static_cast<Sprite::Alpha>(alpha_);
}


const ColorMix& Sprite::get_mix() const
{
    return mix_;
}


Sprite::Size Sprite::get_size() const
{
    return static_cast<Sprite::Size>(size_);
}
