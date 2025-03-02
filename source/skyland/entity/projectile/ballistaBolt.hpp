#pragma once

////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2025  Evan Bowman. Some rights reserved.
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

#include "memory/tinyBuffer.hpp"
#include "projectile.hpp"



namespace skyland
{



class BallistaBolt : public Projectile
{
public:
    using PathNode = Vec2<s16>;
    using Path = TinyBuffer<PathNode, 9>;


    static void generate_path(Path& path,
                              Fixnum start_x,
                              Fixnum start_y,
                              Fixnum target_x,
                              Fixnum target_y,
                              Fixnum arc_height);


    struct State
    {
        Time timer_ = 0;
        Path path_;
        u8 path_idx_ = 0;
        u8 interp_ms_ : 7 = 12;
        u8 player_src_ : 1 = 0;
    };


    BallistaBolt(const Vec2<Fixnum>& position,
                 const Vec2<Fixnum>& target,
                 Fixnum arc_height,
                 Island& src);

    BallistaBolt(const Vec2<Fixnum>& pos, const State& state);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void on_collision(Room& room, Vec2<u8> origin) override;
    void on_collision(Entity& entity) override;

    void destroy();


private:
    State state_;
};



} // namespace skyland
