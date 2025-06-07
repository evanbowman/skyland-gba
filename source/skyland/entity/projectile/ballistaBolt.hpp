#pragma once

////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
