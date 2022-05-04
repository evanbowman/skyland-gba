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


#include "macrocosmSector.hpp"



namespace skyland::macro::terrain
{



class PancakeSector : public Sector
{
public:
    PancakeSector(Vec2<s8> position) : Sector(position, Shape::pancake)
    {
        erase();
        z_view_ = 4;
    }


    void render_setup(Platform& pfrm) override;


    const Block& get_block(const Vec3<u8>& coord) const override;
    Block& ref_block(const Vec3<u8>& coord) override;


    void set_repaint(bool val) override;

    void shadowcast() override;
    void erase() override;

    void restore(const Persistent& p, u8 blocks[4][12][12]) override;

    void rotate() override;


    void update() override;


    static const int z_limit = 4;
    static const int length = 12;


private:
    Block blocks_[z_limit][length][length];
};



} // namespace skyland::macro::terrain
