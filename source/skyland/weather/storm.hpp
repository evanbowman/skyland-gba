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


#include "environment.hpp"
#include "skyland/island.hpp"



namespace skyland::weather
{



class Storm : public CleanEnvironment
{
private:
    Vec2<s16> raindrops_[6];
    Vec2<s16> last_camera_;

public:
    void update(Platform& pfrm, App& app, Microseconds delta);


    void rewind(Platform& pfrm, App& app, Microseconds delta);


    void display(Platform& pfrm, App& app) override;


    const char* music() const override;


    Platform::Screen::Shader shader(App& app) const override;
};



} // namespace skyland::weather
