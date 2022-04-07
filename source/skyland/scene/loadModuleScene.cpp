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


#include "loadModuleScene.hpp"
#include "module.hpp"



namespace skyland
{



void LoadModuleScene::enter(Platform& pfrm, App&, Scene& prev)
{
    show_modules(pfrm, 0);
    pfrm.screen().fade(1.f);
}



void LoadModuleScene::show_modules(Platform& pfrm, int page)
{
    auto factory = detail::_Module::factories_;

    int i = 1;

    while (factory) {
        temp_.emplace_back(pfrm, factory->name(), OverlayCoord{1, (u8)i++});
        factory = factory->next_;
    }
}



} // namespace skyland
