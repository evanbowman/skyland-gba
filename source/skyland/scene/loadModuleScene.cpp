////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "loadModuleScene.hpp"
#include "module.hpp"



namespace skyland
{



void LoadModuleScene::enter(Scene& prev)
{
    show_modules(0);
    PLATFORM.screen().fade(1.f);
}



void LoadModuleScene::show_modules(int page)
{
    auto factory = detail::_Module::factories_;

    int i = 1;

    while (factory) {
        temp_.emplace_back(factory->name(), OverlayCoord{1, (u8)i++});
        factory = factory->next_;
    }
}



} // namespace skyland
