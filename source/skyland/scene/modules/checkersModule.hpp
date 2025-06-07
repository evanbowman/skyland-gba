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

#include "graphics/overlay.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class CheckersModule : public Module<CheckersModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_checkers;
    }


    static u16 icon()
    {
        return 3336;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void init();


    static Factory factory_;

    float scroll_ = 0;

    Vec3<u8> cursor_;

    bool large_board_ = false;

    Optional<Text> board_size_text_;
    Optional<Text> small_text_;
    Optional<Text> large_text_;
};



} // namespace skyland
