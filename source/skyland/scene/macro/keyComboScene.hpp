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
#include "skyland/scene.hpp"



namespace skyland::macro
{



class KeyComboScene : public Scene
{
public:
    KeyComboScene(bool near)
    {
    }


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    StringBuffer<32> text_data_;
    Optional<Text> text_;
};



} // namespace skyland::macro
