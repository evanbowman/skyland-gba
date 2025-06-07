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



namespace skyland
{



class AdventureModeSettingsScene : public Scene
{
public:
    AdventureModeSettingsScene(bool newgame = false) : newgame_(newgame)
    {
    }

    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void render_line(int linenum,
                     SystemString text,
                     SystemString desc,
                     bool selected);

    void update_field(bool inc);

private:
    void repaint_difficulty(int difficulty, bool selected);
    void repaint_permadeath(bool on, bool selected);
    void repaint_faction(Faction faction, bool selected);

    void repaint();

    Optional<Text> title_;
    Buffer<Text, 3> lines_;

    int sel_ = 0;

    Optional<TextView> desc_;
    bool newgame_;
    u8 original_;
    Bitvector<64> stateflags_cached_;
    Faction prev_faction_;
    bool init_ = true;
};



} // namespace skyland
