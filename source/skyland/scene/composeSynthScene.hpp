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


#include "skyland/rooms/speaker.hpp"
#include "skyland/rooms/synth.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ComposeSynthScene : public ActiveWorldScene
{
public:
    ComposeSynthScene(Synth& synth);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    void repaint();


    void demo_note();


    Vec2<u8> cursor_;

    Optional<Text> heading_;


    Synth::Note notes_[16];
    Synth::EffectParameter effect_parameters_[16];

    Speaker::EffectFlags effect_flags_;

    RoomCoord synth_pos_;
    bool synth_near_;

    bool init_ = true;

    Platform::Speaker::ChannelSettings square_1_settings_;
    Platform::Speaker::ChannelSettings square_2_settings_;
    Platform::Speaker::ChannelSettings noise_settings_;
    u16 wave_settings_;

    u8 last_octave_ = 0;
    u8 last_freq_ = 0;

    u8 resume_y_ = 0;
    u8 demo_index_ = 0;

    Time note_demo_timer_ = 0;


    Platform::Speaker::Channel channel_;
};



} // namespace skyland
