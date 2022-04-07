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


#include "skyland/rooms/speaker.hpp"
#include "skyland/rooms/synth.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ComposeSynthScene : public ActiveWorldScene
{
public:
    ComposeSynthScene(App& app, Synth& synth);


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    void repaint(Platform&);


    void demo_note(Platform&);


    Vec2<u8> cursor_;

    std::optional<Text> heading_;


    Synth::Note notes_[16];
    Synth::EffectParameter effect_parameters_[16];

    Speaker::EffectFlags effect_flags_;

    Vec2<u8> synth_pos_;
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

    Microseconds note_demo_timer_ = 0;


    Platform::Speaker::Channel channel_;
};



} // namespace skyland
