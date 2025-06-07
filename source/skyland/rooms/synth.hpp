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

#include "decoration.hpp"
#include "platform/platform.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Speaker;



class Synth final : public Decoration
{
public:
    static void format_description(StringBuffer<512>& buffer);


    Synth(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    static RoomProperties::Bitmask properties()
    {
        return Decoration::properties();
    }


    static const char* name()
    {
        return "synth";
    }


    static SystemString ui_name()
    {
        return SystemString::block_synth;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1768;
    }


    static Icon unsel_icon()
    {
        return 1784;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    bool non_owner_selectable() const override
    {
        return true;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    using Note = Platform::Speaker::NoteDesc;


    struct EffectParameter
    {
        u8 value_;
    };


    Note* notes()
    {
        return notes_;
    }


    EffectParameter* effect_parameters()
    {
        return effect_parameters_;
    }


    Platform::Speaker::Channel channel() const;


    Speaker* speaker() const;


private:
    EffectParameter effect_parameters_[16];
    Note notes_[16];
};



} // namespace skyland
