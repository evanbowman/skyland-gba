////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    void update(Microseconds delta) override;


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


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


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
