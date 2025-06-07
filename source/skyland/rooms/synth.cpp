////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "synth.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/composeSynthScene.hpp"
#include "speaker.hpp"



namespace skyland
{



void Synth::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_synth)->c_str();
}



Synth::Synth(Island* parent, const RoomCoord& position)
    : Decoration(parent, name(), position)
{
    for (auto& note : notes_) {
        note.regular_.note_ = Platform::Speaker::Note::invalid;
        note.regular_.octave_ = 0;
    }

    for (auto& p : effect_parameters_) {
        p.value_ = 0;
    }

    if (auto s = speaker()) {
        if (s->noise() == this) {
            for (auto& note : notes_) {
                note.noise_freq_.frequency_select_ = 0;
                note.noise_freq_.wide_mode_ = 0;
            }
        }
    }

    // I'd like to do the following, but doing so would impose an ordering that
    // could be problematic when reloading rooms from saved data:
    // if (not speaker()) {
    //     // Freestanding synth not allowed!
    //     __set_health(0);
    // }
}



Platform::Speaker::Channel Synth::channel() const
{
    for (int x = 0; x < 4; ++x) {
        int coord = position().x;
        coord -= x + 1;
        if (coord > 0) {
            if (auto room = parent()->get_room({u8(coord), position().y})) {
                if (str_eq(room->name(), "speaker")) {
                    return (Platform::Speaker::Channel)(x);
                }
            }
        }
    }

    return Platform::Speaker::Channel::invalid;
}



void Synth::update(Time delta)
{
    Room::update(delta);

    if (not speaker()) {
        apply_damage(health_upper_limit());
    }
}



void Synth::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::synth;
}



void Synth::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::synth;
}



ScenePtr Synth::select_impl(const RoomCoord& cursor)
{
    return make_scene<ComposeSynthScene>(*this);
}



Speaker* Synth::speaker() const
{
    for (int x = 0; x < 3; ++x) {
        int coord = position().x;
        coord -= x + 1;
        if (coord > -1) {
            if (auto room = parent()->get_room({u8(coord), position().y})) {
                if (str_eq(room->name(), "speaker")) {
                    return room->cast<Speaker>();
                }
            }
        }
    }

    return nullptr;
}



} // namespace skyland
