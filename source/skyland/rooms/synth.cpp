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


#include "synth.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/composeSynthScene.hpp"
#include "speaker.hpp"



namespace skyland
{



void Synth::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_synth)->c_str();
}



Synth::Synth(Island* parent, const Vec2<u8>& position)
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



void Synth::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (not speaker()) {
        apply_damage(pfrm, app, health_upper_limit());
    }
}



void Synth::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::synth;
}



void Synth::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::synth;
}



ScenePtr<Scene> Synth::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    return scene_pool::alloc<ComposeSynthScene>(app, *this);
}



Speaker* Synth::speaker() const
{
    for (int x = 0; x < 4; ++x) {
        int coord = position().x;
        coord -= x + 1;
        if (coord > -1) {
            if (auto room = parent()->get_room({u8(coord), position().y})) {
                if (str_eq(room->name(), "speaker")) {
                    return dynamic_cast<Speaker*>(room);
                }
            }
        }
    }

    return nullptr;
}



} // namespace skyland
