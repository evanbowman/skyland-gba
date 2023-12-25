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



ScenePtr<Scene> Synth::select_impl(const RoomCoord& cursor)
{
    return scene_pool::alloc<ComposeSynthScene>(*this);
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
