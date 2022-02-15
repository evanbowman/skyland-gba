#include "synth.hpp"
#include "skyland/scene/composeSynthScene.hpp"
#include "skyland/island.hpp"
#include "measure.hpp"



namespace skyland {



Synth::Synth(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
    for (auto& note : notes_) {
        note.note_ = Platform::Speaker::Note::invalid;
        note.octave_ = 0;
    }

    for (auto& command : commands_) {
        command.value_ = 0;
    }

    if (not measure()) {
        // Freestanding synth not allowed!
        __set_health(0);
    }
}



Platform::Speaker::Channel Synth::channel() const
{
    for (int x = 0; x < 4; ++x) {
        int coord = position().x;
        coord -= x + 1;
        if (coord > 0) {
            if (auto room = parent()->get_room({u8(coord), position().y})) {
                if (str_eq(room->name(), "measure")) {
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

    if (not measure()) {
        apply_damage(pfrm, app, 9999);
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



ScenePtr<Scene> Synth::select(Platform& pfrm, App& app)
{
    return scene_pool::alloc<ComposeSynthScene>(app, *this);
}



Measure* Synth::measure() const
{
    for (int x = 0; x < 4; ++x) {
        int coord = position().x;
        coord -= x + 1;
        if (coord > 0) {
            if (auto room = parent()->get_room({u8(coord), position().y})) {
                if (str_eq(room->name(), "measure")) {
                    return dynamic_cast<Measure*>(room);
                }
            }
        }
    }

    return nullptr;
}



}
