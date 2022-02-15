#include "synth.hpp"



namespace skyland {



Synth::Synth(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
}



void Synth::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::synth;
}



void Synth::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::synth;
}



ScenePtr<Scene> Synth::select(Platform& pfrm, App&)
{
    pfrm.speaker().stop_music();

    return null_scene();
}



}
