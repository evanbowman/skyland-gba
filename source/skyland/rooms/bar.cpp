#include "bar.hpp"
#include "synth.hpp"
#include "skyland/island.hpp"



namespace skyland {



Bar::Bar(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
    playing_ = 0;
    repeat_ = 0;
    index_ = 0;
}



void Bar::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (playing_) {
        Room::ready();
    } else {
        return;
    }

    timer_ += delta;

    if (timer_ > milliseconds(250)) {

        timer_ = 0;

        if (index_ == 4) {
            pfrm.speaker().play_chiptune_note(0, 1);
        }

        if (index_ == 15) {
            if (repeat_) {

            } else {
                // TODO: play next bar
                playing_ = false;
            }
        } else {
            ++index_;
        }
    }
}



void Bar::render_interior(App& app, u8 buffer[16][16])
{
    for (int x = 0; x < 4; ++x) {
        buffer[position().x + x][position().y] = InteriorTile::bar;
    }

}



void Bar::render_exterior(App& app, u8 buffer[16][16])
{
    for (int x = 0; x < 4; ++x) {
        buffer[position().x + x][position().y] = Tile::bar;
    }
}



ScenePtr<Scene> Bar::select(Platform& pfrm, App&)
{
    pfrm.speaker().stop_music();

    play();

    return null_scene();
}



void Bar::play()
{
    if (playing_) {
        return;
    }

    Room::ready();

    playing_ = true;
    timer_ = 0;
    index_ = 0;
}



Synth* Bar::pulse_1() const
{
    u8 x = position().x;
    u8 y = position().y - 1;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::pulse_2() const
{
    u8 x = position().x + 1;
    u8 y = position().y - 1;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::wave() const
{
    u8 x = position().x + 2;
    u8 y = position().y - 1;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::noise() const
{
    u8 x = position().x + 3;
    u8 y = position().y - 1;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



void Bar::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (auto p1 = pulse_1()) {
        p1->apply_damage(pfrm, app, 9999);
    }

    if (auto p2 = pulse_2()) {
        p2->apply_damage(pfrm, app, 9999);
    }

    if (auto wav = wave()) {
        wav->apply_damage(pfrm, app, 9999);
    }

    if (auto n = noise()) {
        n->apply_damage(pfrm, app, 9999);
    }
}



}
