#include "bar.hpp"
#include "synth.hpp"
#include "skyland/island.hpp"



namespace skyland {



Bar::Bar(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
    playing_ = 0;
    index_ = 0;

    square_1_settings_.envelope_direction_ = 0;
    square_1_settings_.envelope_step_ = 7;
    square_1_settings_.volume_ = 12;
    square_1_settings_.duty_ = 0;
    square_1_settings_.length_ = 0;

    square_2_settings_.envelope_direction_ = 0;
    square_2_settings_.envelope_step_ = 7;
    square_2_settings_.volume_ = 12;
    square_2_settings_.duty_ = 0;
    square_2_settings_.length_ = 0;

    noise_settings_.envelope_direction_ = 0;
    noise_settings_.envelope_step_ = 7;
    noise_settings_.volume_ = 12;
    noise_settings_.duty_ = 0;
    noise_settings_.length_ = 0;
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


    if (timer_ > milliseconds(100)) {

        timer_ = 0;

        auto play = [&](Platform::Speaker::Channel ch, Synth& s) {
            auto note = s.notes()[index_];
            auto n = (Platform::Speaker::Note)note.note_;
            pfrm.speaker().play_chiptune_note(ch, n, note.octave_);
        };


        if (auto p = square_1()) {
            play(Platform::Speaker::Channel::square_1, *p);
        }


        if (auto p = square_2()) {
            play(Platform::Speaker::Channel::square_2, *p);
        }


        if (auto w = wave()) {
            play(Platform::Speaker::Channel::wave, *w);
        }


        if (auto n = noise()) {
            play(Platform::Speaker::Channel::noise, *n);
        }


        if (index_ == 15) {
            // if (repeat_) {
            //     --repeat_;
            //     index_ = 0;
            // } else {
                if (auto room = parent()->get_room({
                            position().x, u8(position().y + 1)
                        })) {
                    if (auto b = dynamic_cast<Bar*>(room)) {
                        b->play(pfrm);
                    } else {
                        pfrm.speaker().resume_music();
                    }
                } else {
                    pfrm.speaker().resume_music();
                }
                playing_ = false;
            // }
        } else {
            ++index_;
        }
    }
}



void Bar::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::bar;
}



void Bar::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::bar;
}



ScenePtr<Scene> Bar::select(Platform& pfrm, App&)
{
    play(pfrm);

    return null_scene();
}



void Bar::reset()
{
    playing_ = false;
}



void Bar::play(Platform& pfrm)
{
    if (playing_) {
        return;
    }

    for (auto& room : parent()->rooms()) {
        if (auto b = dynamic_cast<Bar*>(room.get())) {
            b->reset();
        }
    }

    pfrm.speaker().halt_music();

    Room::ready();

    playing_ = true;
    timer_ = 0;
    index_ = 0;
}



Synth* Bar::square_1() const
{
    u8 x = position().x + 1;
    u8 y = position().y;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::square_2() const
{
    u8 x = position().x + 4;
    u8 y = position().y;

    // I should explain: do not consider a Synth as belonging to this musical
    // bar if there's a bar between this block and the synth. Allows players to
    // create longer compositions, by ignoring the noise channel and the weaker
    // pulse channel.
    if (auto room = parent()->get_room({u8(x - 1), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({u8(x - 2), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({u8(x - 3), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::wave() const
{
    u8 x = position().x + 2;
    u8 y = position().y;

    if (auto room = parent()->get_room({u8(x - 1), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Bar::noise() const
{
    u8 x = position().x + 3;
    u8 y = position().y;

    if (auto room = parent()->get_room({u8(x - 1), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({u8(x - 2), y})) {
        if (str_eq(room->name(), name())) {
            return nullptr;
        }
    }

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



void Bar::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (auto p1 = square_1()) {
        p1->apply_damage(pfrm, app, 9999);
    }

    if (auto p2 = square_2()) {
        p2->apply_damage(pfrm, app, 9999);
    }

    if (auto wav = wave()) {
        wav->apply_damage(pfrm, app, 9999);
    }

    if (auto n = noise()) {
        n->apply_damage(pfrm, app, 9999);
    }

    if (playing_) {
        pfrm.speaker().resume_music();
    }
}



}
