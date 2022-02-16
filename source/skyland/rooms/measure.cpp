#include "measure.hpp"
#include "synth.hpp"
#include "skyland/island.hpp"



namespace skyland {



void Measure::format_description(StringBuffer<512>& buffer)
{
    buffer += "Plays chiptunes! Connect up to four synth blocks to the "
        "right of the speaker block! When finished playing, activates "
        "a speaker block placed beneath it, if any.";
}



Measure::Measure(Island* parent, const Vec2<u8>& position)
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



static const auto time_interval = milliseconds(100);



void Measure::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (playing_) {
        Room::ready();
    } else {
        return;
    }

    timer_ += delta;


    if (timer_ > time_interval) {

        index_ += 1;

        if (index_ == 16) {
            if (auto room = parent()->get_room({
                        position().x, u8(position().y + 1)
                    })) {
                if (auto b = dynamic_cast<Measure*>(room)) {
                    b->play(pfrm);
                } else {
                    pfrm.speaker().resume_music();
                }
            } else {
                pfrm.speaker().resume_music();
            }
            playing_ = false;

            parent()->repaint(pfrm, app);

            return;
        }


        pfrm.speaker().init_chiptune_square_1(square_1_settings_);
        pfrm.speaker().init_chiptune_square_2(square_2_settings_);
        pfrm.speaker().init_chiptune_noise(noise_settings_);

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
    }

    if (index_ == -1) {
        return;
    }

    if (auto p = square_1()) {
        pfrm.speaker().apply_chiptune_effect(Platform::Speaker::Channel::square_1,
                                             load_effect((int)Platform::Speaker::Channel::square_1),
                                             p->effect_parameters()[index_].value_,
                                             delta);
    }


    if (auto n = noise()) {
        pfrm.speaker().apply_chiptune_effect(Platform::Speaker::Channel::noise,
                                             load_effect((int)Platform::Speaker::Channel::noise),
                                             n->effect_parameters()[index_].value_,
                                             delta);
    }



    if (auto w = wave()) {
        pfrm.speaker().apply_chiptune_effect(Platform::Speaker::Channel::wave,
                                             load_effect((int)Platform::Speaker::Channel::wave),
                                             w->effect_parameters()[index_].value_,
                                             delta);
    }



    if (auto p = square_2()) {
        pfrm.speaker().apply_chiptune_effect(Platform::Speaker::Channel::square_2,
                                             load_effect((int)Platform::Speaker::Channel::square_2),
                                             p->effect_parameters()[index_].value_,
                                             delta);
    }
}



Platform::Speaker::Effect Measure::load_effect(int channel)
{
    return effect_flags_.load(channel, index_);
}



void Measure::render_interior(App& app, u8 buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



void Measure::render_exterior(App& app, u8 buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



ScenePtr<Scene> Measure::select(Platform& pfrm, App& app)
{
    bool was_playing_ = playing_;

    play(pfrm);

    if (not was_playing_) {
        parent()->repaint(pfrm, app);
    }

    return null_scene();
}



void Measure::reset(Platform& pfrm, bool resume_music)
{
    if (playing_ and resume_music) {
        pfrm.speaker().resume_music();
    }
    playing_ = false;
}



void Measure::play(Platform& pfrm)
{
    if (playing_) {
        return;
    }

    for (auto& room : parent()->rooms()) {
        if (auto b = dynamic_cast<Measure*>(room.get())) {
            b->reset(pfrm, false);
        }
    }

    pfrm.speaker().halt_music();

    Room::ready();

    playing_ = true;
    timer_ = time_interval;
    index_ = -1;
}



Synth* Measure::square_1() const
{
    u8 x = position().x + 1;
    u8 y = position().y;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Measure::noise() const
{
    u8 x = position().x + 4;
    u8 y = position().y;

    // I should explain: do not consider a Synth as belonging to this musical
    // bar if there's a measure between this block and the synth. Allows players
    // to create longer compositions, by ignoring the noise channel and the
    // weaker pulse channel.
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



Synth* Measure::square_2() const
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



Synth* Measure::wave() const
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



void Measure::finalize(Platform& pfrm, App& app)
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
