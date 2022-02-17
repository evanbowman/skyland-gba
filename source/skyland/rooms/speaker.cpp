#include "speaker.hpp"
#include "skyland/island.hpp"
#include "synth.hpp"



namespace skyland {



void Speaker::format_description(StringBuffer<512>& buffer)
{
    buffer += "Plays chiptunes! Connect up to four synth blocks to the "
              "right of the speaker block! When finished playing, activates "
              "whatever block is placed beneath it, if any.";
}



Speaker::Speaker(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
    end_music_ = 0;
    playing_ = 0;
    index_ = 0;
    signal_ = 0;

    settings_.square_1_.envelope_direction_ = 0;
    settings_.square_1_.envelope_step_ = 7;
    settings_.square_1_.volume_ = 12;
    settings_.square_1_.duty_ = 0;
    settings_.square_1_.length_ = 0;

    settings_.square_2_.envelope_direction_ = 0;
    settings_.square_2_.envelope_step_ = 7;
    settings_.square_2_.volume_ = 12;
    settings_.square_2_.duty_ = 0;
    settings_.square_2_.length_ = 0;

    settings_.noise_.envelope_direction_ = 0;
    settings_.noise_.envelope_step_ = 7;
    settings_.noise_.volume_ = 12;
    settings_.noise_.duty_ = 0;
    settings_.noise_.length_ = 0;
}



static const auto time_interval = milliseconds(100);



void Speaker::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (playing_) {
        Room::ready();
    } else if (end_music_) {
        Room::ready();
        if (timer_ < milliseconds(750)) {
            timer_ += delta;
            if (timer_ > milliseconds(750)) {

                pfrm.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::square_1);

                pfrm.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::square_2);

                pfrm.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::noise);

                pfrm.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::wave);

                end_music_ = false;
            }
        }
        return;
    } else {
        return;
    }

    timer_ += delta;


    if (timer_ > time_interval) {

        index_ += 1;

        if (index_ == 16) {
            if (auto room =
                    parent()->get_room({position().x, u8(position().y + 1)})) {
                auto s = dynamic_cast<Speaker*>(room);
                if (s and signal_) {
                    s->play(pfrm);
                } else {
                    if (signal_) {
                        room->select(pfrm, app);
                    }
                    reset(pfrm, true);
                    end_music_ = true;
                }
            } else {
                reset(pfrm, true);
                end_music_ = true;
            }
            playing_ = false;

            parent()->repaint(pfrm, app);

            return;
        }


        pfrm.speaker().init_chiptune_square_1(settings_.square_1_);
        pfrm.speaker().init_chiptune_square_2(settings_.square_2_);
        pfrm.speaker().init_chiptune_noise(settings_.noise_);

        timer_ = 0;

        auto play_note = [&](Platform::Speaker::Channel ch, Synth& s) {
            auto note = s.notes()[index_];
            auto n = (Platform::Speaker::Note)note.note_;
            pfrm.speaker().play_chiptune_note(ch, n, note.octave_);
        };


        if (auto p = square_1()) {
            play_note(Platform::Speaker::Channel::square_1, *p);
        }


        if (auto p = square_2()) {
            play_note(Platform::Speaker::Channel::square_2, *p);
        }


        if (auto w = wave()) {
            play_note(Platform::Speaker::Channel::wave, *w);
        }


        if (auto n = noise()) {
            play_note(Platform::Speaker::Channel::noise, *n);
        }
    }

    if (index_ == -1) {
        return;
    }

    if (auto p = square_1()) {
        pfrm.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::square_1,
            load_effect((int)Platform::Speaker::Channel::square_1),
            p->effect_parameters()[index_].value_,
            delta);
    }


    if (auto n = noise()) {
        pfrm.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::noise,
            load_effect((int)Platform::Speaker::Channel::noise),
            n->effect_parameters()[index_].value_,
            delta);
    }



    if (auto w = wave()) {
        pfrm.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::wave,
            load_effect((int)Platform::Speaker::Channel::wave),
            w->effect_parameters()[index_].value_,
            delta);
    }



    if (auto p = square_2()) {
        pfrm.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::square_2,
            load_effect((int)Platform::Speaker::Channel::square_2),
            p->effect_parameters()[index_].value_,
            delta);
    }
}



Platform::Speaker::Effect Speaker::load_effect(int channel)
{
    return effect_flags_.load(channel, index_);
}



void Speaker::render_interior(App& app, u8 buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



void Speaker::render_exterior(App& app, u8 buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



ScenePtr<Scene> Speaker::select(Platform& pfrm, App& app)
{
    bool was_playing_ = playing_;

    play(pfrm);

    if (not was_playing_) {
        parent()->repaint(pfrm, app);
    }

    return null_scene();
}



void Speaker::reset(Platform& pfrm, bool resume_music)
{
    if (playing_ and resume_music) {
        pfrm.speaker().resume_music();
    }
    playing_ = false;
    end_music_ = false;
}



void Speaker::play(Platform& pfrm, bool signal)
{
    if (playing_) {
        return;
    }

    signal_ = signal;

    for (auto& room : parent()->rooms()) {
        if (auto b = dynamic_cast<Speaker*>(room.get())) {
            b->reset(pfrm, false);
        }
    }

    pfrm.speaker().halt_music();

    Room::ready();

    playing_ = true;
    timer_ = time_interval;
    index_ = -1;
}



Synth* Speaker::square_1() const
{
    u8 x = position().x + 1;
    u8 y = position().y;

    if (auto room = parent()->get_room({x, y})) {
        return dynamic_cast<Synth*>(room);
    }

    return nullptr;
}



Synth* Speaker::noise() const
{
    u8 x = position().x + 4;
    u8 y = position().y;

    // I should explain: do not consider a Synth as belonging to this musical
    // bar if there's a speaker between this block and the synth. Allows players
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



Synth* Speaker::square_2() const
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



Synth* Speaker::wave() const
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



void Speaker::finalize(Platform& pfrm, App& app)
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

        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_1);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_2);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::noise);
        pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::wave);

        pfrm.speaker().resume_music();
    }
}



} // namespace skyland
