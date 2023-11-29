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


#include "speaker.hpp"
#include "skyland/island.hpp"
#include "synth.hpp"



namespace skyland
{



void Speaker::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_speaker)->c_str();
}



Speaker::Speaker(Island* parent, const RoomCoord& position)
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



void Speaker::update(Microseconds delta)
{
    Room::update(delta);

    if (playing_) {
        Room::ready();
    } else if (end_music_) {
        Room::ready();
        if (timer_ < milliseconds(750)) {
            timer_ += delta;
            if (timer_ > milliseconds(750)) {

                PLATFORM.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::square_1);

                PLATFORM.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::square_2);

                PLATFORM.speaker().stop_chiptune_note(
                    Platform::Speaker::Channel::noise);

                PLATFORM.speaker().stop_chiptune_note(
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
                auto s = room->cast<Speaker>();
                if (s and signal_) {
                    s->play();
                } else {
                    end_music_ = true;
                    if (signal_) {
                        room->select(

                            RoomCoord{position().x, u8(position().y + 1)});
                    } else {
                        reset(true);
                    }
                }
            } else {
                reset(true);
                end_music_ = true;
            }
            playing_ = false;

            parent()->repaint();

            return;
        }


        PLATFORM.speaker().init_chiptune_square_1(settings_.square_1_);
        PLATFORM.speaker().init_chiptune_square_2(settings_.square_2_);
        PLATFORM.speaker().init_chiptune_noise(settings_.noise_);

        timer_ = 0;

        auto play_note = [&](Platform::Speaker::Channel ch, Synth& s) {
            auto note = s.notes()[index_];
            PLATFORM.speaker().play_chiptune_note(ch, note);
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
        PLATFORM.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::square_1,
            load_effect((int)Platform::Speaker::Channel::square_1),
            p->effect_parameters()[index_].value_,
            delta);
    }


    if (auto n = noise()) {
        PLATFORM.speaker().apply_chiptune_effect(
            Platform::Speaker::Channel::noise,
            load_effect((int)Platform::Speaker::Channel::noise),
            n->effect_parameters()[index_].value_,
            delta);
    }

    if (auto p = square_2()) {
        PLATFORM.speaker().apply_chiptune_effect(
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



void Speaker::render_interior(App* app, TileId buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



void Speaker::render_exterior(App* app, TileId buffer[16][16])
{
    if (playing_) {
        buffer[position().x][position().y] = InteriorTile::speaker_active;
    } else {
        buffer[position().x][position().y] = InteriorTile::speaker_inactive;
    }
}



ScenePtr<Scene> Speaker::select(const RoomCoord& cursor)
{
    play();

    parent()->repaint();

    return null_scene();
}



void Speaker::reset(bool resume_music)
{
    playing_ = false;
    end_music_ = false;
}



void Speaker::play(bool signal)
{
    if (playing_) {
        return;
    }

    signal_ = signal;

    for (auto& room : parent()->rooms()) {
        if (auto b = room->cast<Speaker>()) {
            b->reset(false);
            b->end_music_ = false;
        }
    }

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
        return room->cast<Synth>();
    }

    return nullptr;
}



Synth* Speaker::wave() const
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
        return room->cast<Synth>();
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
        return room->cast<Synth>();
    }

    return nullptr;
}



Synth* Speaker::noise() const
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
        return room->cast<Synth>();
    }

    return nullptr;
}



void Speaker::finalize()
{
    Room::finalize();

    if (auto p1 = square_1()) {
        p1->apply_damage(health_upper_limit());
    }

    if (auto p2 = square_2()) {
        p2->apply_damage(health_upper_limit());
    }

    if (auto wav = wave()) {
        wav->apply_damage(health_upper_limit());
    }

    if (auto n = noise()) {
        n->apply_damage(health_upper_limit());
    }

    if (playing_) {

        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::square_1);
        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::square_2);
        PLATFORM.speaker().stop_chiptune_note(
            Platform::Speaker::Channel::noise);
        PLATFORM.speaker().stop_chiptune_note(Platform::Speaker::Channel::wave);
    }
}



} // namespace skyland
