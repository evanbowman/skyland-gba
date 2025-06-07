////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "composeSynthScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/rooms/speaker.hpp"



// NOTE: I wrote this code specifically to take advantage of the DMG sound
// channels on the GBA. This is basically throwaway code that I don't expect to
// need to port or maintain. I don't really ever intend to emulate DMG sound
// channels on non-gba build targets. So I made no effort to make this
// SynthComposer Scene code neat. Written in a couple of hours.



void print_char(utf8::Codepoint c,
                const OverlayCoord& coord,
                const Optional<FontColors>& colors = {});



namespace skyland
{



ComposeSynthScene::ComposeSynthScene(Synth& synth)
    : synth_pos_(synth.position()),
      synth_near_(synth.parent() == &player_island()), channel_(synth.channel())
{
    memcpy(notes_, synth.notes(), sizeof notes_);
    memcpy(effect_parameters_,
           synth.effect_parameters(),
           sizeof effect_parameters_);

    if (auto speaker = synth.speaker()) {
        square_1_settings_ = speaker->settings_.square_1_;
        square_2_settings_ = speaker->settings_.square_2_;
        noise_settings_ = speaker->settings_.noise_;
        wave_settings_ = speaker->settings_.wave_;

        effect_flags_ = speaker->effect_flags();
    }
}



ScenePtr ComposeSynthScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    if (player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (player().key_down(Key::action_1)) {
        if (cursor_.x == 0) {
            demo_note();
        }
    }

    if (note_demo_timer_ > 0) {
        note_demo_timer_ -= delta;
        if (note_demo_timer_ < 0) {
            PLATFORM_EXTENSION(psg_stop_note, channel_);
        }
    }


    PLATFORM_EXTENSION(
        psg_apply_effect,
        Platform::Speaker::Channel::square_1,
        effect_flags_.load((int)Platform::Speaker::Channel::square_1,
                           demo_index_),
        effect_parameters_[demo_index_].value_,
        delta);


    if (not player().key_pressed(Key::action_1)) {
        if (test_key(Key::down)) {
            if (cursor_.y < 15) {
                ++cursor_.y;
                repaint();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_.y > 0) {
                --cursor_.y;
                repaint();
            }
        }
    } else if (cursor_.x == 0) {

        if (test_key(Key::down)) {

            if (channel_ == Platform::Speaker::Channel::noise) {
                if (notes_[cursor_.y].noise_freq_.frequency_select_ == 0 and
                    last_freq_) {
                    notes_[cursor_.y].noise_freq_.frequency_select_ =
                        last_freq_;
                } else if (notes_[cursor_.y].noise_freq_.frequency_select_ <
                           36) {
                    // The frequencies past table entry 36 are practically inaudible.
                    notes_[cursor_.y].noise_freq_.frequency_select_++;
                    last_freq_ =
                        notes_[cursor_.y].noise_freq_.frequency_select_;
                } else {
                    notes_[cursor_.y].noise_freq_.frequency_select_ = 0;
                    last_freq_ =
                        notes_[cursor_.y].noise_freq_.frequency_select_;
                }
            } else {
                if (notes_[cursor_.y].regular_.note_ ==
                    Platform::Speaker::Note::invalid) {
                    notes_[cursor_.y].regular_.octave_ = last_octave_;
                }

                notes_[cursor_.y].regular_.note_ = (Platform::Speaker::Note)(
                    ((u8)notes_[cursor_.y].regular_.note_ + 1));

                if ((u8)notes_[cursor_.y].regular_.note_ >
                    (int)(Platform::Speaker::Note::B)) {
                    notes_[cursor_.y].regular_.note_ =
                        Platform::Speaker::Note::invalid;
                }
            }

            demo_note();

            repaint();
        }

        if (test_key(Key::up)) {

            if (channel_ == Platform::Speaker::Channel::noise) {
                if (notes_[cursor_.y].noise_freq_.frequency_select_ > 0) {
                    notes_[cursor_.y].noise_freq_.frequency_select_--;
                    last_freq_ =
                        notes_[cursor_.y].noise_freq_.frequency_select_;
                } else {
                    if (last_freq_) {
                        notes_[cursor_.y].noise_freq_.frequency_select_ =
                            last_freq_;
                    } else {
                        notes_[cursor_.y].noise_freq_.frequency_select_ = 36;
                    }
                }
            } else {
                if (notes_[cursor_.y].regular_.note_ ==
                    Platform::Speaker::Note::invalid) {
                    notes_[cursor_.y].regular_.octave_ = last_octave_;
                }

                if (notes_[cursor_.y].regular_.note_ ==
                    Platform::Speaker::Note::invalid) {
                    notes_[cursor_.y].regular_.note_ =
                        Platform::Speaker::Note::B;
                } else {
                    notes_[cursor_.y].regular_.note_ =
                        (Platform::Speaker::Note)(
                            ((u8)notes_[cursor_.y].regular_.note_ - 1));
                }
            }

            demo_note();

            repaint();
        }

    } else if (cursor_.x == 1) {

        if (notes_[cursor_.y].regular_.note_ not_eq
            Platform::Speaker::Note::invalid) {
            if (test_key(Key::down)) {

                if (channel_ == Platform::Speaker::Channel::noise) {
                    notes_[cursor_.y].noise_freq_.wide_mode_ =
                        not notes_[cursor_.y].noise_freq_.wide_mode_;

                } else {
                    notes_[cursor_.y].regular_.octave_ += 1;
                    if (notes_[cursor_.y].regular_.octave_ > 6) {
                        notes_[cursor_.y].regular_.octave_ = 0;
                    }

                    last_octave_ = notes_[cursor_.y].regular_.octave_;
                }

                demo_note();

                repaint();
            }

            if (test_key(Key::up)) {

                if (channel_ == Platform::Speaker::Channel::noise) {
                    notes_[cursor_.y].noise_freq_.wide_mode_ =
                        not notes_[cursor_.y].noise_freq_.wide_mode_;
                } else {
                    if (notes_[cursor_.y].regular_.octave_ == 0) {
                        notes_[cursor_.y].regular_.octave_ = 6;
                    } else {
                        notes_[cursor_.y].regular_.octave_ -= 1;
                    }

                    last_octave_ = notes_[cursor_.y].regular_.octave_;
                }

                demo_note();

                repaint();
            }
        }

    } else if (cursor_.x == 2) {
        auto set_effect_param_default = [&](Platform::Speaker::Effect e) {
            auto& param = effect_parameters_[cursor_.y];
            switch (e) {
            case Platform::Speaker::Effect::none:
                param.value_ = 0;
                break;

            case Platform::Speaker::Effect::vibrato:
                param.value_ = 0x1e;
                break;

            case Platform::Speaker::Effect::duty:
                param.value_ = 0;
                break;

            case Platform::Speaker::Effect::envelope:
                param.value_ = 0xf7;
                break;
            }
        };
        if (test_key(Key::down)) {
            auto effect = effect_flags_.load((int)channel_, cursor_.y);
            if ((int)effect < 3) {
                effect = (Platform::Speaker::Effect)((int)effect + 1);
            } else {
                effect = (Platform::Speaker::Effect)0;
            }
            effect_flags_.store((int)channel_, cursor_.y, effect);
            set_effect_param_default(effect);
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }

        } else if (test_key(Key::up)) {
            auto effect = effect_flags_.load((int)channel_, cursor_.y);
            if ((int)effect > 0) {
                effect = (Platform::Speaker::Effect)((int)effect - 1);
            } else {
                effect = (Platform::Speaker::Effect)3;
            }
            effect_flags_.store((int)channel_, cursor_.y, effect);
            set_effect_param_default(effect);
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }
        }
    } else if (cursor_.x == 3) {

        int upper_limit = [&] {
            auto e = effect_flags_.load((int)channel_, cursor_.y);
            switch (e) {
            case Platform::Speaker::Effect::none:
                return 0;

            case Platform::Speaker::Effect::duty:
                return 3;

            case Platform::Speaker::Effect::vibrato:
                return 15;

            case Platform::Speaker::Effect::envelope:
                return 15;

            default:
                return 0;
            }
        }();

        auto p = effect_parameters_[cursor_.y];
        auto val = 0x0f & (p.value_ >> 4);
        if (test_key(Key::up)) {
            if (val > 0) {
                val -= 1;
            } else {
                val = upper_limit;
            }
            p.value_ &= ~0xf0;
            p.value_ |= val << 4;
            effect_parameters_[cursor_.y] = p;
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }

        } else if (test_key(Key::down)) {
            if (val < upper_limit) {
                val += 1;
            } else {
                val = 0;
            }
            p.value_ &= ~0xf0;
            p.value_ |= val << 4;
            effect_parameters_[cursor_.y] = p;
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }
        }
    } else if (cursor_.x == 4) {

        int upper_limit = [&] {
            auto e = effect_flags_.load((int)channel_, cursor_.y);
            switch (e) {
            case Platform::Speaker::Effect::none:
                return 0;

            case Platform::Speaker::Effect::duty:
                return 0;

            case Platform::Speaker::Effect::vibrato:
                return 15;

            case Platform::Speaker::Effect::envelope:
                return 15;

            default:
                return 0;
            }
        }();

        auto p = effect_parameters_[cursor_.y];
        auto val = 0x0f & p.value_;
        if (test_key(Key::up)) {
            if (val > 0) {
                val -= 1;
            } else {
                val = upper_limit;
            }
            p.value_ &= ~0x0f;
            p.value_ |= val;
            effect_parameters_[cursor_.y] = p;
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }

        } else if (test_key(Key::down)) {
            if (val < upper_limit) {
                val += 1;
            } else {
                val = 0;
            }
            p.value_ &= ~0x0f;
            p.value_ |= val;
            effect_parameters_[cursor_.y] = p;
            repaint();

            if (notes_[cursor_.y].regular_.note_ not_eq
                Platform::Speaker::Note::invalid) {
                demo_note();
            }
        }
    } else {

        auto generic_update_settings =
            [&](Platform::Speaker::ChannelSettings& s) {
                if (test_key(Key::down)) {
                    switch (cursor_.y) {
                    case 0:
                        if (s.envelope_step_ == 7) {
                            s.envelope_step_ = 0;
                        } else {
                            s.envelope_step_ += 1;
                        }
                        break;

                    case 1:
                        if (s.envelope_direction_) {
                            s.envelope_direction_ = 0;
                        } else {
                            s.envelope_direction_ = 1;
                        }
                        break;

                    case 2:
                        if (s.duty_ == 3) {
                            s.duty_ = 0;
                        } else {
                            s.duty_ += 1;
                        }
                        break;

                    case 3:
                        if (s.length_ == 63) {
                            s.length_ = 0;
                        } else {
                            s.length_ += 1;
                        }
                        break;

                    case 4:
                        if (s.volume_ == 15) {
                            s.volume_ = 0;
                        } else {
                            s.volume_ += 1;
                        }
                        break;
                    }

                    repaint();

                } else if (test_key(Key::up)) {
                    switch (cursor_.y) {
                    case 0:
                        if (s.envelope_step_ == 0) {
                            s.envelope_step_ = 7;
                        } else {
                            s.envelope_step_ -= 1;
                        }
                        break;

                    case 1:
                        if (s.envelope_direction_) {
                            s.envelope_direction_ = 0;
                        } else {
                            s.envelope_direction_ = 1;
                        }
                        break;

                    case 2:
                        if (s.duty_ == 0) {
                            s.duty_ = 3;
                        } else {
                            s.duty_ -= 1;
                        }
                        break;

                    case 3:
                        if (s.length_ == 0) {
                            s.length_ = 63;
                        } else {
                            s.length_ -= 1;
                        }
                        break;

                    case 4:
                        if (s.volume_ == 0) {
                            s.volume_ = 15;
                        } else {
                            s.volume_ -= 1;
                        }
                        break;
                    }

                    repaint();
                }
            };

        switch (channel_) {
        case Platform::Speaker::Channel::square_1:
            generic_update_settings(square_1_settings_);
            break;

        case Platform::Speaker::Channel::square_2:
            generic_update_settings(square_2_settings_);
            break;

        case Platform::Speaker::Channel::noise:
            generic_update_settings(noise_settings_);
            break;

        default:
        case Platform::Speaker::Channel::wave:
            // TODO...
            break;
        }
    }

    if (player().key_down(Key::right) and cursor_.x < 5) {
        ++cursor_.x;
        if (cursor_.x == 5) {
            resume_y_ = cursor_.y;
            cursor_.y = 0;
        }
        repaint();
    }

    if (player().key_down(Key::left) and cursor_.x > 0) {
        if (cursor_.x == 5) {
            cursor_.y = resume_y_;
        }
        --cursor_.x;
        repaint();
    }


    return null_scene();
}



void ComposeSynthScene::demo_note()
{
    if (cursor_.x < 4) {
        demo_index_ = cursor_.y;
    } else {
        // We're playing with the settings column on the right, which isn't
        // aligned to notes. Play the note associated with our cached y-value.
        demo_index_ = resume_y_;
    }

    PLATFORM_EXTENSION(psg_init_square_1, square_1_settings_);
    PLATFORM_EXTENSION(psg_init_square_2, square_2_settings_);
    PLATFORM_EXTENSION(psg_init_noise, noise_settings_);

    PLATFORM_EXTENSION(psg_play_note, channel_, notes_[demo_index_]);

    note_demo_timer_ = seconds(3);
}



void ComposeSynthScene::repaint()
{
    const auto st = calc_screen_tiles();
    int start_x = st.x / 2 - 12;
    int start_y = (st.y - 16) / 2;


    auto highlight = Text::OptColors{
        {ColorConstant::silver_white, ColorConstant::aerospace_orange}};


    auto put_char =
        [&](char c, int x, int y, const Optional<FontColors>& colors = {}) {
            auto clr = colors;

            if (not colors) {
                clr = Text::OptColors{
                    {ColorConstant::steel_blue, ColorConstant::silver_white}};
            }

            print_char(c, {u8(start_x + x), u8(start_y + y)}, clr);
        };

    auto put_str = [&](const char* str,
                       int x,
                       int y,
                       const Optional<FontColors>& colors = {}) {
        auto clr = colors;

        if (not colors) {
            clr = Text::OptColors{
                {ColorConstant::steel_blue, ColorConstant::silver_white}};
        }

        while (*str not_eq '\0') {

            print_char(*str, {u8(start_x + x), u8(start_y + y)}, clr);

            ++x;
            ++str;
        }
    };


    for (u8 y = 0; y < 16; ++y) {

        if (y < 10) {
            put_char('0' + y, 0, y);
        } else {
            put_char('a' + (y - 10), 0, y);
        }

        put_char(' ', 1, y);
        put_char(' ', -1, y);

        auto note = notes_[y];

        auto str = [&] {
            switch (note.regular_.note_) {
            case Platform::Speaker::Note::C:
                return "C ";

            case Platform::Speaker::Note::CIS:
                return "C#";

            case Platform::Speaker::Note::D:
                return "D ";

            case Platform::Speaker::Note::DIS:
                return "D#";

            case Platform::Speaker::Note::E:
                return "E ";

            case Platform::Speaker::Note::F:
                return "F ";

            case Platform::Speaker::Note::FIS:
                return "F#";

            case Platform::Speaker::Note::G:
                return "G ";

            case Platform::Speaker::Note::GIS:
                return "G#";

            case Platform::Speaker::Note::A:
                return "A ";

            case Platform::Speaker::Note::AIS:
                return "A#";

            case Platform::Speaker::Note::B:
                return "B ";

            case Platform::Speaker::Note::invalid:
                return "--";

            case Platform::Speaker::Note::count:
                return "XX";
            }

            return "C ";
        }();


        if (channel_ == Platform::Speaker::Channel::noise) {
            StringBuffer<2> sel;
            StringBuffer<1> wide;
            if (note.noise_freq_.wide_mode_) {
                wide = "<";
            } else {
                wide = ">";
            }

            if (note.noise_freq_.frequency_select_ == 0) {
                sel = "--";
                wide = "-";
            } else if (note.noise_freq_.frequency_select_ < 10) {
                sel += "0";
                sel += stringify(note.noise_freq_.frequency_select_);
            } else {
                sel += stringify(note.noise_freq_.frequency_select_);
            }

            if (cursor_.y == y and cursor_.x == 0) {
                put_char(sel[0], 2, y, highlight);
                put_char(sel[1], 3, y, highlight);
                put_char(wide[0], 4, y);

            } else if (cursor_.y == y and cursor_.x == 1) {
                put_char(sel[0], 2, y);
                put_char(sel[1], 3, y);
                put_char(wide[0], 4, y, highlight);

            } else {
                put_char(sel[0], 2, y);
                put_char(sel[1], 3, y);
                put_char(wide[0], 4, y);
            }

            put_char(' ', 5, y);

        } else {
            StringBuffer<4> oct;

            if (note.regular_.note_ not_eq Platform::Speaker::Note::invalid) {
                oct += stringify(note.regular_.octave_);
            } else {
                oct = "-";
            }

            if (cursor_.y == y and cursor_.x == 0) {
                put_char(str[0], 2, y, highlight);
                put_char(str[1], 3, y, highlight);
                put_char(oct[0], 4, y);

            } else if (cursor_.y == y and cursor_.x == 1) {
                put_char(str[0], 2, y);
                put_char(str[1], 3, y);
                put_char(oct[0], 4, y, highlight);

            } else {
                put_char(str[0], 2, y);
                put_char(str[1], 3, y);
                put_char(oct[0], 4, y);
            }

            put_char(' ', 5, y);
        }



        auto effect_sym = [&] {
            switch (effect_flags_.load((int)channel_, y)) {
            case Platform::Speaker::Effect::none:
                return '-';

            case Platform::Speaker::Effect::vibrato:
                return 'v';

            case Platform::Speaker::Effect::duty:
                return 'w';

            case Platform::Speaker::Effect::envelope:
                return 'e';

            default:
                return '?';
            }
        }();

        if (cursor_.y == y and cursor_.x == 2) {
            put_char(effect_sym, 6, y, highlight);
        } else {
            put_char(effect_sym, 6, y);
        }

        auto effect_param = effect_parameters_[y];
        const auto p1 = [&] {
            auto val = 0x0f & (effect_param.value_ >> 4);
            if (val < 10) {
                return '0' + val;
            } else {
                return 'a' + (val - 10);
            }
        }();

        const auto p2 = [&] {
            auto val = 0x0f & effect_param.value_;
            if (val < 10) {
                return '0' + val;
            } else {
                return 'a' + (val - 10);
            }
        }();

        if (cursor_.y == y and cursor_.x == 3) {
            put_char(p1, 7, y, highlight);
        } else {
            put_char(p1, 7, y);
        }

        if (cursor_.y == y and cursor_.x == 4) {
            put_char(p2, 8, y, highlight);
        } else {
            put_char(p2, 8, y);
        }

        if (init_) {
            for (int x = 9; x < 22; ++x) {
                put_char(' ', x, y);
            }
        }
    }

    if (init_) {
        switch (channel_) {
        case Platform::Speaker::Channel::square_1:
        case Platform::Speaker::Channel::square_2:
        case Platform::Speaker::Channel::noise:
            put_str("envelope", 10, 1);
            put_str("envlp-dir", 10, 3);
            put_str("duty", 10, 5);
            put_str("length", 10, 7);
            put_str("volume", 10, 9);
            break;


        default:
        case Platform::Speaker::Channel::wave:
            // TODO...
            break;
        }
    }

    auto show_channel_settings = [&](Platform::Speaker::ChannelSettings& s) {
        auto str_fill = [&](u8 val) {
            StringBuffer<2> str;
            if (val < 10) {
                str += "0";
            }
            str += stringify(val);
            return str;
        };

        if (cursor_.x == 5 and cursor_.y == 0) {
            if (s.envelope_step_ == 0) {
                put_char('8', 20, 1, highlight);
            } else {
                put_char(stringify(s.envelope_step_)[0], 20, 1, highlight);
            }
        } else {
            if (s.envelope_step_ == 0) {
                put_char('8', 20, 1);
            } else {
                put_char(stringify(s.envelope_step_)[0], 20, 1);
            }
        }

        if (cursor_.x == 5 and cursor_.y == 1) {
            put_char(stringify(s.envelope_direction_)[0], 20, 3, highlight);
        } else {
            put_char(stringify(s.envelope_direction_)[0], 20, 3);
        }

        put_str(
            [&] {
                switch (s.duty_) {
                case 0:
                    return "12";
                case 1:
                    return "25";
                default:
                case 2:
                    return "50";
                case 3:
                    return "75";
                }
            }(),
            19,
            5,
            (cursor_.x == 5 and cursor_.y == 2) ? highlight : std::nullopt);

        put_str(str_fill(s.length_).c_str(),
                19,
                7,
                (cursor_.x == 5 and cursor_.y == 3) ? highlight : std::nullopt);

        put_str(str_fill(s.volume_).c_str(),
                19,
                9,
                (cursor_.x == 5 and cursor_.y == 4) ? highlight : std::nullopt);
    };

    switch (channel_) {
    case Platform::Speaker::Channel::square_1:
        show_channel_settings(square_1_settings_);
        break;

    case Platform::Speaker::Channel::square_2:
        show_channel_settings(square_2_settings_);
        break;

    case Platform::Speaker::Channel::noise:
        show_channel_settings(noise_settings_);
        break;

    default:
    case Platform::Speaker::Channel::wave:
        // TODO...
        break;
    }

    init_ = false;
}



void ComposeSynthScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    repaint();

    auto island = synth_near_ ? &player_island() : opponent_island();

    for (auto& room : island->rooms()) {
        // Stop any currently-playing chiptunes.
        if (auto b = room->cast<Speaker>()) {
            b->reset();
        }
    }

    island->repaint();


    PLATFORM.screen().schedule_fade(0.5f);

    const auto st = calc_screen_tiles();
    int start_x = st.x / 2 - 13;
    int start_y = (st.y - 16) / 2 - 1;

    heading_.emplace(OverlayCoord{(u8)start_x, (u8)start_y});

    heading_->assign(
        [&]() {
            switch (channel_) {
            case Platform::Speaker::Channel::square_1:
                return "tone_1";

            case Platform::Speaker::Channel::wave:
                return "wave";

            case Platform::Speaker::Channel::noise:
                return "noise";

            case Platform::Speaker::Channel::square_2:
                return "tone_2";

            default:
            case Platform::Speaker::Channel::invalid:
                return "!?";
            }
        }(),
        Text::OptColors{
            {ColorConstant::steel_blue, ColorConstant::silver_white}});


    PLATFORM.speaker().set_music_volume(6);
}



void ComposeSynthScene::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);

    PLATFORM_EXTENSION(psg_stop_note, channel_);

    PLATFORM.screen().schedule_fade(0.f);

    if (synth_near_) {
        if (auto room = player_island().get_room(synth_pos_)) {
            if (auto s = room->cast<Synth>()) {
                memcpy(s->notes(), notes_, sizeof notes_);
                memcpy(s->effect_parameters(),
                       effect_parameters_,
                       sizeof effect_parameters_);

                if (auto speaker = s->speaker()) {
                    speaker->settings_.square_1_ = square_1_settings_;
                    speaker->settings_.square_2_ = square_2_settings_;
                    speaker->settings_.noise_ = noise_settings_;
                    speaker->settings_.wave_ = wave_settings_;

                    speaker->effect_flags() = effect_flags_;
                }
            }
        }
    } else {
        if (opponent_island()) {
            if (auto room = opponent_island()->get_room(synth_pos_)) {
                if (auto s = room->cast<Synth>()) {
                    memcpy(s->notes(), notes_, sizeof notes_);
                    memcpy(s->effect_parameters(),
                           effect_parameters_,
                           sizeof effect_parameters_);

                    if (auto speaker = s->speaker()) {
                        speaker->settings_.square_1_ = square_1_settings_;
                        speaker->settings_.square_2_ = square_2_settings_;
                        speaker->settings_.noise_ = noise_settings_;
                        speaker->settings_.wave_ = wave_settings_;

                        speaker->effect_flags() = effect_flags_;
                    }
                }
            }
        }
    }

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



} // namespace skyland
