#include "composeSynthScene.hpp"
#include "skyland/island.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/measure.hpp"



void print_char(Platform& pfrm,
                utf8::Codepoint c,
                const OverlayCoord& coord,
                const std::optional<FontColors>& colors = {});



namespace skyland {



ComposeSynthScene::ComposeSynthScene(App& app, Synth& synth) :
    synth_pos_(synth.position()),
    synth_near_(synth.parent() == &player_island(app)),
    channel_(synth.channel())
{
    memcpy(notes_, synth.notes(), sizeof notes_);
    memcpy(effect_parameters_,
           synth.effect_parameters(),
           sizeof effect_parameters_);

    if (auto measure = synth.measure()) {
        square_1_settings_ = measure->square_1_settings_;
        square_2_settings_ = measure->square_2_settings_;
        noise_settings_ = measure->noise_settings_;
        wave_settings_ = measure->wave_settings_;
    }
}



ScenePtr<Scene> ComposeSynthScene::update(Platform& pfrm,
                                          App& app,
                                          Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (player(app).key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    if (not player(app).key_pressed(pfrm, Key::action_1)) {
        if (test_key(Key::down)) {
            if (cursor_.y < 15) {
                ++cursor_.y;
                repaint(pfrm);
            }
        }

        if (test_key(Key::up)) {
            if (cursor_.y > 0) {
                --cursor_.y;
                repaint(pfrm);
            }
        }
    } else if (cursor_.x == 0) {

        if (player(app).key_down(pfrm, Key::down)) {

            notes_[cursor_.y].note_ =
                (Platform::Speaker::Note)(((u8)notes_[cursor_.y].note_ + 1));

            if ((u8)notes_[cursor_.y].note_ > (int)(Platform::Speaker::Note::invalid)) {
                notes_[cursor_.y].note_ = (Platform::Speaker::Note)0;
            }

            demo_note(pfrm);

            repaint(pfrm);
        }

        if (player(app).key_down(pfrm, Key::up)) {

            if (notes_[cursor_.y].note_ == (Platform::Speaker::Note)0) {
                notes_[cursor_.y].note_ = Platform::Speaker::Note::invalid;
            } else {
                notes_[cursor_.y].note_ =
                    (Platform::Speaker::Note)(((u8)notes_[cursor_.y].note_ - 1));
            }

            demo_note(pfrm);

            repaint(pfrm);
        }

    } else if (cursor_.x == 1) {

        if (notes_[cursor_.y].note_ not_eq Platform::Speaker::Note::invalid) {
            if (test_key(Key::down)) {

                notes_[cursor_.y].octave_ += 1;
                if (notes_[cursor_.y].octave_ > 6) {
                    notes_[cursor_.y].octave_ = 0;
                }

                demo_note(pfrm);

                repaint(pfrm);
            }

            if (test_key(Key::up)) {
                if (notes_[cursor_.y].octave_ == 0) {
                    notes_[cursor_.y].octave_ = 6;
                } else {
                    notes_[cursor_.y].octave_ -= 1;
                }

                demo_note(pfrm);

                repaint(pfrm);
            }
        }

    } else if (cursor_.x == 2) {
        // TODO
    } else if (cursor_.x == 3) {
        // TODO
    } else {
        switch (channel_) {
        case Platform::Speaker::Channel::square_1:
        case Platform::Speaker::Channel::square_2:
        case Platform::Speaker::Channel::noise:
            break;


        default:
        case Platform::Speaker::Channel::wave:
            // TODO...
            break;

        }
    }

    if (player(app).key_down(pfrm, Key::right) and cursor_.x < 4) {
        ++cursor_.x;
        if (cursor_.x == 4) {
            cursor_.y = 0;
        }
        repaint(pfrm);
    }

    if (player(app).key_down(pfrm, Key::left) and cursor_.x > 0) {
        if (cursor_.x == 4) {
            cursor_.y = 0;
        }
        --cursor_.x;
        repaint(pfrm);
    }


    return null_scene();
}



void ComposeSynthScene::demo_note(Platform& pfrm)
{
    pfrm.speaker().init_chiptune_square_1(square_1_settings_);
    pfrm.speaker().init_chiptune_square_2(square_2_settings_);
    pfrm.speaker().init_chiptune_noise(noise_settings_);

    pfrm.speaker().play_chiptune_note(channel_,
                                      notes_[cursor_.y].note_,
                                      notes_[cursor_.y].octave_);
}



void ComposeSynthScene::repaint(Platform& pfrm)
{
    const auto st = calc_screen_tiles(pfrm);
    int start_x = st.x / 2 - 12;
    int start_y = (st.y - 16) / 2;


    auto put_char = [&](char c,
                        int x,
                        int y,
                        const std::optional<FontColors>& colors = {}) {
        auto clr = colors;

        if (not colors) {
            clr = Text::OptColors{{
                    ColorConstant::steel_blue,
                    ColorConstant::silver_white}};
        }

        print_char(pfrm,
                   c,
                   {u8(start_x + x),
                    u8(start_y + y)},
                   clr);
    };

    auto put_str = [&](const char* str,
                       int x,
                       int y,
                       const std::optional<FontColors>& colors = {}) {
        auto clr = colors;

        if (not colors) {
            clr = Text::OptColors{{
                    ColorConstant::steel_blue,
                    ColorConstant::silver_white}};
        }

        while (*str not_eq '\0') {

            print_char(pfrm,
                       *str,
                       {u8(start_x + x),
                        u8(start_y + y)},
                       clr);

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
            switch (note.note_) {
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

            case Platform::Speaker::Note::BES:
                return "B#";

            case Platform::Speaker::Note::B:
                return "B ";

            case Platform::Speaker::Note::invalid:
                return "--";
            }

            return "C ";
        }();

        StringBuffer<4> oct;

        if (note.note_ not_eq Platform::Speaker::Note::invalid) {
            oct += stringify(note.octave_);
        } else {
            oct = "-";
        }

        auto highlight = Text::OptColors{{
                ColorConstant::silver_white,
                ColorConstant::aerospace_orange}};

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

        if (cursor_.y == y and cursor_.x == 2) {
            put_char('-', 6, y, highlight);
        } else {
            put_char('-', 6, y);
        }

        if (cursor_.y == y and cursor_.x == 3) {
            put_char('0', 7, y, highlight);
            put_char('0', 8, y, highlight);
        } else {
            put_char('0', 7, y);
            put_char('0', 8, y);
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
            put_str("duty", 10, 3);
            put_str("length", 10, 5);
            put_str("tuning", 10, 7);
            break;


        default:
        case Platform::Speaker::Channel::wave:
            // TODO...
            break;

        }
    }

    init_ = false;

}



void ComposeSynthScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    repaint(pfrm);

    auto island = synth_near_ ? &player_island(app) : opponent_island(app);

    for (auto& room : island->rooms()) {
        // Stop any currently-playing chiptunes.
        if (auto b = dynamic_cast<Measure*>(room.get())) {
            b->reset(pfrm);
        }
    }



    pfrm.screen().schedule_fade(0.5f);

    const auto st = calc_screen_tiles(pfrm);
    int start_x = st.x / 2 - 13;
    int start_y = (st.y - 16) / 2 - 1;

    heading_.emplace(pfrm, OverlayCoord{(u8)start_x, (u8)start_y});

    heading_->assign([&]() {
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
        Text::OptColors{{
                    ColorConstant::steel_blue,
                        ColorConstant::silver_white}});


    pfrm.speaker().set_music_volume(6);
}



void ComposeSynthScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.fill_overlay(0);

    pfrm.screen().schedule_fade(0.f);

    if (synth_near_) {
        if (auto room = player_island(app).get_room(synth_pos_)) {
            if (auto s = dynamic_cast<Synth*>(room)) {
                memcpy(s->notes(), notes_, sizeof notes_);
                memcpy(s->effect_parameters(),
                       effect_parameters_,
                       sizeof effect_parameters_);

                if (auto measure = s->measure()) {
                    measure->square_1_settings_ = square_1_settings_;
                    measure->square_2_settings_ = square_2_settings_;
                    measure->noise_settings_ = noise_settings_;
                    measure->wave_settings_ = wave_settings_;
                }
            }
        }
    } else {
        if (opponent_island(app)) {
            if (auto room = opponent_island(app)->get_room(synth_pos_)) {
                if (auto s = dynamic_cast<Synth*>(room)) {
                    memcpy(s->notes(), notes_, sizeof notes_);
                    memcpy(s->effect_parameters(),
                           effect_parameters_,
                           sizeof effect_parameters_);

                    if (auto measure = s->measure()) {
                        measure->square_1_settings_ = square_1_settings_;
                        measure->square_2_settings_ = square_2_settings_;
                        measure->noise_settings_ = noise_settings_;
                        measure->wave_settings_ = wave_settings_;
                    }
                }
            }
        }
    }

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



}
