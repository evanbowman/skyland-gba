////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "datetimeModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



static int day_number(int day, int month, int year)
{
    constexpr int t[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
    year -= month < 3;
    return (year + year / 4 - year / 100 + year / 400 + t[month - 1] + day) % 7;
}



static int days_per_month(int month, int year)
{
    switch (month) {
    default:
    case 0:
    case 2:
    case 4:
    case 6:
    case 7:
    case 9:
    case 11:
        return 31;
    case 3:
    case 5:
    case 8:
    case 10:
        return 30;
    case 1:
        if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0))
            return 29;
        else {
            return 28;
        }
    }
}



static StringBuffer<48> month_name(int month)
{
    auto mstr = SYSTR(months);
    auto cp = mstr->c_str();

    while (month > 0) {
        if (*cp == ',') {
            --month;
        }
        ++cp;
    }

    StringBuffer<48> result;

    while (*cp not_eq ',' and *cp not_eq '\0') {
        result.push_back(*(cp++));
    }

    return result;
}



void DatetimeModule::repaint()
{
    for (int y = 0; y < 20; ++y) {
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    auto mstr = month_name(dt_.date_.month_ - 1);
    auto year_str_len = integer_text_length(2000 + dt_.date_.year_);
    u8 margin =
        centered_text_margins(utf8::len(mstr.c_str()) + 1 + year_str_len);

    auto highlight = Text::OptColors{
        {ColorConstant::rich_black, ColorConstant::spanish_crimson}};

    auto noclr = std::nullopt;

    Text heading(OverlayCoord{margin, 2});
    heading.append(mstr.c_str(),
                   state_ == State::set_month ? highlight : noclr);
    heading.append(" ");
    heading.append(2000 + dt_.date_.year_,
                   state_ == State::set_year ? highlight : noclr);
    heading.__detach();

    int hms_y = 12;
    auto draw_couplet = [&](int x, int val) {
        if (val < 10) {
            draw_image(118 + 0, x, hms_y, 4, 6, Layer::overlay);
            draw_image(118 + val * 24, x + 4, hms_y, 4, 6, Layer::overlay);
        } else {
            int tens = val / 10;
            int ones = val % 10;
            draw_image(118 + tens * 24, x, hms_y, 4, 6, Layer::overlay);
            draw_image(118 + ones * 24, x + 4, hms_y, 4, 6, Layer::overlay);
        }
    };

    draw_image(94, 8, hms_y, 4, 6, Layer::overlay);
    draw_image(94, 18, hms_y, 4, 6, Layer::overlay);

    draw_couplet(1, dt_.hour_);
    draw_couplet(11, dt_.minute_);
    draw_couplet(21, dt_.second_);

    if (state_ == State::set_hour) {
        for (int i = 0; i < 8; ++i) {
            PLATFORM.set_tile(Layer::overlay, 1 + i, hms_y + 6, 358);
        }
    } else if (state_ == State::set_min) {
        for (int i = 0; i < 8; ++i) {
            PLATFORM.set_tile(Layer::overlay, 11 + i, hms_y + 6, 358);
        }
    } else if (state_ == State::set_sec) {
        for (int i = 0; i < 8; ++i) {
            PLATFORM.set_tile(Layer::overlay, 21 + i, hms_y + 6, 358);
        }
    }

    const int day_index = dt_.date_.day_;
    const int month_index = dt_.date_.month_ - 1;
    const int year = 2000 + dt_.date_.year_;

    u8 row = 5;

    int k = 0;
    Optional<Text> t_;
    t_.emplace(OverlayCoord{1, row});


    int current = day_number(1, 1, year);

    for (int i = 0; i < 12; i++) {
        const int days = days_per_month(i, year);

        for (k = 0; k < current; ++k) {
            if (i == month_index) {
                t_->append("    ");
            }
        }

        for (int j = 1; j <= days; ++j) {
            if (j < 10) {
                if (i == month_index) {
                    t_->append(" ");
                }
            }
            if (i == month_index) {
                if (state_ == State::set_day) {
                    t_->append(j, j == day_index ? highlight : noclr);
                } else {
                    t_->append(
                        j,
                        j == day_index
                            ? Text::OptColors{{ColorConstant::silver_white,
                                               custom_color(0x595880)}}
                            : noclr);
                }

                t_->append("  ");
            }
            if (++k > 6) {
                k = 0;
                if (i == month_index) {
                    ++row;
                    t_->__detach();
                    t_.emplace(OverlayCoord{1, row});
                }
            }
        }

        current = k;
    }

    if (t_) {
        t_->__detach();
    }
}



void DatetimeModule::enter(Scene& prev)
{
    info("enter datetime module");

    if (auto tm = PLATFORM.system_clock().now()) {
        dt_ = *tm;
    } else {
        dt_.date_.year_ = 1;
        dt_.date_.month_ = 1;
        dt_.date_.day_ = 1;
    }

    PLATFORM.screen().schedule_fade(0.95f);
    PLATFORM.screen().schedule_fade(1.f);

    PLATFORM.load_overlay_texture("overlay_large_numeral");
    PLATFORM.enable_glyph_mode(true);

    repaint();
}



void DatetimeModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);
}



ScenePtr DatetimeModule::update(Time delta)
{
    auto& p = player();
    p.update(delta);

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    switch (state_) {
    case State::set_month:
        if (test_key(Key::up)) {
            if (dt_.date_.month_ == 1) {
                dt_.date_.month_ = 12;
            } else {
                dt_.date_.month_--;
            }
            PLATFORM.speaker().play_sound("click", 1);
            repaint();
        } else if (test_key(Key::down)) {
            if (dt_.date_.month_ == 12) {
                dt_.date_.month_ = 1;
            } else {
                dt_.date_.month_++;
            }
            PLATFORM.speaker().play_sound("click", 1);
            repaint();
        } else if (key_down<Key::action_1>()) {
            state_ = State::set_year;
            repaint();
        } else if (key_down<Key::action_2>()) {
            if (not next_scene_) {
                return make_scene<TitleScreenScene>(3);
            }
        }
        break;

    case State::set_year:
        if (test_key(Key::up)) {
            if (dt_.date_.year_ > 1) {
                dt_.date_.year_--;
                PLATFORM.speaker().play_sound("click", 1);
                repaint();
            }
        } else if (test_key(Key::down)) {
            dt_.date_.year_++;
            PLATFORM.speaker().play_sound("click", 1);
            repaint();
        } else if (key_down<Key::action_1>()) {
            state_ = State::set_day;
            repaint();
        } else if (key_down<Key::action_2>()) {
            state_ = State::set_month;
            repaint();
        }
        break;

    case State::set_day:
        if (test_key(Key::up)) {
            if (dt_.date_.day_ > 1) {
                dt_.date_.day_--;
                PLATFORM.speaker().play_sound("click", 1);
                repaint();
            }
        } else if (test_key(Key::down)) {
            if (dt_.date_.day_ <
                days_per_month(dt_.date_.month_ - 1, 2000 + dt_.date_.year_)) {
                dt_.date_.day_++;
                PLATFORM.speaker().play_sound("click", 1);
            }
            repaint();
        } else if (key_down<Key::action_1>()) {
            state_ = State::set_hour;
            repaint();
        } else if (key_down<Key::action_2>()) {
            state_ = State::set_year;
            repaint();
        }
        break;

    case State::set_hour:
        if (test_key(Key::up)) {
            if (dt_.hour_ > 0) {
                dt_.hour_--;
                PLATFORM.speaker().play_sound("click", 1);
                repaint();
            }
        } else if (test_key(Key::down)) {
            if (dt_.hour_ < 23) {
                dt_.hour_++;
                PLATFORM.speaker().play_sound("click", 1);
            }
            repaint();
        } else if (key_down<Key::action_1>()) {
            state_ = State::set_min;
            repaint();
        } else if (key_down<Key::action_2>()) {
            state_ = State::set_day;
            repaint();
        }
        break;

    case State::set_min:
        if (test_key(Key::up)) {
            if (dt_.minute_ > 0) {
                dt_.minute_--;
                PLATFORM.speaker().play_sound("click", 1);
                repaint();
            }
        } else if (test_key(Key::down)) {
            if (dt_.minute_ < 59) {
                dt_.minute_++;
                PLATFORM.speaker().play_sound("click", 1);
            }
            repaint();
        } else if (key_down<Key::action_1>()) {
            state_ = State::set_sec;
            repaint();
        } else if (key_down<Key::action_2>()) {
            state_ = State::set_hour;
            repaint();
        }
        break;

    case State::set_sec:
        if (test_key(Key::up)) {
            if (dt_.second_ > 0) {
                dt_.second_--;
                repaint();
                PLATFORM.speaker().play_sound("click", 1);
            }
        } else if (test_key(Key::down)) {
            if (dt_.second_ < 59) {
                dt_.second_++;
                PLATFORM.speaker().play_sound("click", 1);
            }
            repaint();
        } else if (key_down<Key::action_1>()) {
            PLATFORM.system_clock().configure(dt_);

            // Datetime cache created by macrocosm saves. Erase, or people can
            // cheat.
            // flash_filesystem::unlink_file("/save/mt.dat");
            if (not next_scene_) {
                return make_scene<TitleScreenScene>(3);
            } else {
                return (*next_scene_)();
            }
        } else if (key_down<Key::action_2>()) {
            state_ = State::set_min;
            repaint();
        }
        break;

    default:
        break;
    }


    return null_scene();
}



DatetimeModule::Factory DatetimeModule::factory_(true);



} // namespace skyland
