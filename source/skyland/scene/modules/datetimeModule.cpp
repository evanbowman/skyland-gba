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


#include "datetimeModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



static int day_number(int day, int month, int year)
{
    static int t[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
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



static StringBuffer<48> month_name(Platform& pfrm, int month)
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



void DatetimeModule::repaint(Platform& pfrm)
{
    for (int y = 0; y < 20; ++y) {
        for (int x = 0; x < 30; ++x) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    auto mstr = month_name(pfrm, dt_.date_.month_ - 1);
    auto year_str_len = integer_text_length(2000 + dt_.date_.year_);
    u8 margin =
        centered_text_margins(pfrm, utf8::len(mstr.c_str()) + 1 + year_str_len);

    auto highlight = Text::OptColors{
        {ColorConstant::rich_black, ColorConstant::spanish_crimson}};

    auto noclr = std::nullopt;

    Text heading(pfrm, OverlayCoord{margin, 2});
    heading.append(mstr.c_str(),
                   state_ == State::set_month ? highlight : noclr);
    heading.append(" ");
    heading.append(2000 + dt_.date_.year_,
                   state_ == State::set_year ? highlight : noclr);
    heading.__detach();

    int hms_y = 12;
    auto draw_couplet = [&](int x, int val) {
        if (val < 10) {
            draw_image(pfrm, 118 + 0, x, hms_y, 4, 6, Layer::overlay);
            draw_image(
                pfrm, 118 + val * 24, x + 4, hms_y, 4, 6, Layer::overlay);
        } else {
            int tens = val / 10;
            int ones = val % 10;
            draw_image(pfrm, 118 + tens * 24, x, hms_y, 4, 6, Layer::overlay);
            draw_image(
                pfrm, 118 + ones * 24, x + 4, hms_y, 4, 6, Layer::overlay);
        }
    };

    draw_image(pfrm, 94, 8, hms_y, 4, 6, Layer::overlay);
    draw_image(pfrm, 94, 18, hms_y, 4, 6, Layer::overlay);

    draw_couplet(1, dt_.hour_);
    draw_couplet(11, dt_.minute_);
    draw_couplet(21, dt_.second_);

    if (state_ == State::set_hour) {
        for (int i = 0; i < 8; ++i) {
            pfrm.set_tile(Layer::overlay, 1 + i, hms_y + 6, 358);
        }
    } else if (state_ == State::set_min) {
        for (int i = 0; i < 8; ++i) {
            pfrm.set_tile(Layer::overlay, 11 + i, hms_y + 6, 358);
        }
    } else if (state_ == State::set_sec) {
        for (int i = 0; i < 8; ++i) {
            pfrm.set_tile(Layer::overlay, 21 + i, hms_y + 6, 358);
        }
    }

    const int day_index = dt_.date_.day_;
    const int month_index = dt_.date_.month_ - 1;
    const int year = 2000 + dt_.date_.year_;

    u8 row = 5;

    int k = 0;
    std::optional<Text> t_;
    t_.emplace(pfrm, OverlayCoord{1, row});


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
                    t_.emplace(pfrm, OverlayCoord{1, row});
                }
            }
        }

        current = k;
    }

    if (t_) {
        t_->__detach();
    }
}



void DatetimeModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (auto tm = pfrm.system_clock().now()) {
        dt_ = *tm;
    } else {
        dt_.date_.year_ = 1;
        dt_.date_.month_ = 1;
        dt_.date_.day_ = 1;
    }

    pfrm.screen().schedule_fade(0.95f);
    pfrm.screen().schedule_fade(1.f);

    pfrm.load_overlay_texture("overlay_large_numeral");
    pfrm.enable_glyph_mode(true);

    repaint(pfrm);

    Text::platform_retain_alphabet(pfrm);
}



void DatetimeModule::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
DatetimeModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto& p = player(app);
    p.update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    switch (state_) {
    case State::set_month:
        if (test_key(Key::up)) {
            if (dt_.date_.month_ == 1) {
                dt_.date_.month_ = 12;
            } else {
                dt_.date_.month_--;
            }
            pfrm.speaker().play_sound("click", 1);
            repaint(pfrm);
        } else if (test_key(Key::down)) {
            if (dt_.date_.month_ == 12) {
                dt_.date_.month_ = 1;
            } else {
                dt_.date_.month_++;
            }
            pfrm.speaker().play_sound("click", 1);
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            state_ = State::set_year;
            repaint(pfrm);
        } else if (key_down<Key::action_2>(pfrm)) {
            if (not next_scene_) {
                return scene_pool::alloc<TitleScreenScene>(3);
            }
        }
        break;

    case State::set_year:
        if (test_key(Key::up)) {
            if (dt_.date_.year_ > 1) {
                dt_.date_.year_--;
                pfrm.speaker().play_sound("click", 1);
                repaint(pfrm);
            }
        } else if (test_key(Key::down)) {
            dt_.date_.year_++;
            pfrm.speaker().play_sound("click", 1);
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            state_ = State::set_day;
            repaint(pfrm);
        } else if (key_down<Key::action_2>(pfrm)) {
            state_ = State::set_month;
            repaint(pfrm);
        }
        break;

    case State::set_day:
        if (test_key(Key::up)) {
            if (dt_.date_.day_ > 1) {
                dt_.date_.day_--;
                pfrm.speaker().play_sound("click", 1);
                repaint(pfrm);
            }
        } else if (test_key(Key::down)) {
            if (dt_.date_.day_ <
                days_per_month(dt_.date_.month_ - 1, 2000 + dt_.date_.year_)) {
                dt_.date_.day_++;
                pfrm.speaker().play_sound("click", 1);
            }
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            state_ = State::set_hour;
            repaint(pfrm);
        } else if (key_down<Key::action_2>(pfrm)) {
            state_ = State::set_year;
            repaint(pfrm);
        }
        break;

    case State::set_hour:
        if (test_key(Key::up)) {
            if (dt_.hour_ > 0) {
                dt_.hour_--;
                pfrm.speaker().play_sound("click", 1);
                repaint(pfrm);
            }
        } else if (test_key(Key::down)) {
            if (dt_.hour_ < 23) {
                dt_.hour_++;
                pfrm.speaker().play_sound("click", 1);
            }
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            state_ = State::set_min;
            repaint(pfrm);
        } else if (key_down<Key::action_2>(pfrm)) {
            state_ = State::set_day;
            repaint(pfrm);
        }
        break;

    case State::set_min:
        if (test_key(Key::up)) {
            if (dt_.minute_ > 0) {
                dt_.minute_--;
                pfrm.speaker().play_sound("click", 1);
                repaint(pfrm);
            }
        } else if (test_key(Key::down)) {
            if (dt_.minute_ < 59) {
                dt_.minute_++;
                pfrm.speaker().play_sound("click", 1);
            }
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            state_ = State::set_sec;
            repaint(pfrm);
        } else if (key_down<Key::action_2>(pfrm)) {
            state_ = State::set_hour;
            repaint(pfrm);
        }
        break;

    case State::set_sec:
        if (test_key(Key::up)) {
            if (dt_.second_ > 0) {
                dt_.second_--;
                repaint(pfrm);
                pfrm.speaker().play_sound("click", 1);
            }
        } else if (test_key(Key::down)) {
            if (dt_.second_ < 59) {
                dt_.second_++;
                pfrm.speaker().play_sound("click", 1);
            }
            repaint(pfrm);
        } else if (key_down<Key::action_1>(pfrm)) {
            pfrm.system_clock().configure(dt_);

            // Datetime cache created by macrocosm saves. Erase, or people can
            // cheat.
            flash_filesystem::unlink_file(pfrm, "/save/mt.dat");
            if (not next_scene_) {
                return scene_pool::alloc<TitleScreenScene>(3);
            } else {
                return (*next_scene_)();
            }
        } else if (key_down<Key::action_2>(pfrm)) {
            state_ = State::set_min;
            repaint(pfrm);
        }
        break;

    default:
        break;
    }


    return null_scene();
}



DatetimeModule::Factory DatetimeModule::factory_(true);



} // namespace skyland
