////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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


#include "adventureLogScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void AdventureLogScene::show_page(Platform& pfrm, App&, int page_num)
{
    entries_.clear();
    for (int x = 0; x < 30; ++x) {
        for (int y = 1; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 90);
        }
    }

    auto cnt = logentry_count();

    int begin = page_num * entries_.capacity();
    int end = page_num * entries_.capacity() + entries_.capacity();

    max_pages_ = cnt / 3 + ((cnt % 3) > 0);

    if (begin > cnt) {
        return;
    }

    end = clamp(end, begin, cnt);

    Text::print(pfrm,
                "-Adventure-log-",
                {0, 0},
                FontColors{custom_color(0xf7f7ef), custom_color(0x0e0984)});

    auto pages_text = format("%/%", page_num + 1, max_pages_);

    for (int x = 20; x < 30; ++x) {
        pfrm.set_tile(Layer::overlay, x, 0, 90);
    }
    Text::print(pfrm,
                pages_text.c_str(),
                {u8(30 - utf8::len(pages_text.c_str())), 0},
                FontColors{custom_color(0xf7f7ef), custom_color(0x0e0984)});


    u8 y = 3;

    for (int i = begin; i < end; ++i) {
        auto text = format_logentry(pfrm, i);
        entries_.emplace_back(pfrm);
        int lines = entries_.back().assign(text.c_str(), {2, y}, {27, 3});

        pfrm.set_tile(Layer::overlay, 1, y, 82);

        y += 2;

        for (int x = 0; x < 30; ++x) {
            auto t = pfrm.get_tile(Layer::overlay, x, y + 2);
            if (t == 90) {
                y += 2;
                break;
            }
        }

        y += 1;

        // if (lines == 3) {
        //     y += 7;
        // } else if (lines == 2) {
        //     y += 5;
        // } else {
        //     y += 3;
        // }
    }

    if (cnt > end) {
        pfrm.set_tile(Layer::overlay, 29, 18, 84);
        pfrm.set_tile(Layer::overlay, 28, 18, 83);
        pfrm.set_tile(Layer::overlay, 28, 19, 85);
        pfrm.set_tile(Layer::overlay, 29, 19, 86);
    }

    if (page_ > 0) {
        pfrm.set_tile(Layer::overlay, 0, 18, 87);
        pfrm.set_tile(Layer::overlay, 1, 18, 83);
        pfrm.set_tile(Layer::overlay, 0, 19, 89);
        pfrm.set_tile(Layer::overlay, 1, 19, 87);
    }
}



void AdventureLogScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().set_music_volume(8);

    if (logentry_count() == 0) {
        logbook_missing_ = true;
        return;
    }

    pfrm.load_overlay_texture("overlay_adventurelog");
    show_page(pfrm, app, 0);
    pfrm.screen().schedule_fade(1, custom_color(0xcdd6a1));
    Text::platform_retain_alphabet(pfrm);
    Text::print(pfrm,
                "0123456789",
                {0, 20},
                FontColors{custom_color(0xf7f7ef), custom_color(0x0e0984)});
}



void AdventureLogScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
    entries_.clear();
    pfrm.screen().clear();
    pfrm.screen().display();
    pfrm.load_overlay_texture("overlay");
    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



DynamicMemory<FileLine>
get_line_from_file(Platform& pfrm, const char* file_name, int line);



StringBuffer<128> AdventureLogScene::format_logentry(Platform& pfrm, int entry)
{
    StringBuffer<128> result;

    if (auto v = load_logentry(entry)) {

        auto line = lisp::get_list(v, 0)->integer().value_;
        auto str = get_line_from_file(pfrm, "/strings/adventure_log.txt", line);

        Buffer<StringBuffer<20>, 8> args;
        lisp::foreach (v->cons().cdr(), [&](lisp::Value* val) {
            lisp::DefaultPrinter p;
            lisp::format(val, p);
            args.push_back(p.data_.c_str());
        });

        const char* fmt_str = str->c_str();
        u32 current_arg = 0;
        while (*fmt_str not_eq '\0') {
            if (*fmt_str == '%') {
                if (current_arg < args.size()) {
                    result += args[current_arg++];
                }
            } else {
                result.push_back(*fmt_str);
            }
            ++fmt_str;
        }
    }

    return result;
}



lisp::Value* AdventureLogScene::load_logentry(int entry)
{
    const auto num_entries = logentry_count();
    return lisp::get_list(lisp::get_var("adventure-log"),
                          (num_entries - 1) - entry);
}



int AdventureLogScene::logentry_count()
{
    return lisp::length(lisp::get_var("adventure-log"));
}



ScenePtr<Scene>
AdventureLogScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (logbook_missing_) {
        return (*next_)();
    }

    app.player().update(pfrm, app, delta);

    switch (state_) {
    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            return (*next_)();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }

    case State::ready: {
        if (next_ and (app.player().key_down(pfrm, Key::action_1) or
                       app.player().key_down(pfrm, Key::action_2))) {
            state_ = State::fade_out;
            for (int x = 0; x < 30; ++x) {
                pfrm.set_tile(Layer::overlay, x, 0, 90);
            }
            timer_ = 0;
            break;
        }

        if (app.player().key_down(pfrm, Key::right)) {

            auto cnt = logentry_count();

            int begin = page_ * entries_.capacity();
            int end = page_ * entries_.capacity() + entries_.capacity();

            end = clamp(end, begin, cnt);

            if (end < cnt) {
                entries_.clear();
                for (int x = 0; x < 30; ++x) {
                    for (int y = 1; y < 20; ++y) {
                        pfrm.set_tile(Layer::overlay, x, y, 90);
                    }
                }
                state_ = State::page_turn_right_anim;
            }
        }

        if (app.player().key_down(pfrm, Key::left) and page_ > 0) {
            entries_.clear();
            for (int x = 0; x < 30; ++x) {
                for (int y = 1; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 90);
                }
            }
            state_ = State::page_turn_left_anim;
        }
        break;
    }

    case State::page_turn_right_anim:
        state_ = State::ready;
        ++page_;
        show_page(pfrm, app, page_);
        break;
        for (int i = 0; i < 4; ++i) {
            for (int y = 0; y < 20; ++y) {
                for (int x = 0; x < 30; ++x) {
                    auto t = pfrm.get_tile(Layer::overlay, x, y);
                    switch (t) {
                    case 84:
                    case 85:
                        pfrm.set_tile(Layer::overlay, x, y, 121);
                        break;

                    case 89:
                        pfrm.set_tile(Layer::overlay, x, y, 113);
                        break;

                    case 114:
                    case 115:
                    case 116:
                    case 117:
                    case 118:
                    case 119:
                    case 120:
                    case 121:
                    case 122:
                    case 123:
                    case 124:
                    case 125:
                    case 126:
                    case 127:
                    case 128:
                        pfrm.set_tile(Layer::overlay, x, y, t + 1);

                        if (t == 122 and x > 0) {
                            auto prev = pfrm.get_tile(Layer::overlay, x - 1, y);
                            if (prev < 91) {
                                pfrm.set_tile(Layer::overlay, x - 1, y, 115);
                            }
                        }
                        if (t == 122 and y > 0) {
                            auto prev = pfrm.get_tile(Layer::overlay, x, y - 1);
                            if (prev < 91) {
                                pfrm.set_tile(Layer::overlay, x, y - 1, 115);
                            }
                        }
                        break;

                    case 129:
                        if (x > 0 and y > 0) {
                            pfrm.set_tile(Layer::overlay, x - 1, y - 1, 114);
                        }
                        pfrm.set_tile(Layer::overlay, x, y, 113);
                        if (x == 0 and y == 0) {
                            state_ = State::page_fade_in_anim;
                            pfrm.screen().schedule_fade(
                                1, custom_color(0x0e0984), true, true);
                            ++page_;
                            show_page(pfrm, app, page_);

                            timer_ = 0;
                        }
                        break;
                    }
                }
            }
        }
        break;

    case State::page_turn_left_anim:
        --page_;
        show_page(pfrm, app, page_);
        state_ = State::ready;
        break;

    case State::page_fade_in_anim:
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            pfrm.screen().schedule_fade(0);
            state_ = State::ready;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
                amount, custom_color(0x0e0984), true, true);
            pfrm.set_overlay_origin(0, -amount * 16);
        }
        break;
    }


    return null_scene();
}



} // namespace skyland
