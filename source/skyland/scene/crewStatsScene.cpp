////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////

#include "crewStatsScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const auto bold_colors =
    FontColors{custom_color(0xf7f7ef), custom_color(0x0e0984)};



CrewStatsScene::CrewStatsScene(CharacterId selected_chr)
{
    auto collect_chrs = [this](auto& isle) {
        for (auto& room : isle.rooms()) {
            for (auto& chr : room->characters()) {
                if (chr->owner() == &APP.player()) {
                    chrs_.push_back(chr->id());
                }
            }
        }
    };

    collect_chrs(APP.player_island());
    APP.with_opponent_island(collect_chrs);

    for (u32 i = 0; i < chrs_.size(); ++i) {
        if (chrs_[i] == selected_chr) {
            page_index_ = i;
        }
    }
}



void CrewStatsScene::show_page()
{
    auto chr = chrs_[page_index_];

    auto info = BasicCharacter::find_by_id(chr);
    if (not info.first) {
        return;
    }

    u8 chr_icon = info.first->get_icon();
    if (not chr_icon) {
        chr_icon = 31;
    }

    for (int x = 2; x < 28; ++x) {
        for (int y = 1; y < 19; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 90);
        }
    }

    PLATFORM.set_tile(Layer::overlay, 2, 5, 141);
    for (int x = 7; x < 26; ++x) {
        // Squiggly band effect
        PLATFORM.set_tile(Layer::overlay, x, 5, 142);
    }

    const int offset = (chr_icon - 1) * 16;
    PLATFORM.load_overlay_chunk(274, offset, 16, "character_art");

    Text::print(loadstr(title_)->c_str(), {2, 1});

    int tile = 274;
    for (int y = 0; y < 4; ++y) {
        for (int x = 0; x < 4; ++x) {
            PLATFORM.set_tile(Layer::overlay, x + 3, y + 2, tile++, 10);
        }
    }

    PLATFORM.set_tile(Layer::overlay, 8, 3, 140);
    StringBuffer<32> health_text;
    health_text += stringify(info.first->health() / 10);
    health_text += "/";
    health_text += stringify(info.first->get_max_health() / 10);
    Text::print(health_text.c_str(), {9, 3});

    for (int x = 3; x < 27; ++x) {
        for (int y = 7; y < 18; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 138);
        }
    }

    for (int y = 7; y < 17; ++y) {
        PLATFORM.set_tile(Layer::overlay, 2, y, 151);
        PLATFORM.set_tile(Layer::overlay, 27, y, 152);
    }

    for (int x = 3; x < 27; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 6, 149);
        PLATFORM.set_tile(Layer::overlay, x, 17, 150);
    }

    const auto stat_colors =
        FontColors{custom_color(0x495ac5), custom_color(0xe1e6da)};

    auto st = info.first->stats();

    for (int y = 0; y < 4; ++y) {
        PLATFORM.set_tile(Layer::overlay, 27, 1 + y, 181 + y);
    }

    auto separator = [&](int y) {
        for (int x = 3; x < 25; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, y, 180);
        }
    };

    StringBuffer<96> temp;

    auto append_stat = [&](int val) {
        auto tl = integer_text_length(val);
        while (utf8::len(temp.c_str()) + tl < 20) {
            temp.push_back(' ');
        }
        temp += stringify(val);
    };

    PLATFORM.set_tile(Layer::overlay, 25, 7, 139);
    temp = SYS_CSTR(crewmember_stats_battles);
    append_stat(st.battles_fought_);
    Text::print(temp.c_str(), {4, 7}, stat_colors);

    separator(8);

    PLATFORM.set_tile(Layer::overlay, 25, 8, 144);
    PLATFORM.set_tile(Layer::overlay, 25, 9, 143);
    PLATFORM.set_tile(Layer::overlay, 25, 10, 145);
    temp = SYS_CSTR(crewmember_stats_vanquished);
    append_stat(st.enemies_vanquished_);
    Text::print(temp.c_str(), {4, 9}, stat_colors);

    separator(10);

    PLATFORM.set_tile(Layer::overlay, 25, 11, 147);
    PLATFORM.set_tile(Layer::overlay, 26, 11, 148);
    temp = SYS_CSTR(crewmember_stats_repaired);
    append_stat(st.blocks_repaired_.get());
    Text::print(temp.c_str(), {4, 11}, stat_colors);

    separator(12);

    PLATFORM.set_tile(Layer::overlay, 25, 13, 154);
    temp = SYS_CSTR(crewmember_stats_fires);
    append_stat(st.fires_extinguished_);
    Text::print(temp.c_str(), {4, 13}, stat_colors);

    separator(14);

    PLATFORM.set_tile(Layer::overlay, 25, 15, 153);
    temp = SYS_CSTR(crewmember_stats_steps);
    append_stat(st.steps_taken_.get());
    Text::print(temp.c_str(), {4, 15}, stat_colors);

    separator(16);

    temp = "id:";
    temp += stringify(info.first->id());
    Text::print(temp.c_str(), {(u8)(28 - temp.length()), 18});

    PLATFORM.set_tile(Layer::overlay, 2, 18, 155);

    draw_image(156, 20, 2, 5, 4, Layer::overlay);

    if (page_index_ > 0) {
        PLATFORM.set_tile(Layer::overlay, 0, 9, 178);
    } else {
        PLATFORM.set_tile(Layer::overlay, 0, 9, 0);
    }

    if (page_index_ < (int)chrs_.size() - 1) {
        PLATFORM.set_tile(Layer::overlay, 29, 9, 179);
    } else {
        PLATFORM.set_tile(Layer::overlay, 29, 9, 0);
    }
}



void CrewStatsScene::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_adventurelog");
    show_page();
    PLATFORM.screen().schedule_fade(bkg_fade_amount_);
}



void CrewStatsScene::exit(Scene& next)
{
    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.screen().schedule_fade(exit_fade_);
}



static constexpr u8 icons[] = {5,  1,  6,  7,  8,  9,  10, 11, 12, 14, 15,
                               16, 17, 18, 20, 21, 22, 23, 19, 26, 27, 28};



ScenePtr CrewStatsScene::update(Time delta)
{
    APP.player().update(delta);

    auto show_icon = [this]() {
        int offset = (icons[icon_sel_] - 1) * 16;
        PLATFORM.load_overlay_chunk(274, offset, 16, "character_art");
    };

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    switch (state_) {
    case State::regular: {
        if (APP.player().key_down(Key::action_2) or
            APP.player().key_down(Key::action_1)) {
            exit_ = true;
            PLATFORM.fill_overlay(0);
            return null_scene();
        }

        if (APP.player().key_down(Key::right)) {
            if (page_index_ < (int)chrs_.size() - 1) {
                ++page_index_;
                show_page();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (APP.player().key_down(Key::left)) {
            if (page_index_ > 0) {
                --page_index_;
                show_page();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (APP.player().key_down(Key::start)) {
            state_ = State::icon_select;
            PLATFORM.set_tile(Layer::overlay, 2, 4, 176);
            PLATFORM.set_tile(Layer::overlay, 2, 5, 177);
            PLATFORM.speaker().play_sound("button_wooden", 3);
            Text::print("choose icon", {2, 6}, bold_colors);

            auto found = BasicCharacter::find_by_id(chrs_[page_index_]);
            if (found.first) {
                auto icon = found.first->get_icon();
                for (u32 i = 0; i < (sizeof icons); ++i) {
                    if (icon == icons[i]) {
                        icon_sel_ = i;
                        break;
                    }
                }
            }
            show_icon();
            break;
        }

        if (exit_) {
            if (next_) {
                return (*next_)();
            }
            auto found = BasicCharacter::find_by_id(chrs_[page_index_]);
            if (found.first) {
                if (found.second->parent() == APP.opponent_island()) {
                    globals().far_cursor_loc_ = found.first->grid_position();
                    return make_scene<InspectP2Scene>();
                } else {
                    globals().near_cursor_loc_ = found.first->grid_position();
                    return make_scene<ReadyScene>();
                }
            } else {
                return make_scene<ReadyScene>();
            }
        }
        break;
    }

    case State::icon_select: {
        if (APP.player().key_down(Key::action_2)) {
            state_ = State::regular;
            show_page();
        }

        if (APP.player().key_down(Key::action_1)) {
            if (icons[icon_sel_] == 19) {
                state_ = State::regular;
                show_page();
                break;
            }
            PLATFORM.speaker().play_sound("button_wooden", 3);
            auto found = BasicCharacter::find_by_id(chrs_[page_index_]);
            if (found.first) {
                found.first->set_icon(icons[icon_sel_]);
            }
            state_ = State::regular;
            show_page();
        }

        if (test_key(Key::down)) {
            if (icon_sel_ < (int)(sizeof icons) - 1) {
                ++icon_sel_;
            } else {
                icon_sel_ = 0;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_icon();
        }

        if (test_key(Key::up)) {
            if (icon_sel_ == 0) {
                icon_sel_ = (int)(sizeof icons) - 1;
            } else {
                --icon_sel_;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_icon();
        }

        break;
    }
    }

    return null_scene();
}



} // namespace skyland
