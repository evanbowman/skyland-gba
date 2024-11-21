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
#include "skyland/skyland.hpp"



namespace skyland
{



CrewStatsScene::CrewStatsScene(CharacterId selected_chr)
{
    auto collect_chrs = [this](auto& isle) {
        for (auto& room : isle.rooms()) {
            for (auto& chr : room->characters()) {
                chrs_.push_back(chr->id());
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
        chr_icon = 19;
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

    const auto bold_colors = FontColors{custom_color(0xf7f7ef), custom_color(0x0e0984)};

    Text::print(SYS_CSTR(crewmember_stats_title), {2, 1}, bold_colors);

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

    const auto stat_colors = FontColors{custom_color(0x495ac5), custom_color(0xe1e6da)};

    auto st = info.first->stats();

    PLATFORM.set_tile(Layer::overlay, 3, 7, 139);
    StringBuffer<96> temp = stringify(st.battles_fought_);
    temp += " ";
    temp += SYS_CSTR(crewmember_stats_battles);
    Text::print(temp.c_str(), {5, 7}, stat_colors);

    PLATFORM.set_tile(Layer::overlay, 3, 8, 144);
    PLATFORM.set_tile(Layer::overlay, 3, 9, 143);
    PLATFORM.set_tile(Layer::overlay, 3, 10, 145);
    temp = stringify(st.enemies_vanquished_);
    temp += " ";
    temp += SYS_CSTR(crewmember_stats_vanquished);
    Text::print(temp.c_str(), {5, 9}, stat_colors);

    PLATFORM.set_tile(Layer::overlay, 3, 11, 147);
    PLATFORM.set_tile(Layer::overlay, 4, 11, 148);
    temp = stringify(st.blocks_repaired_.get());
    temp += " ";
    temp += SYS_CSTR(crewmember_stats_repaired);
    Text::print(temp.c_str(), {5, 11}, stat_colors);

    PLATFORM.set_tile(Layer::overlay, 3, 13, 154);
    temp = stringify(st.fires_extinguished_);
    temp += " ";
    temp += SYS_CSTR(crewmember_stats_fires);
    Text::print(temp.c_str(), {5, 13}, stat_colors);

    PLATFORM.set_tile(Layer::overlay, 3, 15, 153);
    temp = stringify(st.steps_taken_.get());
    temp += " ";
    temp += SYS_CSTR(crewmember_stats_steps);
    Text::print(temp.c_str(), {5, 15}, stat_colors);

    temp = "id:";
    temp += stringify(info.first->id());
    Text::print(temp.c_str(), {(u8)(28 - temp.length()), 18}, bold_colors);

    PLATFORM.set_tile(Layer::overlay, 2, 18, 155);

    draw_image(156, 20, 3, 5, 3, Layer::overlay);
}



void CrewStatsScene::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_adventurelog");
    show_page();
    PLATFORM.screen().schedule_fade(0.5f);
}



void CrewStatsScene::exit(Scene& next)
{
    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.screen().schedule_fade(0);
}



ScenePtr CrewStatsScene::update(Time delta)
{
    APP.player().update(delta);

    if (APP.player().key_down(Key::action_2)) {
        exit_ = true;
        PLATFORM.fill_overlay(0);
        return null_scene();
    }

    if (APP.player().key_down(Key::right)) {
        if (page_index_ < (int)chrs_.size() - 1) {
            ++page_index_;
            show_page();
        }
    }

    if (APP.player().key_down(Key::left)) {
        if (page_index_ > 0) {
            --page_index_;
            show_page();
        }
    }

    if (exit_ and next_) {
        return (*next_)();
    }

    return null_scene();
}



}
