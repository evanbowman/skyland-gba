////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "adventureModeSettingsScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



struct SettingInfo
{
    SystemString text_;
    SystemString desc_;
};



static const SystemString titles[] = {
    SystemString::sf_difficulty,
    SystemString::permadeath_setting,
    SystemString::faction_setting,
};



static const SettingInfo difficulty_text[] = {
    {SystemString::sf_casual, SystemString::difficulty_hint_easy},
    {SystemString::sf_normal, SystemString::difficulty_hint_normal},
    {SystemString::sf_hard, SystemString::difficulty_hint_hard},
};



static const SettingInfo faction_text[] = {
    {SystemString::faction_human, SystemString::faction_desc_human},
    {SystemString::faction_goblin, SystemString::faction_desc_goblin},
    {SystemString::faction_sylph, SystemString::faction_desc_sylph}};



void AdventureModeSettingsScene::repaint_difficulty(int difficulty,
                                                    bool selected)
{
    auto d = difficulty_text[difficulty];
    render_line(0, d.text_, d.desc_, selected);
}



void AdventureModeSettingsScene::repaint_faction(Faction faction, bool selected)
{
    auto d = faction_text[(int)faction];
    render_line(2, d.text_, d.desc_, selected);
}



void AdventureModeSettingsScene::repaint_permadeath(bool on, bool selected)
{
    render_line(1,
                on ? SystemString::on : SystemString::off,
                on ? SystemString::permadeath_hint_on
                   : SystemString::permadeath_hint_off,
                selected);
}



void AdventureModeSettingsScene::render_line(int linenum,
                                             SystemString text,
                                             SystemString desc,
                                             bool selected)
{
    StringBuffer<96> result;
    auto title = loadstr(titles[linenum]);
    result += title->c_str();
    result += ": ";

    size_t max_title = 0;
    for (u32 i = 0; i < sizeof(titles) / sizeof(titles[0]); ++i) {
        max_title = std::max(max_title, utf8::len(loadstr(titles[i])->c_str()));
    }

    const auto title_len = utf8::len(title->c_str());
    for (size_t i = 0; i < max_title - title_len; ++i) {
        result += " ";
    }

    result += loadstr(text)->c_str();

    auto& line = lines_[linenum];
    line.set_coord(OverlayCoord{2, (u8)(4 + linenum * 2)});

    line.assign(loadstr(titles[linenum])->c_str());
    line.append(": ");

    for (size_t i = 0; i < max_title - title_len; ++i) {
        line.append(" ");
    }

    static const auto highlight_colors =
        Text::OptColors{{custom_color(0xffffff), custom_color(0x406e98)}};

    auto clr = highlight_colors;
    if (not selected) {
        clr = nullopt();
    }

    if (selected) {
        line.append("< ", clr);
    } else {
        line.append("  ");
    }

    line.append(loadstr(text)->c_str(), clr);

    if (selected) {
        line.append(" >", clr);
    }

    auto hint_colors = Text::OptColors{
        {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

    if (selected) {
        desc_->assign(loadstr(desc)->c_str(), {1, 14}, {28, 6}, 0, hint_colors);
    }
}



void AdventureModeSettingsScene::repaint()
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 10; y < 14; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    auto show_dividing_line = [&] {
        for (int x = 1; x < 29; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 13, 377);
        }
    };

    show_dividing_line();

    switch (sel_) {
    case 0:
        repaint_difficulty((int)APP.gp_.difficulty_, true);
        repaint_permadeath(
            APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on),
            false);
        repaint_faction(APP.faction(), false);
        break;

    case 1:
        repaint_difficulty((int)APP.gp_.difficulty_, false);
        repaint_permadeath(
            APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on), true);
        repaint_faction(APP.faction(), false);
        break;

    case 2: {
        repaint_difficulty((int)APP.gp_.difficulty_, false);
        repaint_permadeath(
            APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on),
            false);
        repaint_faction(APP.faction(), true);
        RoomProperties::Value filter;
        switch (APP.faction()) {
        default:
        case Faction::human:
            filter = RoomProperties::human_only;
            break;

        case Faction::goblin:
            filter = RoomProperties::goblin_only;
            break;

        case Faction::sylph:
            filter = RoomProperties::sylph_only;
            break;
        }

        Buffer<int, 5> icons;

        auto [mt, ms] = room_metatable();
        for (int i = 0; i < ms; ++i) {
            if (mt[i]->properties() & filter) {
                icons.push_back(mt[i]->unsel_icon());
            }
        }

        static const int vram_locs[] = {258, 181, 197, 213, 274};

        int x_margin = (30 - icons.size() * 4) / 2;

        for (u32 i = 0; i < icons.size(); ++i) {
            draw_image(
                vram_locs[i], x_margin + i * 4, 10, 4, 4, Layer::overlay);
            PLATFORM.load_overlay_chunk(vram_locs[i], icons[i], 16);
        }

        break;
    }
    }
}



void AdventureModeSettingsScene::enter(Scene& prev)
{
    for (u32 i = 0; i < lines_.size() + 1; ++i) {
        lines_.emplace_back("", OverlayCoord{1, (u8)(3 + i * 2)});
    }

    auto setup_str = SYSTR(setup);
    title_.emplace(
        setup_str->c_str(),
        OverlayCoord{(u8)centered_text_margins(
                         utf8::len(setup_str->c_str()) + 1 +
                         utf8::len(SYSTR(setup_instructions)->c_str())),
                     1});

    title_->append(" ");
    title_->append(SYSTR(setup_instructions)->c_str(),
                   Text::OptColors{{ColorConstant::med_blue_gray,
                                    ColorConstant::rich_black}});


    if (APP.gp_.stateflags_.get(GlobalPersistentData::goblin_faction)) {
        APP.faction() = Faction::goblin;
    }

    if (APP.gp_.stateflags_.get(GlobalPersistentData::sylph_faction)) {
        APP.faction() = Faction::sylph;
    }

    prev_faction_ = APP.faction();


    repaint();

    for (int x = 1; x < 29; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 13, 377);
    }

    PLATFORM.screen().fade(0.96f);
    PLATFORM.screen().fade(1.f);

    original_ = (u8)APP.gp_.difficulty_;
    stateflags_cached_ = APP.gp_.stateflags_;

    desc_.emplace();
}



void AdventureModeSettingsScene::exit(Scene& prev)
{
    desc_.reset();
    title_.reset();
    lines_.clear();
    PLATFORM.fill_overlay(0);
}



SHARED_VARIABLE(enabled_factions_bitfield);



void AdventureModeSettingsScene::update_field(bool inc)
{
    switch (sel_) {
    case 0: {
        auto& diff = APP.gp_.difficulty_;

        if (not inc and (int) diff == 0) {
            diff = GlobalPersistentData::Difficulty::expert;
            break;
        }
        diff = (GlobalPersistentData::Difficulty)(inc ? (int)diff + 1
                                                      : (int)diff - 1);
        if ((int)diff > (int)GlobalPersistentData::Difficulty::expert) {
            diff = GlobalPersistentData::Difficulty::beginner;
        }
        break;
    }

    case 1: {
        bool pd = APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on);
        pd = not pd;
        APP.gp_.stateflags_.set(GlobalPersistentData::permadeath_on, pd);
        break;
    }

    case 2: {
        u32 index = 0;
        Buffer<Faction, (int)Faction::count> factions;
        for (int i = 0; i < (int)Faction::count; ++i) {
            if (enabled_factions_bitfield & (1 << i)) {
                factions.push_back((Faction)i);
                if ((Faction)i == APP.faction()) {
                    index = i;
                }
            }
        }
        if (inc) {
            if (index == factions.size() - 1) {
                index = 0;
            } else {
                ++index;
            }
        } else {
            if (index == 0) {
                index = factions.size() - 1;
            } else {
                --index;
            }
        }
        APP.faction() = (Faction)index;
        break;
    }
    }
}



void load_difficulty_profile();



ScenePtr AdventureModeSettingsScene::update(Time delta)
{
    if (init_) {
        init_ = false;
    }

    APP.player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    static const int sel_max = 2;

    if (test_key(Key::up)) {
        if (sel_ > 0) {
            --sel_;
        } else {
            sel_ = sel_max;
        }
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::down)) {
        if (sel_ < sel_max) {
            ++sel_;
        } else {
            sel_ = 0;
        }
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::left)) {
        update_field(false);
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::right)) {
        update_field(true);
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }


    if (APP.player().key_down(Key::action_1)) {
        PLATFORM.speaker().play_sound("button_wooden", 3);
        load_difficulty_profile();

        if (APP.faction() not_eq prev_faction_) {
            APP.gp_.stateflags_.set(GlobalPersistentData::goblin_faction,
                                    false);
            APP.gp_.stateflags_.set(GlobalPersistentData::sylph_faction, false);

            switch (APP.faction()) {
            default:
                break;

            case Faction::goblin:
                APP.gp_.stateflags_.set(GlobalPersistentData::goblin_faction,
                                        true);
                break;

            case Faction::sylph:
                APP.gp_.stateflags_.set(GlobalPersistentData::sylph_faction,
                                        true);
                break;
            }
        }


        if ((u8)APP.gp_.difficulty_ not_eq original_ or
            APP.gp_.stateflags_ not_eq stateflags_cached_) {
            save::store_global_data(APP.gp_);
        }

        if (newgame_) {
            APP.invoke_script("/scripts/newgame.lisp");
            if (APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on)) {
                APP.persistent_data().set_flag(PersistentData::permadeath_on);
            } else {
                APP.persistent_data().clear_flag(PersistentData::permadeath_on);
            }
        }

        return make_scene<ZoneImageScene>();
    }



    return null_scene();
}



} // namespace skyland
