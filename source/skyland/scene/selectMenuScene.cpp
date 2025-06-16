////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "selectMenuScene.hpp"
#include "constructionScene.hpp"
#include "crewStatsScene.hpp"
#include "droneStatsScene.hpp"
#include "groupSelectionScene.hpp"
#include "inspectP2Scene.hpp"
#include "menuPromptScene.hpp"
#include "modules/flagDesignerModule.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "moveCharacterScene.hpp"
#include "moveRoomScene.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "setGamespeedScene.hpp"
#include "skyland/network.hpp"
#include "skyland/player/player.hpp"
#include "skyland/rooms/bell.hpp"
#include "skyland/scene/adjustPowerScene.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



SHARED_VARIABLE(powerdown_allowed);



Island* SelectMenuScene::island() const
{
    if (is_far_camera()) {
        return opponent_island();
    } else {
        return &player_island();
    }
}



static const auto highlight_colors =
    FontColors{custom_color(0x000010), ColorConstant::aerospace_orange};


static const auto specific_colors =
    FontColors{custom_color(0xead873), ColorConstant::rich_black};



void SelectMenuScene::redraw_line(int line, bool highlight)
{
    auto clr = highlight ? highlight_colors
                         : (opts_->specific_.get(line) ? specific_colors
                                                       : Text::OptColors{});

    opts_->lines_[line].assign(loadstr(opts_->strings_[line])->c_str(), clr);
    opts_->lines_[line].append(opts_->suffixes_[line].c_str(), clr);

    for (int i = opts_->lines_[line].len(); i < opts_->longest_line_; ++i) {
        opts_->lines_[line].append(
            " ", highlight ? highlight_colors : Text::OptColors{});
    }
}



static ScenePtr select_menu_help(bool far)
{
    const auto flag = GlobalPersistentData::sel_menu_help_prompt_dont_remind_me;

    const bool skip_prompt = APP.gp_.stateflags_.get(flag) or
                             state_bit_load(StateBit::sel_menu_help_prompt) or
                             PLATFORM.network_peer().is_connected();

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(flag, true);
        save::store_global_data(APP.gp_);
    };

    auto next = [far] {
        auto ret = make_scene<SelectMenuScene>();
        if (far) {
            ret->far_camera();
        }
        return ret;
    };

    if (not skip_prompt) {
        state_bit_store(StateBit::sel_menu_help_prompt, true);
        return make_scene<MenuPromptScene>(
            SystemString::sel_menu_prompt,
            SystemString::ok,
            SystemString::do_not_show_again,
            next,
            []() {},
            dont_remind);
    } else {
        return null_scene();
    }
}



static ScenePtr move_blocks_setup(bool far)
{
    const auto flag =
        GlobalPersistentData::move_blocks_help_prompt_dont_remind_me;

    const bool skip_prompt =
        APP.gp_.stateflags_.get(flag) or
        state_bit_load(StateBit::move_blocks_help_prompt) or
        APP.game_mode() == App::GameMode::tutorial;

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(flag, true);
        save::store_global_data(APP.gp_);
    };

    auto next = [far] { return make_scene<MoveRoomScene>(not far); };

    if (not skip_prompt) {
        state_bit_store(StateBit::move_blocks_help_prompt, true);
        return make_scene<MenuPromptScene>(
            SystemString::move_blocks_prompt,
            SystemString::ok,
            SystemString::do_not_show_again,
            next,
            []() {},
            dont_remind);
    } else {
        return next();
    }
}



static ScenePtr set_gamespeed_setup()
{
    const auto flag =
        GlobalPersistentData::gamespeed_help_prompt_dont_remind_me;

    const bool skip_prompt = APP.gp_.stateflags_.get(flag) or
                             state_bit_load(StateBit::gamespeed_help_prompt) or
                             APP.game_mode() == App::GameMode::tutorial;

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(flag, true);
        save::store_global_data(APP.gp_);
    };

    auto next = [] {
        auto ret = make_scene<SetGamespeedScene>();
        ret->button_mode_ = 1;
        return ret;
    };

    if (not skip_prompt) {
        state_bit_store(StateBit::gamespeed_help_prompt, true);
        return make_scene<MenuPromptScene>(
            SystemString::gamespeed_prompt,
            SystemString::ok,
            SystemString::do_not_show_again,
            next,
            []() {},
            dont_remind);
    } else {
        return next();
    }
}



void SelectMenuScene::enter(Scene& scene)
{
    disable_ui();
    disable_gamespeed_icon();


    ActiveWorldScene::enter(scene);

    if (auto ws = scene.cast_world_scene()) {
        if (ws->is_far_camera()) {
            far_camera();
        }
    }

    auto cursor = is_far_camera() ? globals().far_cursor_loc_
                                  : globals().near_cursor_loc_;


    auto opp_isle = APP.opponent_island();

    auto drone = is_far_camera()
                     ? (opp_isle ? opp_isle->get_drone(cursor) : nullopt())
                     : APP.player_island().get_drone(cursor);

    PLATFORM.screen().clear();
    display();
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().display();

    auto add_line = [&](SystemString str,
                        const char* suffix,
                        bool specific,
                        auto callback) {
        auto line = loadstr(str);
        opts_->specific_.set(opts_->lines_.size(), specific);
        u8 y = opts_->lines_.size() + 1;
        opts_->lines_.emplace_back(OverlayCoord{1, y});
        if (opts_->lines_.size() == 1) {
            opts_->lines_.back().assign(line->c_str(), highlight_colors);
            opts_->lines_.back().append(suffix);
        } else {
            auto clr = specific ? specific_colors : Text::OptColors{};
            opts_->lines_.back().assign(line->c_str(), clr);
            opts_->lines_.back().append(suffix, clr);
        }
        opts_->longest_line_ =
            std::max(utf8::len(line->c_str()), size_t(opts_->longest_line_));
        opts_->strings_.push_back(str);
        opts_->suffixes_.push_back(suffix);
        opts_->callbacks_.push_back(callback);
    };

    if (auto isle = island()) {
        if (is_player_island(isle) or
            APP.game_mode() == App::GameMode::sandbox) {
            if (isle->interior_visible()) {
                add_line(SystemString::sel_menu_view_exterior,
                         "",
                         false,
                         []() -> ScenePtr {
                             show_island_exterior(&APP.player_island());
                             return null_scene();
                         });
            } else {
                add_line(SystemString::sel_menu_view_interior,
                         "",
                         false,
                         []() -> ScenePtr {
                             show_island_interior(&APP.player_island());
                             return null_scene();
                         });
            }
        }

        if (not PLATFORM.network_peer().is_connected()) {
            if (is_player_island(isle) or
                APP.game_mode() == App::GameMode::sandbox) {
                add_line(SystemString::sel_menu_move_blocks,
                         "",
                         false,
                         [far = is_far_camera()]() {
                             return move_blocks_setup(far);
                         });
            }
        }

        if (not PLATFORM.network_peer().is_connected()) {
            Character* chr = nullptr;
            if (auto room = isle->get_room(cursor)) {
                for (auto& c : room->characters()) {
                    if (c->grid_position() == cursor and
                        c->owner() == &APP.player()) {
                        chr = c.get();
                    }
                }
            }
            if (chr) {
                if (chr->is_superpinned()) {
                    add_line(SystemString::sel_menu_unpin_crewmember,
                             "",
                             true,
                             [id = chr->id()] {
                                 auto chr = Character::find_by_id(id);
                                 if (chr.first) {
                                     chr.first->un_superpin();
                                     chr.first->unpin();
                                 }
                                 return null_scene();
                             });
                } else {
                    add_line(SystemString::sel_menu_pin_crewmember,
                             "",
                             true,
                             [id = chr->id()] {
                                 auto chr = Character::find_by_id(id);
                                 if (chr.first) {
                                     chr.first->superpin();
                                 }
                                 return null_scene();
                             });
                }

                add_line(SystemString::sel_menu_crewmember_stats,
                         "",
                         true,
                         [id = chr->id(), far = is_far_camera()]() {
                             auto next = make_scene<CrewStatsScene>(id);
                             return next;
                         });
            }
        }

        bool bird_found = false;
        for (auto& bird : APP.birds()) {
            if (bird->island() == island() and bird->coordinate() == cursor) {
                bird_found = true;
            }
        }
        if (bird_found) {
            add_line(
                SystemString::sel_menu_spook_bird, "", true, [this, cursor]() {
                    for (auto& bird : APP.birds()) {
                        if (bird->island() == island() and
                            bird->coordinate() == cursor) {
                            PLATFORM.speaker().play_sound("seagull_1.raw", 0);
                            bird->signal();
                        }
                    }
                    return null_scene();
                });
        }


        if (not PLATFORM.network_peer().is_connected()) {
            auto room = island()->get_room(cursor);

            if (is_player_island(island()) and room) {
                if (room->upgrade_mt_name()) {
                    add_line(
                        SystemString::sel_menu_upgrade_block,
                        "",
                        true,
                        [this, cursor]() -> ScenePtr {
                            auto room = island()->get_room(cursor);
                            if (not room) {
                                return null_scene();
                            }
                            auto up = room->upgrade_mt_name();
                            if (not up) {
                                return null_scene();
                            }
                            auto to = metaclass_index(up);
                            return make_scene<UpgradePromptScene>(
                                room->position(), room->metaclass_index(), to);
                        });
                }
            }

            if (room and (room->description_visible() or
                          is_player_island(room->parent()))) {
                add_line(
                    SystemString::sel_menu_describe_block,
                    "",
                    true,
                    [this, cursor]() -> ScenePtr {
                        if (auto room = island()->get_room(cursor)) {
                            auto mt = room->metaclass_index();
                            auto next = make_scene<GlossaryViewerModule>(mt);
                            next->inspect_ = true;
                            next->skip_categories();
                            next->set_next_scene(
                                [far = is_far_camera()]() -> ScenePtr {
                                    PLATFORM.screen().schedule_fade(0);
                                    if (far) {
                                        return make_scene<InspectP2Scene>();
                                    } else {
                                        return make_scene<ReadyScene>();
                                    }
                                    return null_scene();
                                });
                            return next;
                        }
                        return null_scene();
                    });
            }
            if (powerdown_allowed and room and room->power_usage() < 0) {
                if (is_player_island(island())) {
                    add_line(SystemString::sel_menu_adjust_power,
                             "",
                             true,
                             [this, cursor]() {
                                 return make_scene<AdjustPowerScene>();
                             });
                }
            }
            if (powerdown_allowed and room and is_player_island(isle) and
                room->allows_powerdown()) {

                if (room->is_powered_down()) {
                    add_line(
                        SystemString::sel_menu_poweron,
                        format(" +%⚡", (*room->metaclass())->consumes_power())
                            .c_str(),
                        true,
                        [this, c = cursor]() {
                            if (auto room = island()->get_room(c)) {
                                room->set_powerdown(false);
                                PLATFORM.speaker().play_sound("poweron.raw", 4);
                                island()->schedule_repaint();
                            }
                            show_power_on_exit_ = true;
                            return null_scene();
                        });
                } else if (room->power_usage() > 0) {
                    add_line(
                        SystemString::sel_menu_powerdown,
                        format(" -%⚡", (*room->metaclass())->consumes_power())
                            .c_str(),
                        true,
                        [this, c = cursor]() {
                            if (auto room = island()->get_room(c)) {
                                room->set_powerdown(true);
                                PLATFORM.speaker().play_sound("powerdown.raw",
                                                              4);
                                island()->schedule_repaint();
                            }
                            show_power_on_exit_ = true;
                            return null_scene();
                        });
                }
            } else if (is_player_island(isle) and room and
                       (*room->metaclass())->category() ==
                           Room::Category::weapon) {

                if (not PLATFORM.network_peer().is_connected()) {
                    if (room->get_target()) {
                        add_line(SystemString::sel_menu_weapon_halt,
                                 "",
                                 true,
                                 [this, c = cursor]() {
                                     if (auto room = island()->get_room(c)) {
                                         room->unset_target();
                                     }
                                     return null_scene();
                                 });
                    }
                }
            } else if (is_player_island(isle) and room and room->cast<Bell>()) {
                add_line(
                    SystemString::sel_menu_eight_bells,
                    "",
                    true,
                    [this, c = cursor]() {
                        auto room = island()->get_room(c);
                        if (not room) {
                            return null_scene();
                        }

                        auto b = room->cast<Bell>();
                        if (not b) {
                            return null_scene();
                        }

                        const auto rx = room->position().x;

                        for (auto& r : island()->rooms()) {
                            for (auto& c : r->characters()) {
                                c->set_spr_flip(c->grid_position().x < rx);
                                c->drop_movement_path();
                            }
                        }
                        auto interval = milliseconds(1300);
                        b->schedule_chimes(interval, 4, 1, milliseconds(500));
                        auto delay = interval * 4;
                        APP.player().delay_crew_automation(delay);


                        APP.on_timeout(delay, [isle = island()] {
                            Buffer<RoomCoord, 20> positions;
                            for (auto& r : isle->rooms()) {
                                for (auto& c : r->characters()) {
                                    positions.push_back(c->grid_position());
                                }
                            }

                            rng::shuffle(positions, rng::utility_state);
                            for (auto& r : isle->rooms()) {
                                for (auto& c : r->characters()) {
                                    auto p1 = c->grid_position();
                                    auto p2 = positions.back();
                                    positions.pop_back();

                                    auto path =
                                        find_path(isle, c.get(), p1, p2);
                                    if (path and *path) {
                                        c->set_movement_path(std::move(*path));
                                        c->pin();

                                        network::packet::ChrSetTargetV2 packet;
                                        packet.target_x_ = p2.x;
                                        packet.target_y_ = p2.y;
                                        packet.chr_id_.set(c->id());
                                        packet.near_island_ =
                                            isle not_eq &APP.player_island();
                                        network::transmit(packet);
                                    }
                                }
                            }
                        });

                        return null_scene();
                    });

            } else if (drone) {
                if (not PLATFORM.network_peer().is_connected()) {

                    auto d = *drone;

                    add_line(SystemString::sel_menu_drone_stats,
                             "",
                             true,
                             [d]() { return make_scene<DroneStatsScene>(d); });

                    if ((*drone)->get_target()) {
                        add_line(SystemString::sel_menu_weapon_halt,
                                 "",
                                 true,
                                 [d]() {
                                     d->clear_target_queue();
                                     return null_scene();
                                 });
                    }
                }
            }
            if (is_player_island(isle) and room and room->cast_weapon()) {
                if (not PLATFORM.network_peer().is_connected()) {
                    add_line(SystemString::sel_menu_select_all,
                             "",
                             true,
                             [this, c = cursor]() -> ScenePtr {
                                 if (auto room = island()->get_room(c)) {
                                     auto mti = room->metaclass_index();
                                     return make_scene<GroupSelectionScene>(
                                         mti);
                                 }
                                 return null_scene();
                             });
                }
            }
        }
    }

    if (not PLATFORM.network_peer().is_connected()) {

        if (not is_far_camera() and cursor == *APP.player_island().flag_pos()) {
            add_line(SystemString::sel_menu_edit_flag, "", true, []() {
                auto ret = make_scene<FlagDesignerModule>();
                ret->editing_ingame_ = true;
                return ret;
            });
        }

        if (state_bit_load(StateBit::minimap_on)) {
            add_line(SystemString::sel_menu_hide_minimap, "", false, []() {
                state_bit_store(StateBit::minimap_on, false);
                return null_scene();
            });
        } else {
            add_line(SystemString::sel_menu_show_minimap, "", false, []() {
                PLATFORM.speaker().play_sound("minimap_open.raw", 3);
                state_bit_store(StateBit::minimap_on, true);
                return null_scene();
            });
        }

        add_line(SystemString::sel_menu_pause, "", false, []() {
            return set_gamespeed_setup();
        });
    }

    add_line(
        SystemString::sel_menu_back, "", false, []() { return null_scene(); });

    for (int i = 0; i < opts_->longest_line_ + 1; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 0, 425);
    }

    for (auto& line : opts_->lines_) {
        for (int i = line.len(); i < opts_->longest_line_; ++i) {
            if (&line == opts_->lines_.begin()) {
                line.append(" ", highlight_colors);
            } else {
                line.append(" ");
            }
        }
    }
    for (u32 y = 0; y < opts_->lines_.size(); ++y) {
        if (opts_->specific_.get(y)) {
            PLATFORM.set_tile(Layer::overlay, 0, y + 1, 159);
        } else {
            PLATFORM.set_tile(Layer::overlay, 0, y + 1, 112);
        }
    }
    PLATFORM.set_tile(Layer::overlay, 0, 1, 475);
}



void SelectMenuScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    display();
    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.screen().display();

    opts_->lines_.clear();

    if (auto ws = next.cast_world_scene()) {
        if (show_power_on_exit_) {
            ws->force_show_power_usage();
        }
    }
}



ScenePtr SelectMenuScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    if (APP.game_mode() not_eq App::GameMode::tutorial) {
        if (auto scene = select_menu_help(is_far_camera())) {
            return scene;
        }
    }


    for (u32 x = opts_->longest_line_ + 1; x < 30; ++x) {
        for (u32 y = 0; y < 20; ++y) {
            auto t = PLATFORM.get_tile(Layer::overlay, x, y);
            if (t) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
    }

    if ((is_far_camera() and not APP.opponent_island()) or
        player().key_down(Key::action_2)) {
        if (is_far_camera()) {
            return make_scene<InspectP2Scene>();
        } else {
            return make_scene<ReadyScene>();
        }
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };


    if (test_key(Key::right)) {
        PLATFORM.speaker().play_sound("cursor_tick", 0);
        auto prev_sel = sel_;
        if (opts_->specific_.get(sel_)) {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 159);
        } else {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 112);
        }
        redraw_line(sel_, false);
        int tries = 0;
        sel_ = (sel_ + 1) % opts_->lines_.size();
        while (tries < Options::cap) {
            if (opts_->specific_.get(sel_)) {
                break;
            }
            sel_ = (sel_ + 1) % opts_->lines_.size();
            ++tries;
        }
        if (tries == Options::cap) {
            sel_ = prev_sel;
        }
        PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 475);
        redraw_line(sel_, true);
    } else if (test_key(Key::down)) {
        PLATFORM.speaker().play_sound("cursor_tick", 0);
        if (opts_->specific_.get(sel_)) {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 159);
        } else {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 112);
        }
        redraw_line(sel_, false);
        if ((u32)sel_ < opts_->lines_.size() - 1) {
            ++sel_;
        } else {
            sel_ = 0;
        }
        PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 475);
        redraw_line(sel_, true);
    } else if (test_key(Key::up)) {
        PLATFORM.speaker().play_sound("cursor_tick", 0);
        if (opts_->specific_.get(sel_)) {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 159);
        } else {
            PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 112);
        }
        redraw_line(sel_, false);
        if (sel_ > 0) {
            --sel_;
        } else if (sel_ == 0) {
            sel_ = opts_->lines_.size() - 1;
        }
        PLATFORM.set_tile(Layer::overlay, 0, sel_ + 1, 475);
        redraw_line(sel_, true);
    }

    if (player().key_down(Key::action_1) or player().key_down(Key::select)) {
        if (auto next = opts_->callbacks_[sel_]()) {
            return next;
        } else {
            if (is_far_camera()) {
                return make_scene<InspectP2Scene>();
            } else {
                return make_scene<ReadyScene>();
            }
        }
    }


    if (auto isle = island()) {
        auto cursor = is_far_camera() ? globals().far_cursor_loc_
                                      : globals().near_cursor_loc_;

        auto pos = isle->visual_origin();
        pos.x += Fixnum::from_integer(cursor.x * 16);
        pos.y += Fixnum::from_integer(cursor.y * 16);

        auto view_center = PLATFORM.screen().get_view().get_center();
        pos.x -= Fixnum(view_center.x);
        pos.y -= Fixnum(view_center.y) + 8.0_fixed;

        static const Fixnum offset = 8.0_fixed;

        // Right-align menu box if right-aligned box is partially offscreen.
        Fixnum box_width = Fixnum::from_integer((opts_->longest_line_ + 1) * 8);
        if (pos.x + box_width + offset > 240.0_fixed) {
            pos.x -= box_width - (16.0_fixed - offset);
        } else {
            pos.x += offset;
        }
        pos.y += 8.0_fixed;

        // Push the menu upwards if it ends up offscreen
        if (opts_->lines_.size() > 8) {
            pos.y -= Fixnum::from_integer((opts_->lines_.size() - 8) * 8);
        }

        PLATFORM.set_overlay_origin(-pos.x.as_integer(), -pos.y.as_integer());
    }

    return null_scene();
}



void SelectMenuScene::display()
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((97 * 2));

    auto origin = APP.player_island().visual_origin();
    auto cursor_loc = globals().near_cursor_loc_;

    if (is_far_camera()) {
        if (APP.opponent_island()) {
            origin = APP.opponent_island()->visual_origin();
            cursor_loc = globals().far_cursor_loc_;
        }
    }

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);

    if (not is_far_camera()) {
        if (auto isle = island()) {
            if (auto room = isle->get_room(cursor_loc)) {
                room->display_on_hover(PLATFORM.screen(), cursor_loc);
            }
        }
    }

    WorldScene::display();
}



} // namespace skyland
