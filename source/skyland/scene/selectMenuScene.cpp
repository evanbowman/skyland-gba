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


#include "selectMenuScene.hpp"
#include "constructionScene.hpp"
#include "inspectP2Scene.hpp"
#include "menuPromptScene.hpp"
#include "modules/flagDesignerModule.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "moveCharacterScene.hpp"
#include "moveRoomScene.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "setGamespeedScene.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



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
    opts_->lines_[line].assign(loadstr(opts_->strings_[line])->c_str(),
                               highlight ? highlight_colors
                                         : (opts_->specific_.get(line)
                                                ? specific_colors
                                                : Text::OptColors{}));

    for (int i = opts_->lines_[line].len(); i < opts_->longest_line_; ++i) {
        opts_->lines_[line].append(
            " ", highlight ? highlight_colors : Text::OptColors{});
    }
}



static ScenePtr<Scene> select_menu_help(bool far)
{
    const auto flag = GlobalPersistentData::sel_menu_help_prompt_dont_remind_me;

    const bool skip_prompt = APP.gp_.stateflags_.get(flag) or
                             state_bit_load(StateBit::sel_menu_help_prompt);

    auto dont_remind = []() {
        APP.gp_.stateflags_.set(flag, true);
        save::store_global_data(APP.gp_);
    };

    auto next = [far] {
        auto ret = scene_pool::alloc<SelectMenuScene>();
        if (far) {
            ret->far_camera();
        }
        return ret;
    };

    if (not skip_prompt) {
        state_bit_store(StateBit::sel_menu_help_prompt, true);
        return scene_pool::alloc<MenuPromptScene>(
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



static ScenePtr<Scene> move_blocks_setup(bool far)
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

    auto next = [far] { return scene_pool::alloc<MoveRoomScene>(not far); };

    if (not skip_prompt) {
        state_bit_store(StateBit::move_blocks_help_prompt, true);
        return scene_pool::alloc<MenuPromptScene>(
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



static ScenePtr<Scene> set_gamespeed_setup()
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
        auto ret = scene_pool::alloc<SetGamespeedScene>();
        ret->button_mode_ = 1;
        return ret;
    };

    if (not skip_prompt) {
        state_bit_store(StateBit::gamespeed_help_prompt, true);
        return scene_pool::alloc<MenuPromptScene>(
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



class SetCharacterIconScene : public ActiveWorldScene
{
public:
    SetCharacterIconScene(CharacterId id) : id_(id)
    {
        // ...
    }


    void show_icon()
    {
        int offset = (icons[index_] - 1) * 16;
        PLATFORM.load_overlay_chunk(181, offset, 16, "character_art");
        const auto st = calc_screen_tiles();

        PLATFORM.set_tile(Layer::overlay, 4, st.y - 4, 398);
        PLATFORM.set_tile(Layer::overlay, 4, st.y - 3, 399);

        int tile = 181;
        for (int y = 0; y < 4; ++y) {
            for (int x = 0; x < 4; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, st.y - 4 + y, tile++, 10);
            }
        }
    }


    static constexpr u8 icons[] =
        {1, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 20, 21, 22, 23, 19};


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        auto found = BasicCharacter::find_by_id(id_);
        if (found.first) {
            auto icon = found.first->get_icon();
            for (u32 i = 0; i < (sizeof icons); ++i) {
                if (icon == icons[i]) {
                    index_ = i;
                    break;
                }
            }
        }

        show_icon();
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);

        PLATFORM.fill_overlay(0);
    }


    ScenePtr<Scene> update(Microseconds delta)
    {
        if (auto next = ActiveWorldScene::update(delta)) {
            return next;
        }

        auto test_key = [&](Key k) {
            return APP.player().test_key(
                k, milliseconds(500), milliseconds(100));
        };

        if (test_key(Key::down)) {
            if (index_ < (int)(sizeof icons) - 1) {
                ++index_;
            } else {
                index_ = 0;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_icon();
        }

        if (test_key(Key::up)) {
            if (index_ == 0) {
                index_ = (int)(sizeof icons) - 1;
            } else {
                --index_;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            show_icon();
        }

        if (test_key(Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }

        if (test_key(Key::action_1)) {
            auto found = BasicCharacter::find_by_id(id_);
            if (found.first) {
                PLATFORM.speaker().play_sound("button_wooden", 3);
                auto icn = icons[index_];
                if (icn == 19) {
                    found.first->set_icon(0);
                } else {
                    found.first->set_icon(icn);
                }
            }
            return scene_pool::alloc<ReadyScene>();
        }

        return null_scene();
    }


private:
    CharacterId id_;
    int index_ = 0;
};



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


    PLATFORM.screen().clear();
    display();
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().display();

    auto add_line = [&](SystemString str, bool specific, auto callback) {
        auto line = loadstr(str);
        opts_->specific_.set(opts_->lines_.size(), specific);
        u8 y = opts_->lines_.size() + 1;
        opts_->lines_.emplace_back(OverlayCoord{1, y});
        if (opts_->lines_.size() == 1) {
            opts_->lines_.back().assign(line->c_str(), highlight_colors);
        } else {
            opts_->lines_.back().assign(
                line->c_str(), specific ? specific_colors : Text::OptColors{});
        }
        opts_->longest_line_ =
            std::max(utf8::len(line->c_str()), size_t(opts_->longest_line_));
        opts_->strings_.push_back(str);
        opts_->callbacks_.push_back(callback);
    };

    if (auto isle = island()) {
        if (isle == &APP.player_island() or
            APP.game_mode() == App::GameMode::sandbox) {
            if (isle->interior_visible()) {
                add_line(SystemString::sel_menu_view_exterior,
                         false,
                         []() -> ScenePtr<Scene> {
                             show_island_exterior(&APP.player_island());
                             return null_scene();
                         });
            } else {
                add_line(SystemString::sel_menu_view_interior,
                         false,
                         []() -> ScenePtr<Scene> {
                             show_island_interior(&APP.player_island());
                             return null_scene();
                         });
            }
        }

        if (not PLATFORM.network_peer().is_connected()) {
            if (isle == &APP.player_island() or
                APP.game_mode() == App::GameMode::sandbox) {
                add_line(SystemString::sel_menu_move_blocks,
                         false,
                         [far = is_far_camera()]() {
                             return move_blocks_setup(far);
                         });
            }
        }

        if (auto chr = isle->character_at_location(cursor)) {
            if (chr->is_superpinned()) {
                add_line(SystemString::sel_menu_unpin_crewmember,
                         true,
                         [id = chr->id()] {
                             auto chr = BasicCharacter::find_by_id(id);
                             if (chr.first) {
                                 chr.first->un_superpin();
                                 chr.first->unpin();
                             }
                             return null_scene();
                         });
            } else {
                add_line(SystemString::sel_menu_pin_crewmember,
                         true,
                         [id = chr->id()] {
                             auto chr = BasicCharacter::find_by_id(id);
                             if (chr.first) {
                                 chr.first->superpin();
                             }
                             return null_scene();
                         });
            }
            add_line(SystemString::sel_menu_name_crewmember,
                     true,
                     [id = chr->id(),
                      far = is_far_camera(),
                      vis = isle->interior_visible()]() -> ScenePtr<Scene> {
                         if (PLATFORM.network_peer().is_connected()) {
                             PLATFORM.speaker().play_sound("beep_error", 3);
                             return scene_pool::alloc<ReadyScene>();
                         }
                         if (not vis) {
                             show_island_interior(&APP.player_island());
                         }

                         auto next = scene_pool::alloc<ModifyCharacterScene>(
                             id, not far);
                         next->modify_name_ = true;
                         return next;
                     });

            add_line(SystemString::sel_menu_crewmember_icon,
                     true,
                     [id = chr->id()]() {
                         return scene_pool::alloc<SetCharacterIconScene>(id);
                     });

        } else if (auto room = isle->get_room(cursor)) {
            if ((isle == &APP.player_island() or
                 APP.game_mode() == App::GameMode::sandbox) and
                APP.game_mode() not_eq App::GameMode::co_op) {
                if ((*room->metaclass())->category() ==
                    Room::Category::weapon) {

                    if (not PLATFORM.network_peer().is_connected()) {
                        add_line(SystemString::sel_menu_weapon_halt,
                                 true,
                                 [this, cursor]() {
                                     if (auto room =
                                             island()->get_room(cursor)) {
                                         room->unset_target();
                                     }
                                     return null_scene();
                                 });
                    }
                }
            }
        }

        bool bird_found = false;
        for (auto& bird : APP.birds()) {
            if (bird->island() == island() and bird->coordinate() == cursor) {
                bird_found = true;
            }
        }
        if (bird_found) {
            add_line(SystemString::sel_menu_spook_bird, true, [this, cursor]() {
                for (auto& bird : APP.birds()) {
                    if (bird->island() == island() and
                        bird->coordinate() == cursor) {
                        PLATFORM.speaker().play_sound("seagull_1", 0);
                        bird->signal();
                    }
                }
                return null_scene();
            });
        }


        if (not PLATFORM.network_peer().is_connected()) {
            auto room = island()->get_room(cursor);
            if (island() == &APP.player_island() and room) {
                if (room->upgrade_mt_name()) {
                    add_line(
                        SystemString::sel_menu_upgrade_block,
                        true,
                        [this, cursor]() -> ScenePtr<Scene> {
                            auto room = island()->get_room(cursor);
                            if (not room) {
                                return null_scene();
                            }
                            auto up = room->upgrade_mt_name();
                            if (not up) {
                                return null_scene();
                            }
                            auto to = metaclass_index(up);
                            return scene_pool::alloc<UpgradePromptScene>(
                                room->position(), room->metaclass_index(), to);
                        });
                }
            }
            if (room and (room->description_visible() or
                          room->parent() == &APP.player_island())) {
                add_line(
                    SystemString::sel_menu_describe_block,
                    true,
                    [this, cursor]() -> ScenePtr<Scene> {
                        if (auto room = island()->get_room(cursor)) {
                            auto mt = room->metaclass_index();
                            auto next =
                                scene_pool::alloc<GlossaryViewerModule>(mt);
                            next->inspect_ = true;
                            next->skip_categories();
                            next->set_next_scene([far = is_far_camera()]()
                                                     -> ScenePtr<Scene> {
                                PLATFORM.screen().schedule_fade(0);
                                if (far) {
                                    return scene_pool::alloc<InspectP2Scene>();
                                } else {
                                    return scene_pool::alloc<ReadyScene>();
                                }
                                return null_scene();
                            });
                            return next;
                        }
                        return null_scene();
                    });
            }
        }
    }

    if (not PLATFORM.network_peer().is_connected()) {

        if (not is_far_camera() and cursor == *APP.player_island().flag_pos()) {
            add_line(SystemString::sel_menu_edit_flag, true, [this, cursor]() {
                auto ret = scene_pool::alloc<FlagDesignerModule>();
                ret->editing_ingame_ = true;
                return ret;
            });
        }

        add_line(SystemString::sel_menu_pause, false, [this, cursor]() {
            return set_gamespeed_setup();
        });
    }

    add_line(SystemString::sel_menu_back, false, [this, cursor]() {
        return null_scene();
    });

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
}



ScenePtr<Scene> SelectMenuScene::update(Microseconds delta)
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
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
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
                return scene_pool::alloc<InspectP2Scene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
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
