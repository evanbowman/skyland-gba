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


#include "selectorScene.hpp"
#include "checkers.hpp"
#include "createBlockScene.hpp"
#include "keyComboScene.hpp"
#include "menuOptionsScene.hpp"
#include "modifiedSelectorScene.hpp"
#include "moveCheckerScene.hpp"
#include "nextTurnScene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene_pool.hpp"
#include "tileOptionsScene.hpp"



namespace skyland::macro
{



void SelectorScene::enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);
    Text::platform_retain_alphabet(pfrm);

    if (show_island_size_) {
        text_.emplace(pfrm, OverlayCoord{0, 19});
        auto& s = state.sector();
        auto sz = s.size();
        text_->append("(");
        text_->append(sz.x);
        text_->append("x");
        text_->append(sz.y);
        text_->append("x");
        text_->append(sz.z - 1);
        text_->append(")");
    } else if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, 19});
        describe_selected(pfrm, state);
    }
}



void SelectorScene::exit(Platform& pfrm, macro::EngineImpl& state, Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);
    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 0);
    }
    text_.reset();
    text_2_.reset();
}



ScenePtr<Scene>
SelectorScene::update(Platform& pfrm, Player& player, macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    auto& sector = state.sector();
    auto cursor = sector.cursor();

    if (player.key_down(pfrm, Key::start)) {
        await_start_key_ = true;
    }

    if (player.key_pressed(pfrm, Key::start)) {
        if (player.key_down(pfrm, Key::alt_2)) {
            return scene_pool::alloc<KeyComboScene>(true);
        }
    } else {
        if (await_start_key_ and player.key_up(pfrm, Key::start)) {
            return scene_pool::alloc<StartMenuScene>(0);
        }
    }


    if (player.key_down(pfrm, Key::select)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        StringBuffer<48> temp;
        return scene_pool::alloc<NextTurnScene>();
    }


    auto test_key = [&](Key k) {
        return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
    };


    if (player.key_pressed(pfrm, Key::alt_1) and
        not state.data_->freebuild_mode_ and not state.data_->checkers_mode_) {

        return scene_pool::alloc<MenuOptionsScene>();

    } else if (player.key_down(pfrm, Key::alt_2)) {

        return scene_pool::alloc<ModifiedSelectorScene>();

    } else {

        if (test_key(Key::up) and cursor.y > 0) {
            --cursor.y;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
            describe_selected(pfrm, state);
        } else if (test_key(Key::down) and cursor.y < sector.size().x - 1) {
            ++cursor.y;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
            describe_selected(pfrm, state);
        } else if (test_key(Key::right) and cursor.x > 0) {
            --cursor.x;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
            describe_selected(pfrm, state);
        } else if (test_key(Key::left) and cursor.x < sector.size().y - 1) {
            ++cursor.x;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
            describe_selected(pfrm, state);
        }
    }

    if (player.key_down(pfrm, Key::action_1)) {
        if (state.data_->checkers_mode_) {

            if (cursor.z not_eq 2) {
                return null_scene();
            }

            Vec3<u8> pos = {cursor.x, cursor.y, u8(cursor.z - 1)};
            auto& s = state.sector();
            auto board = CheckerBoard::from_sector(s);

            auto val = board.data_[cursor.x][cursor.y];
            if (val == CheckerBoard::red or val == CheckerBoard::red_king) {
                pfrm.speaker().play_sound("beep_error", 3);
                return null_scene();
            }

            auto slots = checker_get_movement_slots(board, {pos.x, pos.y});

            Buffer<Vec2<u8>, 12> pieces_with_jumps;
            if (checkers_forced_jumps) {
                for (u8 x = 1; x < 9; ++x) {
                    for (u8 y = 1; y < 9; ++y) {
                        auto p = board.data_[x][y];
                        if (p == CheckerBoard::black or
                            p == CheckerBoard::black_king) {
                            if (checker_has_jumps(board, {x, y})) {
                                pieces_with_jumps.push_back({x, y});
                            }
                        }
                    }
                }
                if (not pieces_with_jumps.empty()) {
                    bool found = false;
                    for (auto& piece : pieces_with_jumps) {
                        if (pos.x == piece.x and pos.y == piece.y) {
                            found = true;
                        }
                    }
                    if (not found) {
                        text_->assign(SYSTR(checkers_forced_jump)->c_str());
                        pfrm.speaker().play_sound("beep_error", 3);
                        return null_scene();
                    }

                    // Forced jumps: remove any slot that isn't a jump, because
                    // the piece can jump.
                    for (auto it = slots.begin(); it not_eq slots.end();) {
                        auto s = *it;
                        if (abs(s.x - pos.x) < 2 or abs(s.y - pos.y) < 2) {
                            it = slots.erase(it);
                        } else {
                            ++it;
                        }
                    }
                }
            }

            if (not slots.empty()) {
                pfrm.speaker().play_sound("button_wooden", 3);
                state.sector().set_block(cursor, terrain::Type::air);
                return scene_pool::alloc<MoveCheckerScene>(pos, slots);
            } else {
                pfrm.speaker().play_sound("beep_error", 3);
            }

        } else {
            pfrm.speaker().play_sound("button_wooden", 3);
            return scene_pool::alloc<TileOptionsScene>();
        }
    }

    return null_scene();
}



void SelectorScene::describe_selected(Platform& pfrm, macro::EngineImpl& state)
{
    auto& sector = state.sector();

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 0);
    }

    auto s = SystemString::block_air;
    auto cursor = sector.cursor();
    auto tp = terrain::Type::air;
    bool shadowed = false;
    if (cursor.z > 0) {
        --cursor.z;
        auto& block = sector.get_block(cursor);
        tp = block.type();
        s = block.name();
        shadowed = block.shadowed_day_;
    }

    if (tp == terrain::Type::port) {
        StringBuffer<48> b;
        if (auto exp = sector.exports()) {
            for (auto& e : *exp) {
                if (e.source_coord_ == cursor) {
                    b += loadstr(pfrm, name(e.c))->c_str();
                    b += "(";
                    b += stringify(e.export_supply_.get());
                    b += ")->";
                    if (auto dest = state.load_sector(e.destination_)) {
                        b += dest->name();
                    }
                }
            }
        }

        if (not b.empty()) {
            text_->assign(b.c_str());

            for (int i = 0; i < text_->len(); ++i) {
                pfrm.set_tile(Layer::overlay, i, 18, 425);
            }

            return;
        }
    } else {
        text_2_->erase();
    }

    StringBuffer<48> b;
    b += "(";
    b += loadstr(pfrm, s)->c_str();
    b += ")";

    text_->assign(b.c_str());

    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        // Don't show any block stats, they don't matter in this game mode.
        return;
    }

    auto stats = terrain::stats(tp, shadowed);

    if ((terrain::categories(tp) & terrain::Categories::crop) and shadowed) {
        text_->append("  ");
        pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 411);
    } else {
        if (stats.food_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 414);
            text_->append(stats.food_);
        }

        if (stats.housing_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 416);
            text_->append(stats.housing_);
        }

        if (stats.employment_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 415);
            text_->append(stats.employment_);
        }

        if (stats.happiness_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 409);
            text_->append(stats.happiness_);
        }

        if (not stats.commodities_.empty()) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 417);
            text_->append(stats.commodities_[0].supply_);
        }
    }

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }
}



} // namespace skyland::macro
