////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "selectorScene.hpp"
#include "checkers.hpp"
#include "createBlockScene.hpp"
#include "helpScene.hpp"
#include "keyComboScene.hpp"
#include "macroverseScene.hpp"
#include "menuOptionsScene.hpp"
#include "modifiedSelectorScene.hpp"
#include "moveCheckerScene.hpp"
#include "nextTurnScene.hpp"
#include "skyland/entity/macro/macrocosmEffect.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene_pool.hpp"
#include "tileOptionsScene.hpp"



namespace skyland::macro
{



class KeylockScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override
    {
        MacrocosmScene::enter(state, prev);

        Text(SYSTR(macro_mode_lock)->c_str(), OverlayCoord{1, 1}).__detach();

        PLATFORM.set_tile(Layer::overlay, 1, 2, 393);
        Text(":", OverlayCoord{2, 2}).__detach();
        PLATFORM.set_tile(Layer::overlay, 3, 2, 388);
        PLATFORM.set_tile(Layer::overlay, 4, 2, 390);

        PLATFORM.set_tile(Layer::overlay, 1, 3, 395);
        Text(":", OverlayCoord{2, 3}).__detach();
        PLATFORM.set_tile(Layer::overlay, 3, 3, 387);
        PLATFORM.set_tile(Layer::overlay, 4, 3, 390);

        PLATFORM.set_tile(Layer::overlay, 1, 4, 394);
        Text(":", OverlayCoord{2, 4}).__detach();
        PLATFORM.set_tile(Layer::overlay, 3, 4, 389);
        PLATFORM.set_tile(Layer::overlay, 4, 4, 390);

        PLATFORM.set_tile(Layer::overlay, 1, 5, 392);
        Text(":", OverlayCoord{2, 5}).__detach();
        PLATFORM.set_tile(Layer::overlay, 3, 5, 112);
        PLATFORM.set_tile(Layer::overlay, 4, 5, 391);
    }


    void exit(macro::EngineImpl& state, Scene& next) override
    {
        MacrocosmScene::exit(state, next);
        PLATFORM.fill_overlay(0);
    }


    ScenePtr update(Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(player, state)) {
            return scene;
        }

        if (player.key_pressed(Key::left)) {
            state.data_->keylock_ = Keylock::improvelock;
            draw_keylock(state);
        } else if (player.key_pressed(Key::up)) {
            state.data_->keylock_ = Keylock::buildlock;
            draw_keylock(state);
        } else if (player.key_pressed(Key::right)) {
            state.data_->keylock_ = Keylock::deletelock;
            draw_keylock(state);
        } else if (player.key_pressed(Key::down)) {
            state.data_->keylock_ = Keylock::nolock;
            draw_keylock(state);
        }

        if (not player.key_pressed(Key::alt_1)) {
            return make_scene<SelectorScene>();
        }

        return null_scene();
    }
};



void SelectorScene::enter(macro::EngineImpl& state, Scene& prev)
{
    MacrocosmScene::enter(state, prev);

    if (show_island_size_) {
        text_.emplace(OverlayCoord{0, 19});
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
        text_.emplace(OverlayCoord{0, 19});
        describe_selected(state);
    }
}



void SelectorScene::exit(macro::EngineImpl& state, Scene& next)
{
    MacrocosmScene::exit(state, next);
    for (int i = 0; i < text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 0);
    }
    text_.reset();
    text_2_.reset();
}



ScenePtr SelectorScene::update(Player& player, macro::EngineImpl& state)
{
    if (paused_) {
        return null_scene();
    }

    if (auto scene = MacrocosmScene::update(player, state)) {
        return scene;
    }

    auto& sector = state.sector();
    auto cursor = sector.cursor();

    if (player.key_down(Key::start)) {
        await_start_key_ = true;
    }

    if (player.key_pressed(Key::start)) {
        if (player.key_down(Key::alt_2)) {
            return make_scene<KeyComboScene>(true);
        }
    } else {
        if (await_start_key_ and player.key_up(Key::start)) {
            auto next = make_scene<StartMenuScene>(0);
            next->cascade_anim_in_ = true;
            return next;
        }
    }

    auto test_key = [&](Key k) {
        return player.test_key(k, milliseconds(500), milliseconds(100));
    };


    if (player.key_pressed(Key::select) and not state.data_->freebuild_mode_ and
        not state.data_->checkers_mode_) {

        return make_scene<HelpScene>();
    }

    if (player.key_pressed(Key::alt_1) and state.data_->freebuild_mode_) {

        return make_scene<KeylockScene>();

    } else if (player.key_pressed(Key::alt_1) and
               not state.data_->freebuild_mode_ and
               not state.data_->checkers_mode_) {

        return make_scene<MenuOptionsScene>();

    } else if (player.key_down(Key::alt_2)) {

        return make_scene<ModifiedSelectorScene>();

    } else {

        if (test_key(Key::up) and cursor.y > 0) {
            --cursor.y;
            sector.set_cursor(cursor);
            describe_selected(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            describe_selected(state);
        } else if (test_key(Key::down) and cursor.y < sector.size().x - 1) {
            ++cursor.y;
            sector.set_cursor(cursor);
            describe_selected(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            describe_selected(state);
        } else if (test_key(Key::right) and cursor.x > 0) {
            --cursor.x;
            sector.set_cursor(cursor);
            describe_selected(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            describe_selected(state);
        } else if (test_key(Key::left) and cursor.x < sector.size().y - 1) {
            ++cursor.x;
            sector.set_cursor(cursor);
            describe_selected(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            describe_selected(state);
        }
    }

    if (player.key_down(Key::action_1)) {
        if (state.data_->checkers_mode_) {

            if (cursor.z not_eq 2) {
                return null_scene();
            }

            Vec3<u8> pos = {cursor.x, cursor.y, u8(cursor.z - 1)};
            auto& s = state.sector();
            auto board = CheckerBoard::from_sector(s);

            auto val = board.data_[cursor.x][cursor.y];
            if (val == CheckerBoard::red or val == CheckerBoard::red_king) {
                PLATFORM.speaker().play_sound("beep_error", 3);
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
                        PLATFORM.speaker().play_sound("beep_error", 3);
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
                PLATFORM.speaker().play_sound("button_wooden", 3);
                state.sector().set_block(cursor, terrain::Type::air);
                return make_scene<MoveCheckerScene>(pos, slots);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 3);
            }

        } else {
            switch (state.data_->keylock_) {
            case Keylock::nolock: {
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                return make_scene<TileOptionsScene>();
            }

            case Keylock::buildlock:
                return make_scene<CreateBlockScene>();

            case Keylock::improvelock: {
                auto c = state.sector().cursor();
                if (c.z > 0) {
                    --c.z;
                    auto& block = state.sector().get_block(c);
                    auto improvements =
                        terrain::improvements((terrain::Type)block.type_);
                    if (not improvements.empty()) {
                        PLATFORM.speaker().play_sound("button_wooden", 3);
                        return make_scene<BuildImprovementScene>();
                    }
                }
                break;
            }

            case Keylock::deletelock: {
                auto c = state.sector().cursor();
                if (c.z > 0) {
                    c.z--;
                    auto tp = state.sector().get_block(c).type();
                    if (tp == terrain::Type::dynamite) {
                        state.sector().ref_block(c).data_ = 1;
                    } else {
                        PLATFORM.speaker().play_sound("button_wooden", 3);
                        state.sector().set_block(c, terrain::Type::air);
                        state.sector().set_cursor(c);
                    }
                }
                break;
            }
            }
        }
    }

    return null_scene();
}



void SelectorScene::describe_selected(macro::EngineImpl& state)
{
    auto& sector = state.sector();

    for (int i = 0; i < text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 0);
    }

    auto s = SystemString::block_air;
    auto cursor = sector.cursor();
    auto tp = terrain::Type::air;
    // bool shadowed = false;
    if (cursor.z > 0) {
        --cursor.z;
        auto& block = sector.get_block(cursor);
        tp = block.type();
        s = block.name();
        // shadowed = block.shadowed_day_;
    }

    if (tp == terrain::Type::port) {
        StringBuffer<48> b;

        if (not b.empty()) {
            text_->assign(b.c_str());

            for (int i = 0; i < text_->len(); ++i) {
                PLATFORM.set_tile(Layer::overlay, i, 18, 425);
            }

            return;
        }
    } else {
        text_2_->erase();
    }

    StringBuffer<48> b;
    b += "(";
    b += loadstr(s)->c_str();
    b += ")";

    text_->assign(b.c_str());

    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        // Don't show any block stats, they don't matter in this game mode.
        return;
    }

    for (int i = 0; i < text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 425);
    }
}



} // namespace skyland::macro
