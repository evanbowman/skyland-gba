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
    void enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev) override
    {
        MacrocosmScene::enter(pfrm, state, prev);

        Text(pfrm, SYSTR(macro_mode_lock)->c_str(), OverlayCoord{1, 1})
            .__detach();

        pfrm.set_tile(Layer::overlay, 1, 2, 393);
        Text(pfrm, ":", OverlayCoord{2, 2}).__detach();
        pfrm.set_tile(Layer::overlay, 3, 2, 388);
        pfrm.set_tile(Layer::overlay, 4, 2, 390);

        pfrm.set_tile(Layer::overlay, 1, 3, 395);
        Text(pfrm, ":", OverlayCoord{2, 3}).__detach();
        pfrm.set_tile(Layer::overlay, 3, 3, 387);
        pfrm.set_tile(Layer::overlay, 4, 3, 390);

        pfrm.set_tile(Layer::overlay, 1, 4, 394);
        Text(pfrm, ":", OverlayCoord{2, 4}).__detach();
        pfrm.set_tile(Layer::overlay, 3, 4, 389);
        pfrm.set_tile(Layer::overlay, 4, 4, 390);

        pfrm.set_tile(Layer::overlay, 1, 5, 392);
        Text(pfrm, ":", OverlayCoord{2, 5}).__detach();
        pfrm.set_tile(Layer::overlay, 3, 5, 112);
        pfrm.set_tile(Layer::overlay, 4, 5, 391);
    }


    void exit(Platform& pfrm, macro::EngineImpl& state, Scene& next) override
    {
        MacrocosmScene::exit(pfrm, state, next);
        pfrm.fill_overlay(0);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
            return scene;
        }

        if (player.key_pressed(pfrm, Key::left)) {
            state.data_->keylock_ = Keylock::improvelock;
            draw_keylock(pfrm, state);
        } else if (player.key_pressed(pfrm, Key::up)) {
            state.data_->keylock_ = Keylock::buildlock;
            draw_keylock(pfrm, state);
        } else if (player.key_pressed(pfrm, Key::right)) {
            state.data_->keylock_ = Keylock::deletelock;
            draw_keylock(pfrm, state);
        } else if (player.key_pressed(pfrm, Key::down)) {
            state.data_->keylock_ = Keylock::nolock;
            draw_keylock(pfrm, state);
        }

        if (not player.key_pressed(pfrm, Key::alt_1)) {
            return scene_pool::alloc<SelectorScene>();
        }

        return null_scene();
    }
};



class HelpScene : public Scene
{
public:


private:
    int page_ = 0;
    std::optional<TextView> message_;
};



void SelectorScene::enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);

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
    if (paused_) {
        return null_scene();
    }

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
            auto next = scene_pool::alloc<StartMenuScene>(0);
            next->cascade_anim_in_ = true;
            return next;
        }
    }

    auto test_key = [&](Key k) {
        return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
    };


    // if (player.key_pressed(pfrm, Key::select) and
    //     not state.data_->freebuild_mode_ and
    //     not state.data_->checkers_mode_) {

    //     return scene_pool::alloc<HelpScene>();
    // }

    if (player.key_pressed(pfrm, Key::alt_1) and state.data_->freebuild_mode_) {

        return scene_pool::alloc<KeylockScene>();

    } else if (player.key_pressed(pfrm, Key::alt_1) and
               not state.data_->freebuild_mode_ and
               not state.data_->checkers_mode_) {

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
            switch (state.data_->keylock_) {
            case Keylock::nolock: {
                pfrm.speaker().play_sound("cursor_tick", 0);
                return scene_pool::alloc<TileOptionsScene>();
            }

            case Keylock::buildlock:
                return scene_pool::alloc<CreateBlockScene>();

            case Keylock::improvelock: {
                auto c = state.sector().cursor();
                if (c.z > 0) {
                    --c.z;
                    auto& block = state.sector().get_block(c);
                    auto improvements =
                        terrain::improvements((terrain::Type)block.type_);
                    if (not improvements.empty()) {
                        pfrm.speaker().play_sound("button_wooden", 3);
                        return scene_pool::alloc<BuildImprovementScene>();
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
                        pfrm.speaker().play_sound("button_wooden", 3);
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



void SelectorScene::describe_selected(Platform& pfrm, macro::EngineImpl& state)
{
    auto& sector = state.sector();

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 0);
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

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }
}



} // namespace skyland::macro
