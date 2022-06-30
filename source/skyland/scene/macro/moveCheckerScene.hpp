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


#pragma once

#include "checkers.hpp"
#include "macrocosmScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland::macro
{



// TODO: implement forced capture!



inline bool slot_within_bounds(const Vec2<u8>& pos)
{
    return pos.x > 0 and pos.y > 0 and pos.x < 9 and pos.y < 9;
}



inline Vec2<s8> checker_forward_vector(const CheckerBoard& board,
                                       const Vec2<u8>& pos)
{
    const auto rot = board.orientation_;

    switch (board.data_[pos.x][pos.y]) {
    case CheckerBoard::Checker::red:
    case CheckerBoard::Checker::red_king:
        switch (rot) {
        case terrain::Sector::Orientation::north:
            return {1, 0};

        case terrain::Sector::Orientation::south:
            return {-1, 0};

        case terrain::Sector::Orientation::east:
            return {0, 1};

        case terrain::Sector::Orientation::west:
            return {0, -1};
        }
        break;

    case CheckerBoard::Checker::black:
    case CheckerBoard::Checker::black_king:
        switch (rot) {
        case terrain::Sector::Orientation::north:
            return {-1, 0};

        case terrain::Sector::Orientation::south:
            return {1, 0};

        case terrain::Sector::Orientation::east:
            return {0, -1};

        case terrain::Sector::Orientation::west:
            return {0, 1};
        }
        break;

    default:
        break;
    }

    return {};
}



inline bool checker_color_same(CheckerBoard::Checker lhs,
                               CheckerBoard::Checker rhs)
{
    auto is_red = [](auto v) {
        return v == CheckerBoard::red or v == CheckerBoard::red_king;
    };

    return is_red(lhs) == is_red(rhs);
}



inline auto checker_get_movement_slots(const CheckerBoard& board,
                                       const Vec2<u8>& pos)
{
    Buffer<Vec2<u8>, 4> result;

    auto dif = checker_forward_vector(board, pos);

    auto piece = board.data_[pos.x][pos.y];

    auto is_free = [&](auto& coord) {
        return board.data_[coord.x][coord.y] == CheckerBoard::Checker::none;
    };

    auto push = [&](auto& coord, auto& jump_coord) {
        if (is_free(coord)) {
            if (slot_within_bounds(coord)) {
                result.push_back(coord);
            }
        } else {
            auto other = board.data_[coord.x][coord.y];
            if (not checker_color_same(piece, other)) {
                if (is_free(jump_coord) and slot_within_bounds(jump_coord)) {
                    result.push_back(jump_coord);
                }
            }
        }
    };

    auto push_moves = [&] {
        if (dif.x not_eq 0) {
            auto slot1 = pos;
            slot1.x += dif.x;
            auto slot2 = slot1;

            slot1.y += 1;
            slot2.y -= 1;

            auto jc1 = slot1;
            jc1.x += dif.x;
            jc1.y += 1;

            auto jc2 = slot2;
            jc2.x += dif.x;
            jc2.y -= 1;

            push(slot1, jc1);
            push(slot2, jc2);

        } else {
            auto slot1 = pos;
            slot1.y += dif.y;
            auto slot2 = slot1;

            slot1.x += 1;
            slot2.x -= 1;

            auto jc1 = slot1;
            jc1.y += dif.y;
            jc1.x += 1;

            auto jc2 = slot2;
            jc2.y += dif.y;
            jc2.x -= 1;

            push(slot1, jc1);
            push(slot2, jc2);
        }
    };

    push_moves();

    if (piece == CheckerBoard::red_king or piece == CheckerBoard::black_king) {
        // Kings may move in reverse.
        dif.x *= -1;
        dif.y *= -1;
        push_moves();
    }

    return result;
}



inline bool checker_has_jumps(const CheckerBoard& board, const Vec2<u8>& pos)
{
    auto slots = checker_get_movement_slots(board, pos);
    for (auto& slot : slots) {
        if (abs(slot.x - pos.x) > 1 and abs(slot.y - pos.y) > 1) {
            return true;
        }
    }
    return false;
}



inline std::optional<std::pair<CheckerBoard::Checker, Vec2<u8>>>
checker_move(CheckerBoard& board, const Vec2<u8>& from, const Vec2<u8>& to)
{
    const auto type = board.data_[from.x][from.y];
    board.data_[from.x][from.y] = CheckerBoard::Checker::none;
    board.data_[to.x][to.y] = type;

    if (not slot_within_bounds(
            (to.cast<int>() + checker_forward_vector(board, to).cast<int>())
                .cast<u8>())) {
        // We've reached the edge of the board. King me!
        if (board.data_[to.x][to.y] == CheckerBoard::red) {
            board.data_[to.x][to.y] = CheckerBoard::red_king;
        }
        if (board.data_[to.x][to.y] == CheckerBoard::black) {
            board.data_[to.x][to.y] = CheckerBoard::black_king;
        }
    }

    if (abs(from.x - to.x) > 1 and abs(from.y - to.y) > 1) {

        auto midpoint = from;
        if (to.x > midpoint.x) {
            ++midpoint.x;
        } else {
            --midpoint.x;
        }

        if (to.y > midpoint.y) {
            ++midpoint.y;
        } else {
            --midpoint.y;
        }

        auto took = board.data_[midpoint.x][midpoint.y];
        board.data_[midpoint.x][midpoint.y] = CheckerBoard::Checker::none;

        return std::make_pair(took, midpoint);
    }

    return {};
}



using MinimaxResult = int;



bool checkers_forced_jumps = true;



inline MinimaxResult
checkers_minimax(CheckerBoard& board, int depth, int alpha, int beta)
{
    // NOTE: max depth needs even parity? Or doesn't matter?
    if (depth > 4) {
        return board.score();
    }

    const bool minimize = depth % 2 == 0;
    const bool maximize = not minimize;

    MinimaxResult best = 0;
    auto reset_best = [&]() {
        if (minimize) {
            best = 999999;
        } else {
            best = -999999;
        }
    };
    reset_best();

    Buffer<Vec2<u8>, 12> pieces;
    Buffer<Vec2<u8>, 12> pieces_with_jumps;

    for (u8 x = 1; x < 9; ++x) {
        for (u8 y = 1; y < 9; ++y) {
            auto c = board.data_[x][y];
            if (((c == CheckerBoard::red or c == CheckerBoard::red_king) and
                 maximize) or
                ((c == CheckerBoard::black or c == CheckerBoard::black_king) and
                 minimize)) {
                pieces.push_back({x, y});
            }
        }
    }

    if (checkers_forced_jumps) {
        for (auto& p : pieces) {
            if (checker_has_jumps(board, p)) {
                pieces_with_jumps.push_back(p);
            }
        }
        if (not pieces_with_jumps.empty()) {
            pieces = pieces_with_jumps;
        }
    }

    for (auto& piece : pieces) {
        auto slots = checker_get_movement_slots(board, piece);
        const u8 x = piece.x;
        const u8 y = piece.y;
        for (auto& s : slots) {

            if (not pieces_with_jumps.empty()) {
                if (abs(s.x - x) < 2 or abs(s.y - y) < 2) {
                    // Forced jump, the piece cannot move without
                    // jumping.
                    continue;
                }
            }

            // NOTE: need to store prev because checker_move does king
            // conversion.
            const auto prev = board.data_[x][y];
            auto took = checker_move(board, {x, y}, s);


            auto result = checkers_minimax(board, depth + 1, alpha, beta);
            if (minimize) {
                best = std::min(best, result);
                beta = std::min(best, beta);
            } else {
                best = std::max(best, result);
                alpha = std::max(best, alpha);
            }

            // Undo each state change, as we're doing everything
            // in-place. If we had a large stack, it'd be less bug-prone
            // to just copy the board every time. But, we're on a
            // gameboy, and only have a few kb for the stack.
            if (took) {
                // un-add value from taking the piece
                // un-take the taken piece
                board.data_[took->second.x][took->second.y] = took->first;
            }

            // un-move the piece
            board.data_[s.x][s.y] = CheckerBoard::Checker::none;
            board.data_[x][y] = prev;

            if (beta <= alpha) {
                goto DONE;
            }
        }
    }

DONE:

    return best;
}



inline std::pair<Vec2<u8>, Vec2<u8>>
checkers_opponent_move(CheckerBoard& board,
                       std::optional<Vec2<u8>> piece = {})
{
    struct Move
    {
        Vec2<u8> from_;
        Vec2<u8> to_;
        MinimaxResult data_;
    };

    Buffer<Move, 48> moves_;

    Buffer<Vec2<u8>, 12> pieces;
    Buffer<Vec2<u8>, 12> pieces_with_jumps;

    if (piece) {
        // Move one specific piece.
        pieces.push_back(*piece);
        pieces_with_jumps.push_back(*piece);
    } else {
        for (u8 x = 1; x < 9; ++x) {
            for (u8 y = 1; y < 9; ++y) {
                auto c = board.data_[x][y];
                if ((c == CheckerBoard::red or c == CheckerBoard::red_king)) {
                    pieces.push_back({x, y});
                }
            }
        }

        if (checkers_forced_jumps) {
            for (auto& p : pieces) {
                if (checker_has_jumps(board, p)) {
                    pieces_with_jumps.push_back(p);
                }
            }
            if (not pieces_with_jumps.empty()) {
                pieces = pieces_with_jumps;
            }
        }
    }

    for (auto& piece : pieces) {
        const u8 x = piece.x;
        const u8 y = piece.y;

        auto c = board.data_[x][y];
        if (c == CheckerBoard::Checker::red or
            c == CheckerBoard::Checker::red_king) {
            auto slots = checker_get_movement_slots(board, {x, y});
            if (not slots.empty()) {
                for (auto& slot : slots) {
                    if (not pieces_with_jumps.empty()) {
                        if (abs(slot.x - x) < 2 or abs(slot.y - y) < 2) {
                            // Forced jump, the piece cannot move without
                            // jumping.
                            continue;
                        }
                    }
                    const auto prev = board.data_[x][y];
                    auto took = checker_move(board, {x, y}, slot);
                    Move m;
                    m.from_ = {x, y};
                    m.to_ = slot;

                    m.data_ = checkers_minimax(board, 0, -9999, 9999);
                    moves_.push_back(m);

                    // Undo move!
                    if (took) {
                        board.data_[took->second.x][took->second.y] =
                            took->first;
                    }
                    board.data_[slot.x][slot.y] = CheckerBoard::Checker::none;
                    board.data_[x][y] = prev;
                }
            }
        }
    }

    std::sort(moves_.begin(), moves_.end(), [](auto& lhs, auto& rhs) {
        return lhs.data_ < rhs.data_;
    });

    if (not moves_.empty()) {
        return {moves_.back().from_, moves_.back().to_};
    } else {
        return {{0, 0}, {0, 0}};
    }
}



class OpponentMoveCheckerScene : public MacrocosmScene
{
public:

    std::optional<Vec2<u8>> piece_;

    OpponentMoveCheckerScene(std::optional<Vec2<u8>> piece = {}) :
        piece_(piece)
    {
    }

    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state)
    {
        Text t(pfrm, SYSTR(checkers_ai_thinking)->c_str(), OverlayCoord{0, 19});
        pfrm.screen().clear();
        pfrm.screen().display();

        auto& sector = state.sector();
        auto board = CheckerBoard::from_sector(sector);

        auto result = checkers_opponent_move(board, piece_);
        auto opp_from = result.first;
        auto opp_to = result.second;
        auto prev = sector.get_block({opp_from.x, opp_from.y, 1}).type();
        sector.set_block({opp_from.x, opp_from.y, 1}, terrain::Type::air);
        sector.set_block({opp_to.x, opp_to.y, 1}, prev);

        auto board_prev = board.data_[opp_from.x][opp_from.y];

        auto took = checker_move(board, opp_from, opp_to);
        if (took) {
            sector.set_block({took->second.x, took->second.y, 1},
                             terrain::Type::air);
        }

        if (board.data_[opp_to.x][opp_to.y] not_eq board_prev) {
            if (board.data_[opp_to.x][opp_to.y] == CheckerBoard::red_king) {
                sector.set_block({opp_to.x, opp_to.y, 1},
                                 terrain::Type::checker_red_king);
            }
        } else {
            if (took) {
                // We took a piece, we may need to move again...
                auto slots = checker_get_movement_slots(board, opp_to);
                for (auto it = slots.begin(); it not_eq slots.end();) {
                    auto s = *it;
                    if (abs(s.x - opp_to.x) > 1 and abs(s.y - opp_to.y) > 1) {
                        // Slot is a jump
                        ++it;
                    } else {
                        it = slots.erase(it);
                    }
                }
                // We have jumps left for this checker, we must jump again!
                if (not slots.empty()) {
                    return scene_pool::alloc<OpponentMoveCheckerScene>(opp_to);
                }
            }
        }


        return scene_pool::alloc<SelectorScene>();
    }
};



class MoveCheckerScene : public MacrocosmScene
{
public:
    MoveCheckerScene(const Vec3<u8>& piece_loc,
                     const Buffer<Vec2<u8>, 4>& slots,
                     bool cancellable = true)
        : piece_loc_(piece_loc), slots_(slots), cancellable_(cancellable)
    {
    }


    void enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev) override
    {
        refresh(state);
    }


    void exit(Platform& pfrm, macro::EngineImpl& state, Scene& next) override
    {
    }


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
            return scene;
        }

        const u32 prev_slot = current_slot_;

        if (player.key_down(pfrm, Key::action_2) and cancellable_) {
            auto& sector = state.sector();

            for (auto& slot : slots_) {

                sector.set_block({slot.x, slot.y, 1}, terrain::Type::air);
            }

            sector.set_cursor(piece_loc_);

            return scene_pool::alloc<SelectorScene>();

        } else if (player.key_down(pfrm, Key::action_1)) {
            auto& sector = state.sector();

            auto board = CheckerBoard::from_sector(sector);
            auto board_prev = board.data_[piece_loc_.x][piece_loc_.y];
            auto slot = slots_[current_slot_];
            auto took = checker_move(board, {piece_loc_.x, piece_loc_.y}, slot);

            const bool king_convert =
                board_prev not_eq board.data_[slot.x][slot.y] and
                board.data_[slot.x][slot.y] == CheckerBoard::black_king;

            const auto type = sector.get_block(piece_loc_).type();

            for (auto& slot : slots_) {
                sector.set_block({slot.x, slot.y, 1}, terrain::Type::air);
            }
            if (not slots_.empty()) {
                auto s = slots_[current_slot_];
                sector.set_block(
                    {s.x, s.y, 1},
                    king_convert ? terrain::Type::checker_black_king : type);
            }

            if (took) {
                sector.set_block({took->second.x, took->second.y, 1},
                                 terrain::Type::air);
            }

            sector.set_block(piece_loc_, terrain::Type::air);

            if (took and not king_convert) {
                auto slots = checker_get_movement_slots(board, slot);
                for (auto it = slots.begin(); it not_eq slots.end();) {
                    auto s = *it;
                    if (abs(s.x - slot.x) > 1 and abs(s.y - slot.y) > 1) {
                        // Slot is a jump
                        ++it;
                    } else {
                        it = slots.erase(it);
                    }
                }
                // We have jumps left for this checker, we must jump again!
                if (not slots.empty()) {
                    return scene_pool::alloc<MoveCheckerScene>(
                        Vec3<u8>{slot.x, slot.y, 1}, slots, false);
                }
            } else {
                sector.set_cursor(piece_loc_);
            }

            return scene_pool::alloc<OpponentMoveCheckerScene>();

        } else if (player.key_down(pfrm, Key::right)) {
            ++current_slot_;
            current_slot_ %= slots_.size();
        }

        if (current_slot_ not_eq prev_slot) {
            refresh(state);
        }

        return null_scene();
    }


    void refresh(macro::EngineImpl& state)
    {
        auto& sector = state.sector();

        for (u32 i = 0; i < slots_.size(); ++i) {
            auto s = slots_[i];
            if (i == current_slot_) {
                sector.set_block({s.x, s.y, 1}, terrain::Type::air);
            } else {
                sector.set_block({s.x, s.y, 1},
                                 terrain::Type::checker_highlight);
            }
        }

        if (not slots_.empty()) {
            auto s = slots_[current_slot_];
            sector.set_cursor({s.x, s.y, 1});
        }
    }


private:
    Vec3<u8> piece_loc_;
    Buffer<Vec2<u8>, 4> slots_;
    u32 current_slot_ = 0;
    bool cancellable_ = false;
};



} // namespace skyland::macro
