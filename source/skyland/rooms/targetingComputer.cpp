////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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


#include "targetingComputer.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/warhead.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStream.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



TargetingComputer::TargetingComputer(Island* parent,
                                     const RoomCoord& position,
                                     const char* n)
    : Room(parent, n, position)
{
}



void TargetingComputer::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_targeting_computer)->c_str();
}



void TargetingComputer::update(Time delta)
{
    Room::update(delta);

    if (not enabled_) {
        return;
    }

    if (is_powered_down()) {
        return;
    }

    Room::ready();

    if (&parent()->owner() == &APP.opponent()) {
        if (APP.game_mode() == App::GameMode::multiplayer) {
            return;
        }
        apply_damage(Room::health_upper_limit());
        return;
    }

    if (APP.opponent().is_friendly()) {
        for (auto& r : parent()->rooms()) {
            r->unset_target();
        }
        return;
    }

    if (PLATFORM.screen().fade_active()) {
        return;
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        // Bugfix: If stuff is even slightly de-syncd upon level entry, one game
        // can jump ahead of another. Add in an additional seconds buffer to
        // increase the liklihood that the targeting computer assigns weapon
        // targets at the same time.
        next_action_timer_ = seconds(4);
        return;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        return;
    }

    if (next_action_timer_ > 0) {
        next_action_timer_ -= delta;
    } else {
        if (room_update_index_ >= player_island().rooms().size()) {
            room_update_index_ = 0;
            next_action_timer_ = seconds(3);
        } else {
            auto& room = *player_island().rooms()[room_update_index_++];
            if (&room not_eq this and room.metaclass() == this->metaclass()) {
                // Player built two targeting computers.
                room.apply_damage(Room::health_upper_limit());
            }
            const auto category = (*room.metaclass())->category();
            if (category == Room::Category::weapon) {

                // Even when the targeting AI is active, the game allows you to
                // pin targets manually, and the AI won't try to assign them
                // again until the block to which the target is pinned is
                // destroyed.
                const bool has_pinned_target =
                    room.target_pinned() and room.get_target() and
                    APP.opponent_island()->get_room(*room.get_target());

                if (not has_pinned_target and not room.cast<Warhead>()) {
                    EnemyAI::update_room(room,
                                         APP.opponent_island()->rooms_plot(),
                                         &APP.player(),
                                         &APP.player_island(),
                                         APP.opponent_island());
                }
                next_action_timer_ = milliseconds(64);
            } else {
                next_action_timer_ = milliseconds(32);
            }
        }
    }

    // A block was destroyed. Attempt to re-assign some weapon targets, for
    // weapons that no longer have a valid target block.
    if (rescan_) {

        ATP highest_weight = 0.0_atp;
        Optional<RoomCoord> highest_weighted_target;

        // Find the highest-weighted target still in existence.
        for (auto& room : APP.player_island().rooms()) {
            const auto category = (*room->metaclass())->category();
            if (category == Room::Category::weapon) {
                if (auto target = room->get_target()) {
                    if (auto o = APP.opponent_island()->get_room(*target)) {
                        if (o->get_atp() > highest_weight) {
                            highest_weight = o->get_atp();
                            highest_weighted_target = target;
                        }
                    }
                }
            }
        }

        // Assign the highest weighted extant target to all rooms that no longer
        // have a valid target block.
        if (highest_weighted_target) {
            for (auto& room : APP.player_island().rooms()) {
                const auto category = (*room->metaclass())->category();
                if (category == Room::Category::weapon) {
                    if (auto target = room->get_target()) {
                        if (not APP.opponent_island()->get_room(*target)) {
                            room->set_target(*highest_weighted_target, false);
                        }
                    }
                }
            }
        }

        rescan_ = false;
    }
}



void TargetingComputer::unset_target()
{
    next_action_timer_ = seconds(2);
}



ScenePtr<Scene> TargetingComputer::select_impl(const RoomCoord& cursor)
{
    if (parent() == &player_island()) {
        // I repurposed the ReloadComplete event because it's already looped
        // into a callback (overriden below).
        time_stream::event::PlayerRoomReloadComplete e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomReloadComplete e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);
    }

    enabled_ = not enabled_;

    if (enabled_) {
        PLATFORM.speaker().play_sound("poweron", 4);
    } else {
        PLATFORM.speaker().play_sound("powerdown", 4);
    }

    schedule_repaint();

    Room::ready();

    return null_scene();
}



void TargetingComputer::___rewind___finished_reload()
{
    enabled_ = not enabled_;
    schedule_repaint();
}



void TargetingComputer::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    if (enabled_) {
        buffer[x][y] = InteriorTile::targeting_computer_1;
        buffer[x][y + 1] = InteriorTile::targeting_computer_2;
    } else {
        buffer[x][y] = InteriorTile::targeting_computer_3;
        buffer[x][y + 1] = InteriorTile::targeting_computer_4;
    }
}



void TargetingComputer::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    if (enabled_) {
        buffer[x][y] = Tile::targeting_computer_1;
        buffer[x][y + 1] = Tile::targeting_computer_2;
    } else {
        buffer[x][y] = Tile::targeting_computer_3;
        buffer[x][y + 1] = Tile::targeting_computer_4;
    }
}



} // namespace skyland
