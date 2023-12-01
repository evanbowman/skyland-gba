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


#include "mindControl.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/mindControlTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStream.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



MindControl::MindControl(Island* parent,
                         const RoomCoord& position,
                         const char* n)
    : Room(parent, n, position)
{
}



void MindControl::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_mind_control)->c_str();
}



void MindControl::update(Microseconds delta)
{
    Room::update(delta);

    Room::ready();

    if (APP.opponent().is_friendly()) {
        return;
    }

    next_action_timer_ -= delta;
    if (next_action_timer_ <= 0) {
        next_action_timer_ = milliseconds(1000);

        if (controlled_character_) {
            auto [chr, room] =
                BasicCharacter::find_by_id(controlled_character_);

            if (not chr) {
                return;
            }

            if (is_player_island(parent())) {
                if (room->parent() == &APP.player_island()) {
                    EnemyAI::assign_local_character(app,
                                                    *chr,
                                                    &APP.player(),
                                                    &APP.player_island(),
                                                    APP.opponent_island(),
                                                    true);
                } else {
                    EnemyAI::assign_boarded_character(app,
                                                      *chr,
                                                      &APP.player(),
                                                      &APP.player_island(),
                                                      APP.opponent_island());
                }
            } else {
                if (room->parent() == &APP.player_island()) {
                    EnemyAI::assign_boarded_character(app,
                                                      *chr,
                                                      &APP.opponent(),
                                                      APP.opponent_island(),
                                                      &APP.player_island());
                } else {
                    EnemyAI::assign_local_character(app,
                                                    *chr,
                                                    &APP.opponent(),
                                                    APP.opponent_island(),
                                                    &APP.player_island());
                }
            }
        }
    }
}



void MindControl::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::mind_control1;
    buffer[x][y + 1] = InteriorTile::mind_control2;
}



void MindControl::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::mind_control1;
    buffer[position().x][position().y + 1] = Tile::mind_control2;
}



ScenePtr<Scene> MindControl::select(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (APP.opponent_island() and controlled_character_ == 0) {
        return scene_pool::alloc<MindControlTargetScene>(position());
    } else if (controlled_character_) {
        auto [chr, room] = BasicCharacter::find_by_id(controlled_character_);
        if (not chr) {
            return scene_pool::alloc<MindControlTargetScene>(position());
        }
        if (room->parent() == APP.opponent_island()) {
            globals().far_cursor_loc_ = chr->grid_position();
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            globals().near_cursor_loc_ = chr->grid_position();
        }
    }

    return null_scene();
}



void MindControl::display_on_hover(Platform::Screen& screen,

                                   const RoomCoord& cursor)
{
    if (not controlled_character_) {
        return;
    }

    auto [chr, room] = BasicCharacter::find_by_id(controlled_character_);

    if (chr) {
        Sprite spr;
        spr.set_position(chr->sprite().get_position());
        spr.set_texture_index(45);
        spr.set_size(Sprite::Size::w16_h32);

        screen.draw(spr);
    }
}



void MindControl::finalize()
{
    Room::finalize();

    if (controlled_character_) {
        auto [chr, room] = BasicCharacter::find_by_id(controlled_character_);
        if (chr) {
            chr->stop_mind_control(&other_island()->owner(), this);
        }
    }
}



} // namespace skyland
