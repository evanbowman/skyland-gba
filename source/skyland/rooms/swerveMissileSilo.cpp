////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "swerveMissileSilo.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/swerveMissile.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/worldScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class SwerveMissilePathSelectionScene : public ActiveWorldScene
{
public:
    SwerveMissilePathSelectionScene(const RoomCoord& weapon_loc)
        : weapon_loc_(weapon_loc)
    {
    }


    Optional<Text> desc_;


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);
        update_desc();
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);
        desc_.reset();
    }


    void update_desc()
    {
        if (not desc_) {
            desc_.emplace(OverlayCoord{0, (u8)(calc_screen_tiles().y - 1)});
        }

        desc_->assign(
            format(SYS_CSTR(draw_path), path_.size() + 1, path_.capacity())
                .c_str());
    }


    ScenePtr update(Time delta) override
    {
        if (auto new_scene = ActiveWorldScene::update(delta)) {
            return new_scene;
        }

        auto test_key = [&](Key k) {
            return APP.player().test_key(
                k, milliseconds(500), milliseconds(100));
        };

        if (test_key(Key::action_2)) {
            if (not path_.empty()) {
                path_.pop_back();
                exit_timer_ = 0;
            } else {
                globals().near_cursor_loc_ = weapon_loc_;
                return make_scene<ReadyScene>();
            }
        }

        if (not APP.opponent_island()) {
            globals().near_cursor_loc_ = weapon_loc_;
            return make_scene<ReadyScene>();
        }

        if (path_.full()) {
            exit_timer_ += delta;
            if (exit_timer_ > milliseconds(500)) {
                globals().near_cursor_loc_ = weapon_loc_;

                if (auto room = APP.player_island().get_room(weapon_loc_)) {
                    if (auto m = room->cast<SwerveMissileSilo>()) {
                        m->set_path(path_);
                        RoomCoord target;
                        target.x = path_.back().pos_.x;
                        target.y = path_.back().pos_.y;
                        m->set_target(target, true);

                        if (room->group() not_eq Room::Group::none) {
                            for (auto& other : APP.player_island().rooms()) {
                                if (other->group() == room->group()) {
                                    if (auto m2 =
                                            other->cast<SwerveMissileSilo>()) {
                                        m2->set_path(path_);
                                        m2->set_target(target, true);
                                    }
                                }
                            }
                        }
                    }
                }

                return make_scene<ReadyScene>();
            }
        }

        auto& cursor_loc =
            is_near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        auto isle = is_near_ ? &APP.player_island() : APP.opponent_island();

        if (is_near_) {
            near_camera();
        } else {
            far_camera();
        }

        if (test_key(Key::action_1)) {
            SwerveMissileSilo::Node new_node;
            new_node.near_ = is_near_;
            new_node.pos_.x = cursor_loc.x;
            new_node.pos_.y = cursor_loc.y;

            path_.push_back(new_node);
            update_desc();
            PLATFORM.speaker().play_sound("typewriter", 2);
        }

        if (test_key(Key::right)) {
            if (is_near_ and cursor_loc.x == isle->terrain().size()) {
                is_near_ = false;
                globals().far_cursor_loc_.y = cursor_loc.y;
                globals().far_cursor_loc_.x = 0;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            } else if ((is_near_ and cursor_loc.x < isle->terrain().size()) or
                       (not is_near_ and
                        cursor_loc.x < isle->terrain().size() - 1)) {
                ++cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (test_key(Key::left)) {
            if (not is_near_ and cursor_loc.x == 0) {
                is_near_ = true;
                globals().near_cursor_loc_.y = cursor_loc.y;
                globals().near_cursor_loc_.x =
                    APP.player_island().terrain().size();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            } else if (cursor_loc.x > 0) {
                --cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        return null_scene();
    }


    void display() override
    {
        if (not APP.opponent_island()) {
            return;
        }

        auto isle = is_near_ ? &APP.player_island() : APP.opponent_island();
        auto& cursor_loc =
            is_near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        auto origin = isle->visual_origin();

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_texture_index((17 * 2));
        sprite.set_size(Sprite::Size::w16_h16);

        PLATFORM.screen().draw(sprite);

        int n = 0;
        for (auto& node : path_) {
            ++n;
            Sprite spr;
            auto isle =
                node.near_ ? &APP.player_island() : APP.opponent_island();
            auto pos = isle->visual_origin();
            pos.x += Fixnum::from_integer(node.pos_.x * 16);
            pos.y += Fixnum::from_integer(node.pos_.y * 16);

            pos.x += 8.0_fixed;
            pos.y += 8.0_fixed;

            spr.set_size(Sprite::Size::w8_h8);
            spr.set_position(pos);
            spr.set_tidx_8x8(29, n);
            PLATFORM.screen().draw(spr);

            pos.x -= 8.0_fixed;
            pos.y -= 8.0_fixed;

            spr.set_size(Sprite::Size::w16_h16);
            spr.set_texture_index((97 * 2));

            spr.set_position(pos);
            PLATFORM.screen().draw(spr);
        }

        WorldScene::display();
    }


private:
    RoomCoord weapon_loc_;

    SwerveMissileSilo::Path path_;
    bool is_near_ = true;
    Time exit_timer_ = 0;
};



extern Sound missile_sound;
extern SharedVariable missile_silo_reload_ms;



void SwerveMissileSilo::format_description(StringBuffer<512>& buffer)
{
    // ...
}



SwerveMissileSilo::SwerveMissileSilo(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * missile_silo_reload_ms)
{
}



void SwerveMissileSilo::set_path(const Path& p)
{
    if (p.size() not_eq Path::capacity()) {
        LOGIC_ERROR();
    }

    memcpy(&path_, p.data(), sizeof path_);
}



void SwerveMissileSilo::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto room = island->get_room(*target_);
    if (room and not PLATFORM.network_peer().is_connected()) {
        // Note: if we use the center of a room as a target, we
        // have issues with multiplayer games, where a missile
        // targets a 2x2 room covered by 1x1 hull blocks for
        // example. Because the multiplayer coordinate system is
        // sort of mirrored over the y-axis, a missile aimed at
        // the border between two 1x1 blocks might hit the left
        // block in one game and the right block in another. So
        // missiles really should be constrained to columns for
        // multiplayer games. Just trying to explain the
        // network_peer().is_connected() check above.
        target = room->center();
    } else {
        auto origin = island->origin();
        origin.x += Fixnum::from_integer(target_->x * 16 + 8);
        origin.y += Fixnum::from_integer(target_->y * 16 + 8);
        target = origin;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<10>(target, rng::critical_state);
    }

    auto start = center();
    start.y -= 24.0_fixed;

    APP.camera()->shake(6);

    auto m = APP.alloc_entity<SwerveMissile>(position(), path_);

    if (m) {
        parent()->projectiles().push(std::move(m));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



ScenePtr SwerveMissileSilo::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (is_powered_down()) {
        return null_scene();
    }

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (auto scn = reject_if_friendly()) {
        return scn;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }


    if (is_player_island(parent())) {

        using Next = SwerveMissilePathSelectionScene;

        auto next = make_deferred_scene<Next>(position());

        if (APP.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(next);
        } else {
            return next();
        }
    }
    return null_scene();
}



Time SwerveMissileSilo::reload() const
{
    return 1000 * missile_silo_reload_ms;
}



void SwerveMissileSilo::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::homing_torpedo_1;
    buffer[position().x][position().y + 1] = Tile::homing_torpedo_2;
}



void SwerveMissileSilo::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::homing_torpedo_1;
    buffer[position().x][position().y + 1] = Tile::homing_torpedo_2;
}



} // namespace skyland
