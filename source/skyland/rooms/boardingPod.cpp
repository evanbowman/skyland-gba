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


#include "boardingPod.hpp"
#include "skyland/tile.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/sound.hpp"
#include "skyland/entity/explosion/explosion2.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland
{



namespace
{



class BoardingPodEntity : public Entity
{
public:

    BoardingPodEntity(Platform::DynamicTexturePtr dt,
                      Vec2<Fixnum> position) : Entity({{32, 32}, {16, 16}}),
                                               dt_(dt)
    {
        sprite_.set_position(position);
        sprite_.set_origin({16, 16});
        dt_->remap(80 * 2);
        sprite_.set_texture_index(dt->mapping_index());
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        auto pos = sprite_.get_position();

        switch (state_) {
        case State::rising_1:
            timer_ += delta;
            pos.y -= app.delta_fp() * 0.00006_fixed;
            if (timer_ > milliseconds(200)) {
                timer_ -= milliseconds(200);
                state_ = State::wait_1;
            }
            break;

        case State::wait_1:
            timer_ += delta;
            pos.y -= app.delta_fp() * 0.00001_fixed;
            if (timer_ > milliseconds(500)) {
                timer_ -= milliseconds(500);
                state_ = State::rising_2;
            }
            break;

        case State::rising_2:
            timer_ += delta;
            pos.y -= app.delta_fp() * 0.00027_fixed;
            if (timer_ > milliseconds(600)) {
                timer_ -= milliseconds(600);
                state_ = State::wait_2;
            }
            break;

        case State::wait_2:
            timer_ += delta;
            if (timer_ > seconds(3)) {
                timer_ -= seconds(3);
                state_ = State::falling;
            }
            break;

        case State::falling:
            if (target_island_) {
                pos.x = target_island_->visual_origin().x + target_.x * 16 + 16;
            }
            pos.y += app.delta_fp() * 0.00041_fixed;
            timer_ += delta;
            if (target_island_) {
                target_island_->test_collision(pfrm, app, *this);
            }
            break;

        default:
            break;
        }

        sprite_.set_position(pos);

        if (sprite_.get_position().y < 450.0_fixed) {
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else {
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }
    }



    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        auto pos = sprite_.get_position();

        switch (state_) {
        case State::rising_1:
            timer_ -= delta;
            pos.y += app.delta_fp() * 0.00006_fixed;
            if (timer_ <= 0) {
                kill();
            }
            break;

        case State::wait_1:
            timer_ -= delta;
            pos.y += app.delta_fp() * 0.00001_fixed;
            if (timer_ <= 0) {
                timer_ += milliseconds(200);
                state_ = State::rising_1;
            }
            break;

        case State::rising_2:
            timer_ -= delta;
            pos.y += app.delta_fp() * 0.00027_fixed;
            if (timer_ <= 0) {
                timer_ += milliseconds(500);
                state_ = State::wait_1;
            }
            break;

        case State::wait_2:
            timer_ -= delta;
            if (timer_ <= 0) {
                timer_ += milliseconds(600);
                state_ = State::rising_2;
            }
            if (target_island_) {
                pos.x = source_island_->visual_origin().x + source_.x * 16 + 16;
            }
            break;

        case State::falling:
            pos.y -= app.delta_fp() * 0.00041_fixed;
            timer_ -= delta;
            if (timer_ <= 0) {
                state_ = State::wait_2;
                timer_ += seconds(3);
            }
            break;

        default:
            break;
        }

        sprite_.set_position(pos);

        if (sprite_.get_position().y < 450.0_fixed) {
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else {
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }
    }



    void on_collision(Platform& pfrm, App& app, Room& room)
    {
        if ((*room.metaclass())->category() == Room::Category::wall or
            (*room.metaclass())->category() == Room::Category::decoration) {
            room.apply_damage(pfrm, app, 9999);
            return;
        }

        kill();
        app.camera()->shake(32);

        auto coord = room.position();
        coord.y -= 3;
        coord.x = target_.x;
        target_island_->add_room<BoardingPod>(pfrm,
                                              app,
                                              coord,
                                              true);

        int x_diff = coord.x - source_.x;
        int y_diff = coord.y - source_.y;


        if (auto room = target_island_->get_room(coord)) {

            if (auto bp = room->cast<BoardingPod>()) {
                bp->owner_ = source_island_;
            }

            for (auto& c : characters_) {
                auto gp = c->grid_position();
                gp.x += x_diff;
                gp.y += y_diff;
                c->drop_movement_path();
                c->set_grid_position(gp);
                c->set_idle(app);
                c->set_parent(target_island_);
                room->characters().push(std::move(c));
            }
        }

        time_stream::event::BoardingPodLanded e;
        e.timer_.set(timer_);
        e.source_near_ = source_island_ == &app.player_island();
        e.src_x_ = source_.x;
        e.src_y_ = source_.y;
        e.dst_x_ = target_.x;
        e.dst_y_ = target_.y;
        e.x_.set(sprite_.get_position().x.as_integer());
        e.y_.set(sprite_.get_position().y.as_integer());
        e.room_x_ = coord.x;
        e.room_y_ = coord.y;
        app.time_stream().push(app.level_timer(), e);
    }


    void restore(App& app,
                 time_stream::event::BoardingPodLanded e,
                 Room& r)
    {
        timer_ = e.timer_.get();
        state_ = State::falling;
        if (e.source_near_) {
            target_island_ = app.opponent_island();
            source_island_ = &app.player_island();
        } else {
            source_island_ = app.opponent_island();
            target_island_ = &app.player_island();
        }
        source_.x = e.src_x_;
        source_.y = e.src_y_;
        target_.x = e.dst_x_;
        target_.y = e.dst_y_;

        Vec2<Fixnum> pos;
        pos.x = e.x_.get();
        pos.y = e.y_.get();

        Vec2<u8> coord;
        coord.x = e.room_x_;
        coord.y = e.room_y_;

        int x_diff = coord.x - source_.x;
        int y_diff = coord.y - source_.y;

        for (auto& c : r.characters()) {
            auto gp = c->grid_position();
            gp.x -= x_diff;
            gp.y -= y_diff;
            c->set_grid_position(gp);
            c->set_parent(source_island_);
            characters_.push(std::move(c));
        }
    }


    bool entity_oom_deletable() const
    {
        return false;
    }


    EntityList<BasicCharacter> characters_;
    Platform::DynamicTexturePtr dt_;
    Island* target_island_ = nullptr;
    Island* source_island_ = nullptr;

    Microseconds flame_spawn_count_ = 0;
    Microseconds timer_ = 0;

    RoomCoord source_;
    RoomCoord target_;

    enum class State { rising_1, wait_1, rising_2, wait_2, falling, } state_ = State::rising_1;
};
}



void restore_boarding_pod_entity(Platform& pfrm,
                                 App& app,
                                 Room& src,
                                 time_stream::event::BoardingPodLanded& e)
{
    auto dt = pfrm.make_dynamic_texture();
    if (not dt) {
        return;
    }
    Vec2<Fixnum> pos;
    pos.x = e.x_.get();
    pos.y = e.y_.get();
    if (auto bp = app.alloc_entity<BoardingPodEntity>(pfrm, *dt, pos)) {
        bp->restore(app, e, src);
        app.effects().push(std::move(bp));
    }
}




BoardingPod::BoardingPod(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    owner_ = parent;
}



void BoardingPod::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    if (launch_timer_ > 0) {
        launch_timer_ -= delta;
    } else {
        launch_timer_ = 0;
    }
}



void BoardingPod::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    if (launch_timer_) {
        launch_timer_ += delta;

        if (launch_timer_ > seconds(3)) {
            auto dt = pfrm.make_dynamic_texture();
            if (not dt) {
                return;
            }
            auto dt2 = pfrm.make_dynamic_texture();
            if (not dt2) {
                return;
            }
            auto pos = visual_center();
            pos.y += 10;
            if (auto bp = app.alloc_entity<BoardingPodEntity>(pfrm, *dt, pos)) {
                for (auto& chr : characters()) {
                    auto& c = *chr;

                    // Just to trigger a recording of the character's health for
                    // rewinding purposes.
                    c.apply_damage(pfrm, app, 0);

                    if (((time_stream::event::Header*)app.time_stream().end())
                        ->type_ not_eq
                        time_stream::event::Type::character_health_changed) {
                        Platform::fatal("logic error: apply_damage(0) "
                                        "does not record hp");
                    }

                    // NOTE: CharacterDied is simply the easiest way to record
                    // state for rewinding purposes. Consider the characters
                    // dead, that way, they'll be respawned by the rewinding
                    // logic.
                    time_stream::event::CharacterDied e;
                    e.x_ = c.grid_position().x;
                    e.y_ = c.grid_position().y;
                    e.id_.set(c.id());
                    e.owned_by_player_ = c.owner() == &app.player();
                    e.near_ = parent() == &app.player_island();
                    e.is_replicant_ = c.is_replicant();
                    app.time_stream().push(app.level_timer(), e);
                }
                characters().move_contents(bp->characters_);
                bp->source_ = position();
                bp->target_ = *target_;
                bp->source_island_ = parent();
                bp->target_island_ = other_island(app);
                app.effects().push(std::move(bp));
                apply_damage(pfrm, app, 9999);
                app.camera()->shake(6);
            }
        }
    }
}



void BoardingPod::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_boarding_pod)->c_str();
}



void BoardingPod::render_scaffolding(App& app, TileId buffer[16][16])
{
    if (parent()->interior_visible()) {
        if (parent()->get_room({position().x, u8(position().y + 3)})) {
            buffer[position().x][position().y + 3] = InteriorTile::ladder_mid;
        }
    }
    Room::render_scaffolding(app, buffer);
}



void BoardingPod::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::boarding_pod1;
    buffer[position().x + 1][position().y] = InteriorTile::boarding_pod2;
    buffer[position().x][position().y + 1] = InteriorTile::boarding_pod3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::boarding_pod4;
    buffer[position().x][position().y + 2] = InteriorTile::boarding_pod5;
    buffer[position().x + 1][position().y + 2] = InteriorTile::boarding_pod6;
}



void BoardingPod::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::boarding_pod1;
    buffer[position().x + 1][position().y] = Tile::boarding_pod2;
    buffer[position().x][position().y + 1] = Tile::boarding_pod3;
    buffer[position().x + 1][position().y + 1] = Tile::boarding_pod4;
    buffer[position().x][position().y + 2] = Tile::boarding_pod5;
    buffer[position().x + 1][position().y + 2] = Tile::boarding_pod6;
}



void BoardingPod::plot_walkable_zones(App& app, bool matrix[16][16])
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
    matrix[position().x + 1][position().y + 1] = true;
    if (parent()->get_room({position().x, u8(position().y + 3)})) {
        matrix[position().x][position().y + 3] = true;
    }
}



void BoardingPod::set_target(Platform& pfrm, App& app, const RoomCoord& target)
{
    target_ = target;
    ready();
    launch_timer_ = 1;

    for (auto& b : app.birds()) {
        if (b->island(app) == parent()) {
            b->signal(pfrm, app);
        }
    }
}



void BoardingPod::unset_target(Platform& pfrm, App& app)
{
    target_.reset();
}



ScenePtr<Scene>
BoardingPod::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    if (parent()->interior_visible()) {
        if (auto scn = Room::select(pfrm, app, cursor)) {
            return scn;
        }
    }


    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (app.opponent_island() and
        // NOTE: cast should be safe, as a derived instance of Opponent should
        // always be bound to the opponent island.
        (static_cast<Opponent&>(app.opponent_island()->owner()))
            .is_friendly()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        pfrm.speaker().play_sound("beep_error", 3);
        auto str = SYSTR(error_friendly);
        return scene_pool::alloc<NotificationScene>(str->c_str(), future_scene);
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        pfrm.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return scene_pool::alloc<NotificationScene>(str->c_str(), future_scene);
    }


    if (parent() == &app.player_island()) {

        using Next = WeaponSetTargetScene;

        auto next =
            scene_pool::make_deferred_scene<Next>(position(), true, target_);

        if (app.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(pfrm, next);
        } else {
            return next();
        }
    }

    return null_scene();
}



}
