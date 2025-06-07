////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "boardingPod.hpp"
#include "skyland/entity/explosion/explosion3.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



namespace
{



class BoardingPodEntity : public Entity
{
public:
    class Upper : public Entity
    {
    public:
        Upper(Platform::DynamicTexturePtr dt, Vec2<Fixnum> position)
            : Entity({}), dt_(dt)
        {
            position.y -= 48.0_fixed;
            position.x -= 16.0_fixed;
            sprite_.set_position(position);
            dt_->remap(81 * 2);
            sprite_.set_texture_index(dt->mapping_index());
        }


        void update(Time delta) override
        {
        }


        void rewind(Time delta) override
        {
        }


        bool entity_oom_deletable() const
        {
            return false;
        }


        Sprite& spr()
        {
            return sprite_;
        }

    private:
        Platform::DynamicTexturePtr dt_;
    };


    BoardingPodEntity(Platform::DynamicTexturePtr dt, Vec2<Fixnum> position)
        : Entity({{32, 32}, {16, 16}}), dt_(dt)
    {
        sprite_.set_position(position);
        sprite_.set_origin({16, 16});
        dt_->remap(80 * 2);
        sprite_.set_texture_index(dt->mapping_index());
    }


    void update(Time delta) override
    {
        auto pos = sprite_.get_position();

        auto spawn_flames = [&](Time timeout) {
            flame_spawn_count_ += delta;
            if (flame_spawn_count_ > timeout) {
                flame_spawn_count_ -= timeout;
                auto p = pos;
                p.x += 8.0_fixed;
                p.y += 16.0_fixed;
                if (auto exp = APP.alloc_entity<Explosion3>(p, 270 / 2, 1)) {
                    exp->set_speed({0.0_fixed, 0.0001_fixed});
                    APP.effects().push(std::move(exp));
                }
            }
        };

        switch (state_) {
        case State::rising_1:
            spawn_flames(milliseconds(80));
            timer_ += delta;
            pos.y -= APP.delta_fp() * 0.00006_fixed;
            if (timer_ > milliseconds(200)) {
                timer_ -= milliseconds(200);
                state_ = State::wait_1;
            }
            break;

        case State::wait_1:
            spawn_flames(milliseconds(250));
            timer_ += delta;
            pos.y -= APP.delta_fp() * 0.00001_fixed;
            if (timer_ > milliseconds(500)) {
                timer_ -= milliseconds(500);
                state_ = State::rising_2;
            }
            break;

        case State::rising_2:
            spawn_flames(milliseconds(40));
            timer_ += delta;
            pos.y -= APP.delta_fp() * 0.00027_fixed;
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
                pos.x = target_island_->visual_origin().x +
                        Fixnum::from_integer(target_.x * 16 + 16);
            }
            pos.y += APP.delta_fp() * 0.00041_fixed;
            timer_ += delta;
            if (target_island_) {
                target_island_->test_collision(*this);
            }
            break;

        default:
            break;
        }

        sprite_.set_position(pos);

        Sprite::Alpha a;
        if (sprite_.get_position().y < 450.0_fixed) {
            a = Sprite::Alpha::transparent;
        } else {
            a = Sprite::Alpha::opaque;
        }

        sprite_.set_alpha(a);

        pos.y -= 48.0_fixed;
        pos.x -= 16.0_fixed;

        if (upper_) {
            upper_->spr().set_position(pos);
            upper_->spr().set_alpha(a);
        }
    }



    void rewind(Time delta) override
    {
        auto pos = sprite_.get_position();

        switch (state_) {
        case State::rising_1:
            timer_ -= delta;
            pos.y += APP.delta_fp() * 0.00006_fixed;
            if (timer_ <= 0) {
                kill();
                if (upper_) {
                    upper_->kill();
                }
            }
            break;

        case State::wait_1:
            timer_ -= delta;
            pos.y += APP.delta_fp() * 0.00001_fixed;
            if (timer_ <= 0) {
                timer_ += milliseconds(200);
                state_ = State::rising_1;
            }
            break;

        case State::rising_2:
            timer_ -= delta;
            pos.y += APP.delta_fp() * 0.00027_fixed;
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
                pos.x = source_island_->visual_origin().x +
                        Fixnum::from_integer(source_.x * 16 + 16);
            }
            break;

        case State::falling:
            pos.y -= APP.delta_fp() * 0.00041_fixed;
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

        Sprite::Alpha a;
        if (sprite_.get_position().y < 450.0_fixed) {
            a = Sprite::Alpha::transparent;
        } else {
            a = Sprite::Alpha::opaque;
        }

        sprite_.set_alpha(a);

        pos.y -= 48.0_fixed;
        pos.x -= 16.0_fixed;

        if (upper_) {
            upper_->spr().set_position(pos);
            upper_->spr().set_alpha(a);
        }
    }



    void on_collision(Room& room, Vec2<u8> origin)
    {
        if ((*room.metaclass())->category() == Room::Category::wall or
            (*room.metaclass())->category() == Room::Category::decoration) {
            room.apply_damage(Room::health_upper_limit());
            return;
        }

        Room* r = &room;
        auto rc = room.position();
        rc.x = target_.x;
        for (u8 x = rc.x; x < rc.x + BoardingPod::size().x; ++x) {
            for (u8 y = 0; y < rc.y + BoardingPod::size().y; ++y) {
                if (auto r2 = target_island_->get_room({x, y})) {
                    if ((*r2->metaclass())->properties() &
                        RoomProperties::habitable) {
                        if (rc.y >= r2->position().y) {
                            r = r2;
                        }
                    }
                }
            }
        }

        kill();
        APP.camera()->shake(32);

        if (upper_) {
            upper_->kill();
            upper_ = nullptr;
        }

        auto coord = r->position();
        coord.y -= 3;
        coord.x = target_.x;

        for (u8 x = coord.x; x < coord.x + BoardingPod::size().x; ++x) {
            for (u8 y = 0; y < coord.y + BoardingPod::size().y; ++y) {
                if (auto r2 = target_island_->get_room({x, y})) {
                    r2->apply_damage(Room::health_upper_limit());
                }
            }
        }

        target_island_->add_room<BoardingPod>(coord, true);

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
                c->set_idle();
                c->set_parent(target_island_);
                room->edit_characters().push(std::move(c));
                room->update_description();
            }
        }

        time_stream::event::BoardingPodLanded e;
        e.timer_.set(timer_);
        e.source_near_ = is_player_island(source_island_);
        e.src_x_ = source_.x;
        e.src_y_ = source_.y;
        e.dst_x_ = target_.x;
        e.dst_y_ = target_.y;
        e.x_.set(sprite_.get_position().x.as_integer());
        e.y_.set(sprite_.get_position().y.as_integer());
        e.room_x_ = coord.x;
        e.room_y_ = coord.y;
        APP.time_stream().push(APP.level_timer(), e);
    }


    void restore(time_stream::event::BoardingPodLanded e, Room& r)
    {
        timer_ = e.timer_.get();
        state_ = State::falling;
        if (e.source_near_) {
            target_island_ = APP.opponent_island();
            source_island_ = &APP.player_island();
        } else {
            source_island_ = APP.opponent_island();
            target_island_ = &APP.player_island();
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


    EntityList<Character> characters_;
    Platform::DynamicTexturePtr dt_;
    Island* target_island_ = nullptr;
    Island* source_island_ = nullptr;
    Upper* upper_ = nullptr;

    Time flame_spawn_count_ = 0;
    Time timer_ = 0;

    RoomCoord source_;
    RoomCoord target_;

    enum class State {
        rising_1,
        wait_1,
        rising_2,
        wait_2,
        falling,
    } state_ = State::rising_1;
};
} // namespace



void restore_boarding_pod_entity(Room& src,
                                 time_stream::event::BoardingPodLanded& e)
{
    auto dt = PLATFORM.make_dynamic_texture();
    if (not dt) {
        return;
    }
    Vec2<Fixnum> pos;
    pos.x = e.x_.get();
    pos.y = e.y_.get();
    if (auto bp = APP.alloc_entity<BoardingPodEntity>(*dt, pos)) {
        bp->restore(e, src);

        if (auto dt2 = PLATFORM.make_dynamic_texture()) {
            pos.y -= 16.0_fixed;
            if (auto e =
                    APP.alloc_entity<BoardingPodEntity::Upper>(*dt2, pos)) {
                bp->upper_ = e.get();
                APP.effects().push(std::move(e));
            }
        }

        APP.effects().push(std::move(bp));
    }
}



BoardingPod::BoardingPod(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    owner_ = parent;
}



void BoardingPod::rewind(Time delta)
{
    Room::rewind(delta);

    if (launch_timer_ > 0) {
        launch_timer_ -= delta;
    } else {
        launch_timer_ = 0;
    }
}



void BoardingPod::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &owner()->owner() and
            character->state() not_eq Character::State::fighting) {
            ++characters_healing;
        }
    }

    if (characters_healing) {
        heal_timer_ += delta;
        if (heal_timer_ > milliseconds(1000)) {
            heal_timer_ -= milliseconds(1000);
            int distribute_health = 16;
            distribute_health /= characters_healing;
            for (auto& character : characters()) {
                if (character->owner() == &owner()->owner() and
                    character->state() not_eq Character::State::fighting) {
                    character->heal(distribute_health);
                }
            }
        }
    }

    if (launch_timer_) {
        launch_timer_ += delta;

        if (launch_timer_ > seconds(3)) {
            auto dt = PLATFORM.make_dynamic_texture();
            if (not dt) {
                return;
            }
            auto dt2 = PLATFORM.make_dynamic_texture();
            if (not dt2) {
                return;
            }
            auto pos = visual_center();
            pos.y += 10.0_fixed;
            if (auto bp = APP.alloc_entity<BoardingPodEntity>(*dt, pos)) {
                for (auto& chr : characters()) {
                    auto& c = *chr;

                    // Just to trigger a recording of the character's health for
                    // rewinding purposes.
                    c.apply_damage(0);

                    if (APP.time_stream().pushes_enabled() and
                        ((time_stream::event::Header*)APP.time_stream().end())
                                ->type_ not_eq time_stream::event::Type::
                                                   character_health_changed) {
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
                    e.owned_by_player_ = c.owner() == &APP.player();
                    e.near_ = is_player_island(parent());
                    e.is_replicant_ = c.is_replicant();
                    e.race_ = c.get_race();
                    e.icon_ = c.get_icon();
                    APP.time_stream().push(APP.level_timer(), e);
                }
                edit_characters().move_contents(bp->characters_);
                update_description();
                bp->source_ = position();
                bp->target_ = *target_;
                bp->source_island_ = parent();
                bp->target_island_ = other_island();

                if (auto e =
                        APP.alloc_entity<BoardingPodEntity::Upper>(*dt2, pos)) {
                    bp->upper_ = e.get();
                    APP.effects().push(std::move(e));
                }

                APP.effects().push(std::move(bp));
                apply_damage(9999, parent());
                APP.camera()->shake(6);
            }
        }
    }
}



void BoardingPod::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_boarding_pod)->c_str();
}



void BoardingPod::render_scaffolding(TileId buffer[16][16])
{
    if (parent()->interior_visible()) {
        if (parent()->get_room({position().x, u8(position().y + 3)})) {
            buffer[position().x][position().y + 3] = InteriorTile::ladder_mid;
        }
    }
    Room::render_scaffolding(buffer);
}



void BoardingPod::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::boarding_pod1;
    buffer[position().x + 1][position().y] = InteriorTile::boarding_pod2;
    buffer[position().x][position().y + 1] = InteriorTile::boarding_pod3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::boarding_pod4;
    buffer[position().x][position().y + 2] = InteriorTile::boarding_pod5;
    buffer[position().x + 1][position().y + 2] = InteriorTile::boarding_pod6;
}



void BoardingPod::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::boarding_pod1;
    buffer[position().x + 1][position().y] = Tile::boarding_pod2;
    buffer[position().x][position().y + 1] = Tile::boarding_pod3;
    buffer[position().x + 1][position().y + 1] = Tile::boarding_pod4;
    buffer[position().x][position().y + 2] = Tile::boarding_pod5;
    buffer[position().x + 1][position().y + 2] = Tile::boarding_pod6;
}



void BoardingPod::plot_walkable_zones(bool matrix[16][16],
                                      Character* for_character)
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
    matrix[position().x + 1][position().y + 1] = true;
    if (parent()->get_room({position().x, u8(position().y + 3)})) {
        matrix[position().x][position().y + 3] = true;
    }
}



void BoardingPod::set_target(const RoomCoord& target, bool pinned)
{
    auto island = other_island();
    if (island) {
        if (auto room = island->get_room(target)) {
            if (not((*room->metaclass())->properties() &
                    RoomProperties::habitable)) {
                PLATFORM.speaker().play_sound("beep_error", 3);
                return;
            }
        }
    }

    target_ = target;
    ready();
    launch_timer_ = 1;

    for (auto& b : APP.birds()) {
        if (b->island() == parent()) {
            b->signal();
        }
    }
}



void BoardingPod::unset_target()
{
    target_.reset();
}



ScenePtr BoardingPod::select(const RoomCoord& cursor)
{
    if (parent()->interior_visible()) {
        if (auto scn = Room::select_impl(cursor)) {
            return scn;
        }
    }


    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (APP.opponent_island() and
        // NOTE: cast should be safe, as a derived instance of Opponent should
        // always be bound to the opponent island.
        (static_cast<Opponent&>(APP.opponent_island()->owner()))
            .is_friendly()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 3);
        auto str = SYSTR(error_friendly);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }


    if (is_player_island(parent())) {

        using Next = WeaponSetTargetScene;

        auto next = make_deferred_scene<Next>(position(), true, target_);

        if (APP.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(next);
        } else {
            return next();
        }
    }

    return null_scene();
}



} // namespace skyland
