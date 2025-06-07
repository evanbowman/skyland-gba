////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "swerveMissile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"



namespace skyland
{



SHARED_VARIABLE(smart_missile_damage);



SwerveMissile::SwerveMissile(RoomCoord origin,
                             const SwerveMissileSilo::PathArray& path)
    : Projectile({{6, 6}, {6, 6}}), origin_(origin)
{
    memcpy(path_, path, sizeof path);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(58 * 2);

    sprite_.set_origin({8, 8});
}



void SwerveMissile::update(Time delta)
{
    timer_ += delta;

    if (++flame_counter_ == 3) {
        flame_counter_ = 0;

        auto p = sprite_.get_position();
        if (auto exp = APP.alloc_entity<Explosion>(p)) {
            APP.effects().push(std::move(exp));
        }
    }

    const auto path_interval = milliseconds(400);

    if (timer_ > path_interval) {
        timer_ = 0;
        ++path_index_;
        if (path_index_ == SwerveMissileSilo::Path::capacity()) {
            on_destroy();
            return;
        }
    }

    if (not APP.opponent_island()) {
        kill();
        return;
    }

    auto node_to_coord = [&](SwerveMissileSilo::Node n) {
        auto isle = n.near_ ? &APP.player_island() : APP.opponent_island();
        auto pos = isle->origin();
        pos.x += Fixnum::from_integer(n.pos_.x * 16) + 8.0_fixed;
        pos.y += Fixnum::from_integer(n.pos_.y * 16) + 8.0_fixed;
        return pos;
    };

    auto dest_pos = node_to_coord(path_[path_index_]);

    SwerveMissileSilo::Node prev_node;
    if (path_index_ > 0) {
        prev_node = path_[path_index_ - 1];
    } else {
        prev_node.near_ = true;
        prev_node.pos_.x = origin_.x;
        prev_node.pos_.y = origin_.y - 1;
    }

    auto src_pos = node_to_coord(prev_node);

    auto current_pos = interpolate_fp(
        dest_pos, src_pos, Fixnum(Float(timer_) / path_interval));

    sprite_.set_position(current_pos);
}



void SwerveMissile::rewind(Time delta)
{
    kill(); // TODO...
}



extern Sound sound_impact;



void SwerveMissile::on_collision(Room& room, Vec2<u8> coord)
{
    if (room.parent() == &APP.player_island() and
        is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < smart_missile_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    on_destroy();

    auto metac = room.metaclass();

    if (str_cmp((*metac)->name(), "stacked-hull") == 0) {
        room.apply_damage(smart_missile_damage / 4, nullptr);
    } else {
        room.apply_damage(smart_missile_damage, nullptr);
    }

    if (room.health()) {
        sound_impact.play(1);
    }
}



void SwerveMissile::on_collision(Entity& entity)
{
}



void SwerveMissile::on_destroy()
{
    // TODO... time stream event

    kill();
    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());
}



} // namespace skyland
