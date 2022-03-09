#include "ionBurst.hpp"

#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



SHARED_VARIABLE(ion_burst_damage);



IonBurst::IonBurst(const Vec2<Float>& position,
                   const Vec2<Float>& target,
                   Island* source,
                   const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(56);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    step_vector_ = direction(position, target) * speed;
}



Sound sound_fizzle("fizzle");



void IonBurst::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;


    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(90)) {
        anim_timer_ = 0;
        const auto kf = sprite_.get_texture_index();
        if (kf < 58) {
            sprite_.set_texture_index(kf + 1);
        } else {
            sprite_.set_texture_index(56);
        }
    }

    Island* target;
    if (source_ == &app.player_island()) {
        target = app.opponent_island();
    } else {
        target = &app.player_island();
    }

    if (target) {
        auto t_y = (int)target->origin().y;
        auto max_y = t_y + 16 * 16 + 32;
        auto min_y = t_y + construction_zone_min_y * 16;
        int max_x = 9999999;
        int min_x = -9999999;
        if (target == &app.player_island()) {
            // If we're shooting at the player's island, the projectile moves
            // leftwards, and we care about the min bound.
            min_x = (int)target->origin().x - 32;
        } else {
            // Otherwise, we need to check the max bound.
            max_x =
                (int)target->origin().x + 16 * target->terrain().size() + 32;
        }
        if (pos.y > max_y or pos.y < min_y or pos.x > max_x or pos.x < min_x) {
            this->destroy(pfrm, app, pos.y > min_y);
            sound_fizzle.play(pfrm, 1);
        }
    }


    if (timer_ > seconds(3)) {
        kill();
    }
}



void IonBurst::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;


    anim_timer_ -= delta;
    if (anim_timer_ < 0) {
        anim_timer_ = milliseconds(90);
        const auto kf = sprite_.get_texture_index();
        if (kf < 58) {
            sprite_.set_texture_index(kf + 1);
        } else {
            sprite_.set_texture_index(56);
        }
    }


    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        }
        kill();
    }
}



void IonBurst::destroy(Platform& pfrm, App& app, bool explosion)
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x);
            c.y_pos_.set(sprite_.get_position().y);
            memcpy(&c.x_speed_, &step_vector_.x, sizeof(Float));
            memcpy(&c.y_speed_, &step_vector_.y, sizeof(Float));
        };

    if (source_ == &app.player_island()) {
        time_stream::event::PlayerIonBurstDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    } else {
        time_stream::event::OpponentIonBurstDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    }

    kill();
    app.camera()->shake(8);
    if (explosion) {
        medium_explosion(pfrm, app, sprite_.get_position());
    }
}



void IonBurst::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and room.metaclass() == ion_cannon_mt) {
        return;
    }

    if (not((*room.metaclass())->properties() &
            RoomProperties::accepts_ion_damage)) {
        return;
    }

    destroy(pfrm, app, true);

    if ((*room.metaclass())->properties() &
        RoomProperties::cancels_ion_damage) {
        sound_fizzle.play(pfrm, 1);
    } else {
        room.apply_damage(pfrm, app, ion_burst_damage);
    }
}



} // namespace skyland
