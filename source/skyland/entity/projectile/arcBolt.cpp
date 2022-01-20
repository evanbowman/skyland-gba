#include "arcBolt.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



SHARED_VARIABLE(arcbolt_damage);



ArcBolt::ArcBolt(const Vec2<Float>& position,
                       const Vec2<Float>& target,
                       Island* source,
                       const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(78);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00011f;
    step_vector_ = direction(position, target) * speed;
}



void ArcBolt::update(Platform&, App&, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(90)) {
        anim_timer_ = 0;
        const auto kf = sprite_.get_texture_index();
        if (kf == 78) {
            sprite_.set_texture_index(79);
        } else {
            sprite_.set_texture_index(78);
        }
    }


    if (timer_ > seconds(2)) {
        kill();
    }
}



extern Sound sound_impact;



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void ArcBolt::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent()) {
        if (room.position().x + (room.size().x - 1) == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }


    kill();
    app.camera().shake(8);
    medium_explosion(pfrm, app, sprite_.get_position());

    struct Temp {
        u8 matrix_[16][16];
        Buffer<Room*, 70> rooms_;
    };

    auto state = allocate_dynamic<Temp>(pfrm);
    room.parent()->plot_rooms(state->matrix_);

    // Remove any room from plot if type differs from colliding room type.
    for (u32 x = 0; x < room.parent()->terrain().size(); ++x) {
        for (int y = 0; y < 16; ++y) {
            if (state->matrix_[x][y]) {
                if (room.parent()->get_room({u8(x), u8(y)})->metaclass()
                    not_eq room.metaclass()) {
                    state->matrix_[x][y] = 0;
                }
            }
        }
    }

    flood_fill(pfrm, state->matrix_, 16, room.position().x, room.position().y);

    for (u32 x = 0; x < room.parent()->terrain().size(); ++x) {
        for (int y = 0; y < 16; ++y) {
            if (state->matrix_[x][y] == 16) {
                if (auto r = room.parent()->get_room({u8(x), u8(y)})) {
                    bool found = false;
                    for (auto& room : state->rooms_) {
                        if (room == r) {
                            found = true;
                            break;
                        }
                    }
                    if (not found) {
                        state->rooms_.push_back(r);
                    }
                }
            }
        }
    }

    for (auto& room : state->rooms_) {
        room->apply_damage(pfrm, app, arcbolt_damage);
    }

    if (room.health()) {
        sound_impact.play(pfrm, 1);
    }
}



} // namespace skyland
