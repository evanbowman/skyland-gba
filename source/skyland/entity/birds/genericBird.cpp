#include "genericBird.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"
#include "number/random.hpp"



namespace skyland {



GenericBird::GenericBird(Platform::DynamicTexturePtr dt,
                         const Vec2<u8>& position,
                         bool near) :
    Bird({{}, {}}),
    dt_(dt),
    position_(position),
    near_(near)
{
    // NOTE: Two bird graphics of different color sit interleaved in texture
    // data.
    color_ = rng::choice<2>(rng::utility_state);

    dt_->remap(63 * 2 + color_);

    sprite_.set_texture_index(dt->mapping_index() * 2);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});
}



void GenericBird::update(Platform& pfrm, App& app, Microseconds delta)
{
    Island* island = nullptr;

    if (near_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island == nullptr or island->is_destroyed()) {
        this->kill();
        return;
    }

    switch (state_) {
    case State::roost: {
        auto o = island->origin();
        o.x += position_.x * 16;
        o.y += position_.y * 16;

        auto layer = island->layer();
        auto t = pfrm.get_tile(layer, position_.x, position_.y);
        switch (t) {

        case InteriorTile::roof_1:
        case InteriorTile::roof_3:
            o.y -= 10;
            break;

        case InteriorTile::roof_2:
            // A bird sitting atop a billowing chimney just looks silly.
            this->kill();
            break;

        case InteriorTile::flag_mount:
            o.y -= 48;
            o.x += 5;
            break;

        case InteriorTile::roof_flag:
            o.y -= 32;
            o.x += 5;
            break;

        case InteriorTile::null:
        case InteriorTile::grass:
            break;

        default:
            state_ = State::fly;
            break;
        }

        o.y += island->get_ambient_movement();

        sprite_.set_position(o);
        break;
    }

    case State::fly: {
        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(110)) {
            anim_timer_ -= milliseconds(110);

            if (anim_index_ == 6) {
                anim_index_ = 1;
            } else {
                ++anim_index_;
            }

            dt_->remap((63 + anim_index_) * 2 + color_);

        }

        auto pos = sprite_.get_position();

        if (pos.y < 450) {
            kill();
        }

        if (delta > 0) {
            pos.y -= delta * 0.0001f;
        }

        if (sprite_.get_flip().x) {
            pos.x += delta * speed_;
        } else {
            pos.x -= delta * speed_;
        }

        sprite_.set_position(pos);

        break;

    }
    }
}



void GenericBird::signal(Platform&, App&)
{
    if (state_ == State::roost) {
        state_ = State::fly;
        anim_timer_ = 0;

        sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

        speed_ = 0.00003f * rng::choice<4>(rng::utility_state);
    }
}



Island* GenericBird::island(App& app)
{
    if (near_) {
        return &player_island(app);
    } else {
        return opponent_island(app);
    }
}



}
