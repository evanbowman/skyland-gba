#include "genericBird.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"
#include "number/random.hpp"



namespace skyland {



GenericBird::GenericBird(Platform::DynamicTexturePtr dt,
                         const Vec2<u8>& position,
                         bool near) :
    Entity({{}, {}}),
    dt_(dt),
    position_(position),
    near_(near)
{
    dt_->remap((rng::choice<2>(rng::utility_state) + 63) * 2);

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

    case State::fly:
        // TODO...
        this->kill();
        break;
    }
}




}
