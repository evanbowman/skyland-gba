#include "genericBird.hpp"
#include "number/random.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



GenericBird::GenericBird(Platform::DynamicTexturePtr dt,
                         const Vec2<u8>& position,
                         bool near)
    : Bird({{}, {}}), dt_(dt), position_(position), near_(near)
{
    // NOTE: Two bird graphics of different color sit interleaved in texture
    // data.
    color_ = rng::choice<2>(rng::utility_state);

    dt_->remap(63 * 2 + color_);

    sprite_.set_texture_index(dt->mapping_index() * 2);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

    sprite_.set_priority(2);
}



void GenericBird::update(Platform& pfrm, App& app, Microseconds delta)
{
    Island* island = nullptr;

    if (near_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island->is_destroyed()) {
        this->signal(pfrm, app);
    }

    if (island == nullptr) {
        this->kill();
    }

    switch (state_) {
    case State::roost: {
        auto o = island->origin();
        o.x += position_.x * 16;
        o.y += position_.y * 16;

        auto layer = island->layer();
        auto t = pfrm.get_tile(layer, position_.x, position_.y);
        auto below = pfrm.get_tile(layer, position_.x, position_.y + 1);

        if ((below == Tile::null or below == Tile::grass) and delta > 0) {
            alerted_ = true;
        } else if (below == Tile::null or below == Tile::grass) {
            ++position_.y;
        }

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
            o.y -= 32;
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
            if (delta > 0) {
                alerted_ = true;
            }
            position_.y -= 1;
            break;
        }

        o.y += island->get_ambient_movement();

        sprite_.set_position(o);

        if (alerted_ and delta > 0) {
            state_ = State::fly;
            anim_timer_ = 0;

            sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

            speed_ = 0.00003f * (1 + rng::choice<3>(rng::utility_state));
        }
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
        alerted_ = true;
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



void GenericBird::generate(Platform& pfrm, App& app)
{
    for (auto it = app.birds().begin(); it not_eq app.birds().end();) {
        if ((*it)->island(app) == opponent_island(app)) {
            it = app.birds().erase(it);
        } else {
            ++it;
        }
    }

    static const int max_birds = 6;
    int remaining_birds = max_birds;
    if (rng::choice<4>(rng::utility_state) > 0) {
        int used = rng::choice<4>(rng::utility_state);
        remaining_birds -= used;
        GenericBird::spawn(pfrm, app, *app.opponent_island(), used);
    }
    if (rng::choice<4>(rng::utility_state) > 0) {
        GenericBird::spawn(pfrm,
                           app,
                           app.player_island(),
                           rng::choice(remaining_birds, rng::utility_state));
    }
}



void GenericBird::spawn(Platform& pfrm, App& app, Island& island, int count)
{
    Buffer<u8, 10> used;

    for (auto& bird : app.birds()) {
        if (bird->island(app) == &island) {
            used.push_back(bird->coordinate().x);
        }
    }

    for (int i = 0; i < count; ++i) {
        u8 tries = 40;
        u8 column = 0;
        bool retry = false;
        while (tries--) {
            column = rng::choice(island.terrain().size(), rng::utility_state);
            for (auto& u : used) {
                if (column == u) {
                    retry = true;
                    break;
                }
            }
            if (not retry) {
                used.push_back(column);
                break;
            }
        }

        if (retry) {
            continue;
        }

        for (u8 y = 0; y < 15; ++y) {
            if (y == 14 or island.rooms_plot().get(column, y + 1)) {

                auto pos = Vec2<u8>{column, y};

                if (auto dt = pfrm.make_dynamic_texture()) {
                    bool near = &island == &app.player_island();
                    app.birds().push(
                        app.alloc_entity<GenericBird>(pfrm, *dt, pos, near));
                    break;
                }
            }
        }
    }
}



} // namespace skyland
