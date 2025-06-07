////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "explosion.hpp"
#include "number/random.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



TeenyExplosion::TeenyExplosion(const Vec2<Fixnum>& position) : Entity({{}, {}})
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w8_h8);
    sprite_.set_tidx_8x8(start_index, 0);
    sprite_.set_origin({4, 4});
}



void TeenyExplosion::update(Time delta)
{
    timer_ += delta * 2;

    if (timer_ > milliseconds(55)) {
        timer_ = 0;

        if (anim_index_ < 5) {
            sprite_.set_tidx_8x8(start_index, anim_index_++);
        } else {
            kill();
        }
    }
}



Explosion::Explosion(const Vec2<Fixnum>& position, int priority)
    : Entity({{}, {}})
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(start_index);
    sprite_.set_origin({8, 8});
    sprite_.set_priority(priority);


    bool is_offscreen =
        (position.x.as_integer() <
         PLATFORM.screen().get_view().int_center().x + 8 - (1 * 16) / 2) or
        (position.x.as_integer() - (1 * 16) / 2 >
         (int)(PLATFORM.screen().get_view().int_center().x +
               PLATFORM.screen().size().x));

    if (is_offscreen) {
        kill();
        return;
    }
}



void Explosion::update(Time delta)
{
    timer_ += delta * 2;

    if (timer_ > milliseconds(55)) {
        timer_ = 0;

        auto index = sprite_.get_texture_index();
        if (index < start_index + 5) {
            sprite_.set_texture_index(index + 1);
        } else {
            kill();
        }
    }
}



static const int exp_flash_index_seq[] = {52, 53, 54, 55, 56, 50, 24};



ExpFlash::ExpFlash(const Vec2<Fixnum>& position) : Entity({{}, {}})
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w32_h32);
    sprite_.set_texture_index(exp_flash_index_seq[anim_index_]);
    sprite_.set_origin({16, 16});

    sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state),
                      (bool)rng::choice<2>(rng::utility_state)});
}



void ExpFlash::update(Time delta)
{
    timer_ += delta * 2;

    if (timer_ > milliseconds(75)) {
        timer_ = 0;

        if (anim_index_ < 6) {
            sprite_.set_texture_index(exp_flash_index_seq[++anim_index_]);
        } else {
            kill();
        }
    }
}



void medium_explosion(const Vec2<Fixnum>& position)
{
    auto first = APP.alloc_entity<Explosion>(
        rng::sample<18>(position, rng::utility_state));

    if (first) {
        APP.effects().push(std::move(first));
    }

    APP.on_timeout(milliseconds(60), [pos = position]() {
        APP.rumble().activate(milliseconds(200));

        auto second = APP.alloc_entity<Explosion>(
            rng::sample<18>(pos, rng::utility_state));
        if (second) {
            APP.effects().push(std::move(second));
        }

        APP.on_timeout(milliseconds(120), [pos = pos]() {
            auto third = APP.alloc_entity<Explosion>(
                rng::sample<18>(pos, rng::utility_state));

            if (third) {
                APP.effects().push(std::move(third));
            }
        });
    });
}



void medium_explosion_inv(const Vec2<Fixnum>& position)
{
    auto first = APP.alloc_entity<Explosion>(
        rng::sample<18>(position, rng::utility_state));

    if (first) {
        first->seek_end();
        APP.effects().push(std::move(first));
    }

    APP.on_timeout(milliseconds(60), [pos = position]() {
        APP.rumble().activate(milliseconds(200));
        auto exp = APP.alloc_entity<Explosion>(
            rng::sample<18>(pos, rng::utility_state));

        if (exp) {
            exp->seek_end();
            APP.effects().push(std::move(exp));
        }


        APP.on_timeout(milliseconds(120), [pos = pos]() {
            auto exp = APP.alloc_entity<Explosion>(
                rng::sample<18>(pos, rng::utility_state));

            if (exp) {
                exp->seek_end();
                APP.effects().push(std::move(exp));
            }
        });
    });
}



void big_explosion(const Vec2<Fixnum>& position, const BigExplosionConfig& conf)
{
    if (conf.centerflash_) {
        if (auto ent = APP.alloc_entity<ExpFlash>(position)) {
            APP.effects().push(std::move(ent));
        }
    }

    int init_iters = conf.centerflash_ ? 3 : 4;

    for (int i = 0; i < init_iters; ++i) {
        auto ent = APP.alloc_entity<Explosion>(
            rng::sample<18>(position, rng::utility_state), conf.draw_priority_);
        if (ent) {
            APP.effects().push(std::move(ent));
        }
    }

    int p = conf.draw_priority_;
    auto ipos = ivec(position);

    APP.on_timeout(milliseconds(90), [pos = ipos, p]() {
        APP.rumble().activate(milliseconds(390));

        for (int i = 0; i < 3; ++i) {
            auto ent = APP.alloc_entity<Explosion>(
                rng::sample<32>(pos.cast<Fixnum>(), rng::utility_state), p);
            if (ent) {
                APP.effects().push(std::move(ent));
            }
        }
        APP.on_timeout(milliseconds(90), [pos, p]() {
            for (int i = 0; i < 2; ++i) {
                auto ent = APP.alloc_entity<Explosion>(
                    rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state), p);
                if (ent) {
                    APP.effects().push(std::move(ent));
                }
            }
            APP.on_timeout(milliseconds(90), [pos, p]() {
                for (int i = 0; i < 1; ++i) {
                    auto ent = APP.alloc_entity<Explosion>(
                        rng::sample<48>(pos.cast<Fixnum>(), rng::utility_state),
                        p);
                    if (ent) {
                        APP.effects().push(std::move(ent));
                    }
                }
            });
        });
    });

    APP.camera()->shake(18);
}



void big_explosion_inv(const Vec2<Fixnum>& position)
{
    for (int i = 0; i < 1; ++i) {
        auto exp = APP.alloc_entity<Explosion>(
            rng::sample<48>(position, rng::utility_state));

        if (exp) {
            exp->seek_end();
            APP.effects().push(std::move(exp));
        }
    }

    APP.on_timeout(milliseconds(90), [pos = position]() {
        APP.rumble().activate(milliseconds(390));

        for (int i = 0; i < 2; ++i) {
            auto exp = APP.alloc_entity<Explosion>(
                rng::sample<48>(pos, rng::utility_state));
            if (exp) {
                exp->seek_end();
                APP.effects().push(std::move(exp));
            }
        }
        APP.on_timeout(milliseconds(90), [pos]() {
            for (int i = 0; i < 3; ++i) {
                auto exp = APP.alloc_entity<Explosion>(
                    rng::sample<32>(pos, rng::utility_state));
                if (exp) {
                    exp->seek_end();
                    APP.effects().push(std::move(exp));
                }
            }
            APP.on_timeout(milliseconds(90), [pos]() {
                for (int i = 0; i < 4; ++i) {
                    auto exp = APP.alloc_entity<Explosion>(
                        rng::sample<18>(pos, rng::utility_state));
                    if (exp) {
                        exp->seek_end();
                        APP.effects().push(std::move(exp));
                    }
                }
            });
        });
    });

    APP.camera()->shake(18);
}



} // namespace skyland
