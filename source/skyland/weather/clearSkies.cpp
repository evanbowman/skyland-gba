#include "environment.hpp"
#include "skyland/skyland.hpp"



namespace skyland::weather
{



void ClearSkies::update(Platform& pfrm, App& app, Microseconds delta)
{
    timer_ -= delta;
    if (timer_ <= 0) {

        timer_ += timer_ =
            seconds(3) + seconds(rng::choice<6>(rng::utility_state));

        switch (rng::choice<3>(rng::utility_state)) {
        case 0:
            if (not app.birds().empty()) {
                pfrm.speaker().play_sound("seagull_1", 0);
            }
            break;

        case 1:
            if (not app.birds().empty()) {
                pfrm.speaker().play_sound("seagull_2", 0);
            }
            break;

        case 2:
            break;
        }
    }
}



void ClearSkies::display(Platform& pfrm, App& app)
{
    // return; // eh, I'm undecided about the sun effect

    const auto& view_pos = pfrm.screen().get_view().get_center();

    Sprite spr;
    spr.set_priority(3);
    spr.set_size(Sprite::Size::w16_h16);
    spr.set_tidx_16x16(116, 0);
    Fixnum x = 60.0_fixed;
    x += Fixnum::from_integer(view_pos.x) * 0.75_fixed;
    Fixnum y = 487.0_fixed;
    y += Fixnum::from_integer(view_pos.y) * 0.65_fixed;

    if (y > 474.0_fixed) {
        return;
    }

    spr.set_position({x, y});
    pfrm.screen().draw(spr);
    struct LensFlare {
        Fixnum frac_;
        u16 tidx_;
    };

    spr.set_alpha(Sprite::Alpha::translucent);
    spr.set_tidx_16x16(78, 0);
    pfrm.screen().draw(spr);

    Vec2<Fixnum> scrn_center;
    scrn_center.x = Fixnum::from_integer(pfrm.screen().size().x / 2) +
        Fixnum::from_integer(view_pos.x);
    scrn_center.y = Fixnum::from_integer(280 + pfrm.screen().size().y / 2) +
        Fixnum::from_integer(view_pos.y);

    const auto dx = scrn_center.x - x;
    const auto dy = scrn_center.y - y;

    Fixnum dist = y - scrn_center.y;

    struct Flare
    {
        u16 spr_;
        Fixnum offset_;
    };

    Flare flares[] = {{1, 0.1_fixed},
                      {2, 0.2_fixed},
                      {3, 0.25_fixed},};

    auto amt = clamp((int)(255.0_fixed
                       * ((165.0_fixed - dist)
                       * Fixnum(1.f / 19.f))).as_integer(),
                     0, 255);

    if (amt > 210) {
        return;
    }
    spr.set_priority(1);

    spr.set_mix({custom_color(0x63b5e7), (u8)amt});

    for (auto& f : flares) {
        spr.set_position({x + dx * f.offset_, y - dy * f.offset_});
        spr.set_tidx_16x16(116, f.spr_);
        pfrm.screen().draw(spr);
    }

    Flare flares2[] = {{59, 0.32_fixed},};

    spr.set_size(Sprite::Size::w32_h32);
    for (auto& f : flares2) {
        spr.set_position({x + dx * f.offset_ - 8.0_fixed,
                          y - dy * f.offset_ - 8.0_fixed});
        spr.set_texture_index(f.spr_);
        pfrm.screen().draw(spr);
    }


    return;
}



}
