#include "environment.hpp"
#include "skyland/scene/worldScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::weather
{



ColorConstant ClearSkies::fadein_colorize_tone() const
{
    return custom_color(0x071b2d);
}



void ClearSkies::update(Time delta)
{
    timer_ -= delta;
    if (timer_ <= 0) {

        timer_ += timer_ =
            seconds(3) + seconds(rng::choice<6>(rng::utility_state));

        switch (rng::choice<3>(rng::utility_state)) {
        case 0:
            if (not APP.birds().empty()) {
                PLATFORM.speaker().play_sound("seagull_1.raw", 0);
            }
            break;

        case 1:
            if (not APP.birds().empty()) {
                PLATFORM.speaker().play_sound("seagull_2.raw", 0);
            }
            break;

        case 2:
            break;
        }
    }
}



EnvironmentId ClearSkies::id() const
{
    return 1;
}



void ClearSkies::display()
{
    bool disable_lensflare = false;

    const auto& view_pos = PLATFORM.screen().get_view().int_center();

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
    PLATFORM.screen().draw(spr);


    if (auto ws = APP.scene().cast_world_scene()) {
        // auto cursor = ws->is_far_camera() ? globals().far_cursor_loc_
        //     : globals().near_cursor_loc_;

        auto isle = not ws->is_far_camera() ? &APP.player_island()
                                            : APP.opponent_island();

        HitBox sun_hb;
        Vec2<Fixnum> sun_pos = {x, y};
        sun_hb.position_ = &sun_pos;
        sun_hb.dimension_.size_ = {16, 16};

        if (isle) {
            for (u32 x = 0; x < isle->terrain().size(); ++x) {
                for (int y = 4; y < 7; ++y) {
                    if (isle->rooms_plot().get(x, y)) {
                        disable_lensflare = true;
                        goto LENSFLARE_CHECK_DONE;
                    }
                }
            }
        }
    }
LENSFLARE_CHECK_DONE:

    bool hardware_supports_spr_overlapping = false;
    PLATFORM_EXTENSION(sprite_overlapping_supported,
                       hardware_supports_spr_overlapping);

    if (hardware_supports_spr_overlapping) {
        disable_lensflare = false;
    }

    struct LensFlare
    {
        Fixnum frac_;
        u16 tidx_;
    };

    spr.set_alpha(Sprite::Alpha::translucent);
    spr.set_tidx_16x16(78, 0);
    PLATFORM.screen().draw(spr);

    Vec2<Fixnum> scrn_center;
    scrn_center.x = Fixnum::from_integer(PLATFORM.screen().size().x / 2) +
                    Fixnum::from_integer(view_pos.x);
    scrn_center.y = Fixnum::from_integer(280 + PLATFORM.screen().size().y / 2) +
                    Fixnum::from_integer(view_pos.y);

    const auto dx = scrn_center.x - x;
    const auto dy = scrn_center.y - y;

    Fixnum dist = y - scrn_center.y;

    struct Flare
    {
        u16 spr_;
        Fixnum offset_;
    };

    Flare flares[] = {
        {1, 0.1_fixed},
        {2, 0.2_fixed},
        {3, 0.25_fixed},
    };

    auto amt =
        clamp((int)(255.0_fixed * ((165.0_fixed - dist) * Fixnum(1.f / 19.f)))
                  .as_integer(),
              0,
              255);

    if (amt > 210) {
        return;
    }
    spr.set_priority(1);

    spr.set_mix({custom_color(0x63b5e7), (u8)amt});

    if (disable_lensflare) {
        return;
    }

    for (auto& f : flares) {
        spr.set_position({x + dx * f.offset_, y - dy * f.offset_});
        spr.set_tidx_16x16(116, f.spr_);
        PLATFORM.screen().draw(spr);
    }

    Flare flares2[] = {
        {59, 0.32_fixed},
    };

    spr.set_size(Sprite::Size::w32_h32);
    for (auto& f : flares2) {
        spr.set_position(
            {x + dx * f.offset_ - 8.0_fixed, y - dy * f.offset_ - 8.0_fixed});
        spr.set_texture_index(f.spr_);
        PLATFORM.screen().draw(spr);
    }


    return;
}



} // namespace skyland::weather
