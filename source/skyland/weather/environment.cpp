#include "environment.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/blizzard.hpp"
#include "skyland/weather/dustStorm.hpp"
#include "skyland/weather/night.hpp"
#include "skyland/weather/slightlyOvercast.hpp"
#include "skyland/weather/solarStorm.hpp"
#include "skyland/weather/storm.hpp"
#include "skyland/weather/typhoon.hpp"



namespace skyland::weather
{



Environment::Environment()
{
}



bool Environment::is_night() const
{
    return id() == Night::id_;
}



Conf::String Environment::read_conf(const char* field) const
{
    auto fd = PLATFORM.load_file("scripts/data", "environment.ini");
    if (not fd.second) {
        PLATFORM.fatal("missing env config file!");
    }

    Conf c;
    auto v = c.get(fd.first, format("environment_%", id()).c_str(), field);

    return std::move(*std::get_if<Conf::String>(&v));
}



Conf::String Environment::music() const
{
    auto str = read_conf("music");
    if (*str == "[none]") {
        *str = PLATFORM.speaker().current_music();
    }

    return str;
}



Conf::String Environment::ambiance() const
{
    return read_conf("ambiance");
}



void Environment::render_glow_effect(const Vec2<Fixnum>& pos,
                                     int radius,
                                     ColorConstant color,
                                     u8 intensity)
{
    if (state_bit_load(StateBit::lighting_enabled)) {
        PLATFORM_EXTENSION(draw_point_light,
                           pos.x,
                           pos.y,
                           radius,
                           color,
                           intensity * modulate_glow_amount());
    }
}



Float Environment::modulate_glow_amount()
{
    return 1.f;
}



} // namespace skyland::weather



namespace skyland
{



void environment_init(EnvironmentId type)
{
    switch (type) {
    case 1:
        APP.swap_environment<weather::ClearSkies>();
        break;

    case 2:
        APP.swap_environment<weather::SlightlyOvercast>();
        break;

    case 3:
        APP.swap_environment<weather::Storm>();
        break;

    case 4:
        APP.swap_environment<weather::Blizzard>();
        break;

    case 5:
        APP.swap_environment<weather::Typhoon>();
        break;

    case 6:
        APP.swap_environment<weather::DustStorm>();
        break;

    case 7:
        APP.swap_environment<weather::Night>();
        break;

    case 8:
        APP.swap_environment<weather::SolarStorm>();
        break;
    }

    if (APP.environment().id() not_eq type) {
        LOGIC_ERROR();
    }
}



void environment_apply()
{
    PLATFORM.screen().set_shader(APP.environment().shader());
    PLATFORM.screen().set_shader_argument(0);

    if (not PLATFORM.speaker().is_music_playing(
            APP.environment().music()->c_str())) {
        PLATFORM.speaker().stream_music(APP.environment().music()->c_str(), 0);
    }


    APP.player_island().schedule_repaint();

    if (APP.opponent_island()) {
        APP.opponent_island()->schedule_repaint();
    }
}



} // namespace skyland
