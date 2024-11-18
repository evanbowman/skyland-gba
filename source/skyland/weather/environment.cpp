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



} // namespace skyland
