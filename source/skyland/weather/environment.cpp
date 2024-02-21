#include "environment.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/blizzard.hpp"
#include "skyland/weather/dustStorm.hpp"
#include "skyland/weather/slightlyOvercast.hpp"
#include "skyland/weather/storm.hpp"
#include "skyland/weather/typhoon.hpp"



namespace skyland::weather
{



Environment::Environment()
{
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
    }

    if (APP.environment().id() not_eq type) {
        LOGIC_ERROR();
    }
}



} // namespace skyland
