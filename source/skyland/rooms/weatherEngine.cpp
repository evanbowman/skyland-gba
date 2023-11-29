////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "weatherEngine.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void WeatherEngine::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_weather_engine)->c_str();
}



WeatherEngine::WeatherEngine(Island* parent,
                             const RoomCoord& position,
                             const char* n)
    : Room(parent, n, position)
{
}



void environment_init(int type);



ScenePtr<Scene> WeatherEngine::select(const RoomCoord& cursor)
{
    time_stream::event::WeatherChanged e;

    if (APP.environment().is_overcast()) {
        e.prev_weather_ = 3; // FIXME!
        environment_init(1);
    } else {
        e.prev_weather_ = 1;
        environment_init(3);
    }
    APP.time_stream().push(APP.level_timer(), e);

    PLATFORM.screen().set_shader(APP.environment().shader());
    PLATFORM.screen().set_shader_argument(0);

    if (not PLATFORM.speaker().is_music_playing(APP.environment().music())) {
        PLATFORM.speaker().play_music(APP.environment().music(), 0);
    }

    PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();
    PLATFORM.sleep(4);
    PLATFORM.screen().schedule_fade(0.f);

    PLATFORM.speaker().play_sound("bell", 3);

    return null_scene();
}



void WeatherEngine::update(Microseconds delta)
{
    Room::update(delta);
}



void WeatherEngine::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::weather_engine_1;
    buffer[position().x][position().y + 1] = InteriorTile::weather_engine_2;
}



void WeatherEngine::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::weather_engine_1;
    buffer[position().x][position().y + 1] = Tile::weather_engine_2;
}



} // namespace skyland
