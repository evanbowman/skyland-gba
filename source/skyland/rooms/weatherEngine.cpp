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
#include "skyland/tile.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void WeatherEngine::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_weather_engine)->c_str();
}



WeatherEngine::WeatherEngine(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void environment_init(App & app, int type);



ScenePtr<Scene> WeatherEngine::select(Platform& pfrm,
                                      App& app,
                                      const RoomCoord& cursor)
{
    environment_init(app, 1);
    pfrm.screen().set_shader(app.environment().shader(app));
    pfrm.screen().set_shader_argument(0);

    if (not pfrm.speaker().is_music_playing(app.environment().music())) {
        pfrm.speaker().play_music(app.environment().music(), 0);
    }

    pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);
    pfrm.screen().clear();
    pfrm.screen().display();
    pfrm.sleep(4);
    pfrm.screen().schedule_fade(0.f);

    pfrm.speaker().play_sound("button_wooden", 3);

    return null_scene();
}



void WeatherEngine::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void WeatherEngine::render_interior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::weather_engine_1;
    buffer[position().x][position().y + 1] = InteriorTile::weather_engine_2;
}



void WeatherEngine::render_exterior(App& app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::weather_engine_1;
    buffer[position().x][position().y + 1] = Tile::weather_engine_2;
}



} // namespace skyland
