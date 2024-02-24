////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "weatherEngine.hpp"
#include "platform/platform.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/worldScene.hpp"
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



class SetWeatherScene : public ActiveWorldScene
{
public:
    SetWeatherScene()
    {
    }


    void enter(Scene& prev)
    {
        ActiveWorldScene::enter(prev);

        const auto st = calc_screen_tiles();

        PLATFORM.set_tile(Layer::overlay, 0, st.y - 1, 398);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 1, 399);

        text_.emplace(OverlayCoord{2, u8(st.y - 1)});
        set_str();
    }


    void exit(Scene& next)
    {
        ActiveWorldScene::exit(next);

        text_.reset();
        PLATFORM.fill_overlay(0);
    }


    void set_str()
    {
        text_->assign(" ");
        text_->append(
            loadstr((SystemString)((int)SystemString::weather_clear + index_))
                ->c_str());
    }


    ScenePtr<Scene> update(Time delta)
    {
        ActiveWorldScene::update(delta);

        if (APP.player().key_down(Key::down)) {
            if (index_ == 5) {
                index_ = 0;
            } else {
                ++index_;
            }
            set_str();
        }

        if (APP.player().key_down(Key::up)) {
            if (index_ == 0) {
                index_ = 5;
            } else {
                --index_;
            }
            set_str();
        }

        if (APP.player().key_down(Key::action_1)) {

            time_stream::event::WeatherChanged e;

            e.prev_weather_ = APP.environment().id();
            APP.time_stream().push(APP.level_timer(), e);

            environment_init(index_ + 1);

            PLATFORM.screen().set_shader(APP.environment().shader());
            PLATFORM.screen().set_shader_argument(0);

            if (not PLATFORM.speaker().is_music_playing(
                    APP.environment().music())) {
                PLATFORM.speaker().play_music(APP.environment().music(), 0);
            }

            PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            PLATFORM.sleep(4);
            PLATFORM.screen().schedule_fade(0.f);

            PLATFORM.speaker().play_sound("bell", 3);

            return scene_pool::alloc<ReadyScene>();
        }

        if (APP.player().key_down(Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }

        return null_scene();
    }

    Optional<Text> text_;
    int index_ = 0;
};



ScenePtr<Scene> WeatherEngine::select_impl(const RoomCoord& cursor)
{
    return scene_pool::alloc<SetWeatherScene>();
}



void WeatherEngine::update(Time delta)
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
