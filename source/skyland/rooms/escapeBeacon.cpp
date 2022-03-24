#include "escapeBeacon.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



EscapeBeacon::EscapeBeacon(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void EscapeBeacon::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_escape_beacon)->c_str();
}



void EscapeBeacon::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (activated_) {

        Room::ready();

        if (timer_ >= 0) {
            timer_ -= delta;

            if (timer_ <= 0) {

                if (parent() == &player_island(app)) {
                    app.exit_condition() = App::ExitCondition::player_fled;
                } else {
                    app.exit_condition() = App::ExitCondition::opponent_fled;
                }

                activated_ = false;
                timer_ = 0;
            }
        }
    }
}



ScenePtr<Scene>
EscapeBeacon::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    const bool was_activated = activated_;

    activated_ = true;

    if (not was_activated) {
        parent()->repaint(pfrm, app);
        timer_ = seconds(60);
    }

    Room::ready();

    return null_scene();
}



void EscapeBeacon::render_interior(App& app, u8 buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    if (activated_) {
        buffer[x][y] = InteriorTile::escape_beacon_on_1;
        buffer[x][y + 1] = InteriorTile::escape_beacon_on_2;
    } else {
        buffer[x][y] = InteriorTile::escape_beacon_off_1;
        buffer[x][y + 1] = InteriorTile::escape_beacon_off_2;
    }


    buffer[x][y + 2] = InteriorTile::escape_beacon_base;
}



void EscapeBeacon::render_exterior(App& app, u8 buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    if (activated_) {
        buffer[x][y] = Tile::escape_beacon_on_1;
        buffer[x][y + 1] = Tile::escape_beacon_on_2;
    } else {
        buffer[x][y] = Tile::escape_beacon_off_1;
        buffer[x][y + 1] = Tile::escape_beacon_off_2;
    }


    buffer[x][y + 2] = InteriorTile::escape_beacon_base;
}



}
