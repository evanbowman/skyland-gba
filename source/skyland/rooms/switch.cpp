#include "switch.hpp"
#include "skyland/tile.hpp"
#include "skyland/island.hpp"



namespace skyland {



void Switch::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_hull)->c_str();
}



ScenePtr<Scene> Switch::select(Platform& pfrm,
                               App& app,
                               const Vec2<u8>& cursor)
{
    if (Vec2<u8>{u8(cursor.x - 1), cursor.y} == position()) {
        on_ = not on_;
        parent()->repaint(pfrm, app);
    } else {
        // TODO...
    }

    return null_scene();
}




Switch::Switch(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
}



void Switch::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = InteriorTile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = InteriorTile::switch_off;
    }
}



void Switch::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = Tile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = Tile::switch_off;
    }
}



} // namespace skyland
