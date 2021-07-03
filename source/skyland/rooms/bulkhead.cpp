#include "bulkhead.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Bulkhead::Bulkhead(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(100))
{
}



void Bulkhead::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Bulkhead::render_interior(Platform& pfrm, Layer layer)
{
    if (open_) {
        pfrm.set_tile(
            layer, position().x, position().y, InteriorTile::bulkhead_open_1);
        pfrm.set_tile(layer,
                      position().x,
                      position().y + 1,
                      InteriorTile::plain_floor);
    } else {
        pfrm.set_tile(
            layer, position().x, position().y, InteriorTile::bulkhead_closed_1);
        pfrm.set_tile(layer,
                      position().x,
                      position().y + 1,
                      InteriorTile::bulkhead_closed_2);
    }

    interior_visible_ = true;
}



void Bulkhead::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_plain_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_plain_2);

    interior_visible_ = false;
}



ScenePtr<Scene> Bulkhead::select(Platform& pfrm, App& app)
{
    if (length(characters())) {
        return Room::select(pfrm, app);
    }

    if (not open_) {
        open_ = true;
        render_interior(pfrm, parent()->layer());
    } else {
        open_ = false;

        render_interior(pfrm, parent()->layer());

        parent()->on_layout_changed({position().x, u8(position().y + 1)});
    }
    return null_scene();
}



} // namespace skyland
