#include "infirmary.hpp"
#include "skyland/tile.hpp"
#include "skyland/island.hpp"



namespace skyland {



Infirmary::Infirmary(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(60))
{
}


void Infirmary::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &parent()->owner() and
            character->state() == BasicCharacter::State::moving_or_idle) {
            ++characters_healing;
        }
    }

    if (characters_healing) {
        heal_timer_ += delta;
        if (heal_timer_ > milliseconds(1000)) {
            heal_timer_ = 0;
            int distribute_health = 20;
            distribute_health /= characters_healing;
            for (auto& character : characters()) {
                if (character->owner() == &parent()->owner()  and
                    character->state() == BasicCharacter::State::moving_or_idle) {
                    character->heal(distribute_health);
                }
            }
        }
    }
}


void Infirmary::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::infirmary_1);
    pfrm.set_tile(
        layer, position().x, position().y + 1, InteriorTile::infirmary_2);
    pfrm.set_tile(
        layer, position().x + 1, position().y, InteriorTile::infirmary_3);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, InteriorTile::infirmary_4);
}


void Infirmary::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_window_2);
    pfrm.set_tile(layer, position().x + 1, position().y, Tile::wall_plain_1);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, Tile::wall_plain_2);
}



} // namespace skyland
