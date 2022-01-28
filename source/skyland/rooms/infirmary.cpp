#include "infirmary.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Infirmary::Infirmary(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}


void Infirmary::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &parent()->owner() and
            character->state() not_eq BasicCharacter::State::fighting) {
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
                if (character->owner() == &parent()->owner() and
                    character->state() not_eq BasicCharacter::State::fighting) {
                    character->heal(pfrm, app, distribute_health);
                }
            }
        }
    }
}


void Infirmary::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::infirmary_1;
    buffer[position().x][position().y + 1] = InteriorTile::infirmary_2;
    buffer[position().x + 1][position().y] = InteriorTile::infirmary_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::infirmary_4;
}


void Infirmary::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



} // namespace skyland
