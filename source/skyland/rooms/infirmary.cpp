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


#include "infirmary.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Infirmary::Infirmary(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



static const auto infirmary_heal_interval = milliseconds(1000);



Time Infirmary::heal_interval() const
{
    Time interval = infirmary_heal_interval;
    if (amplify_) {
        interval /= 2;
    }
    return interval;
}



void Infirmary::on_powerchange()
{
    heal_timer_ = heal_interval();
}



bool Infirmary::allows_powerdown()
{
    return true;
}



void Infirmary::update(Time delta)
{
    Room::update(delta);

    if (is_powered_down()) {
        return;
    }

    // Optimization: room has no inhabitants, don't schedule for updates.
    if (characters().empty()) {
        return;
    }

    Room::ready();

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &parent()->owner() and
            character->state() not_eq BasicCharacter::State::fighting) {
            ++characters_healing;
        }
    }

    if (characters_healing) {
        heal_timer_ += delta;
        if (heal_timer_ > heal_interval()) {
            heal_timer_ -= heal_interval();
            int distribute_health = 20;
            distribute_health /= characters_healing;
            for (auto& character : characters()) {
                if (character->owner() == &parent()->owner() and
                    character->state() not_eq BasicCharacter::State::fighting) {
                    character->heal(distribute_health);
                }
            }
        }
    }
}



void Infirmary::amplify(bool enabled)
{
    amplify_ = enabled;
}



void Infirmary::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_infirmary)->c_str();
}


void Infirmary::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::infirmary_1;
    buffer[position().x][position().y + 1] = InteriorTile::infirmary_2;
    buffer[position().x + 1][position().y] = InteriorTile::infirmary_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::plain_floor;
}


void Infirmary::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



void Infirmary::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



} // namespace skyland
