#include "decimator.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Decimator::Decimator(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



ScenePtr<Scene> Decimator::select(Platform& pfrm, App& app)
{
    if (auto scene = Room::select(pfrm, app)) {
        return scene;
    }

    return null_scene();
}



void Decimator::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    bool has_pilot = false;
    for (auto& chr : characters()) {
        if (chr->parent() == parent()) {
            has_pilot = true;
        }
    }

    if (has_pilot and reload_ > 0) {
        reload_ -= delta;
    } else if (has_pilot) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island(app);

        if (island and not island->is_destroyed()) {
            app.camera().shake(4);

            auto start = center();

            // This just makes it a bit less likely for cannonballs to
            // run into the player's own buildings, especially around
            // corners.
            if (island == &app.player_island()) {
                start.x -= 18;
            } else {
                start.x += 18;
            }

            auto target = center();
            if (parent() == &app.player_island()) {
                target.x += 100.f;
            } else {
                target.x -= 100.f;
            }


            auto c = alloc_entity<DecimatorBurst>(
                start, target, parent(), position());

            if (c) {
                parent()->projectiles().push(std::move(c));
            }

            if (counter_ < 6) {
                ++counter_;
                reload_ = milliseconds(200);
            } else {
                reload_ = reload_time;
                counter_ = 0;
            }
        }
    }
}



void Decimator::plot_walkable_zones(App& app, bool matrix[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        matrix[pos.x][pos.y + 1] = true;
    } else {
        matrix[pos.x + 1][pos.y + 1] = true;
    }
}



void Decimator::render_interior(App& app, u8 buffer[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x][pos.y] = InteriorTile::decimator_int;
    } else {
        buffer[pos.x][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_int;
    }

}



void Decimator::render_exterior(App& app, u8 buffer[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        buffer[pos.x + 1][pos.y] = Tile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x][pos.y] = Tile::armored_wall_1;
        buffer[pos.x][pos.y + 1] = Tile::wall_plain_2;
    } else {
        buffer[pos.x][pos.y] = Tile::decimator_1;
        buffer[pos.x][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x + 1][pos.y] = Tile::armored_wall_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::wall_plain_2;
    }
}



} // namespace skyland
