#include "transporter.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Transporter::Transporter(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(100))
{
}



void Transporter::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (recharge_ > 0) {
        recharge_ -= delta;

        if (recharge_ < 0) {
            recharge_ = 0;
        }
    }
}



void Transporter::recover_character(App& app, const Vec2<u8>& position)
{
    recharge_ = recharge_time;

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    if (auto room = island->get_room(position)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {
            if ((*it)->owner() not_eq &island->owner() and
                (*it)->grid_position() == position) {
                auto unlinked = std::move(*it);
                room->characters().erase(it);

                // If the character was in the process of moving, we need to
                // detach its path, as we are transporting the character back to
                // our island, where the path would make no sense.
                unlinked->drop_movement_path();

                // Again, the character is warping to a new location, let's
                // update its position.
                unlinked->set_grid_position({
                        this->position().x,
                        u8(this->position().y + 1)
                    });

                unlinked->set_parent(parent());

                characters().push(std::move(unlinked));

                return;
            } else {
                ++it;
            }
        }
    }
}



void Transporter::random_transport_occupant(Platform& pfrm, App& app)
{
    recharge_ = recharge_time;

    auto chr = characters().begin();

    auto island = other_island(app);
    if (island == nullptr) {
        return;
    }

    bool matrix[16][16];
    island->plot_walkable_zones(matrix);

    Buffer<Vec2<u8>, 32> slots;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y]) {
                slots.push_back({x, y});
            }
        }
    }

    std::optional<Vec2<u8>> dest;

    while (not slots.empty()) {
        const auto index = rng::choice(slots.size(), rng::critical_state);
        auto slot = &slots[index];

        if (island->character_at_location(*slot)) {
            slots.erase(slot);
        } else {
            dest = *slot;
            break;
        }
    }

    if (not dest) {
        // TODO: raise an alert, that there is no room to teleport into.
        return;
    }

    if (auto room = island->get_room(*dest)) {
        (*chr)->set_grid_position(*dest);
        (*chr)->set_parent(island);
        room->characters().push(std::move(*chr));
    } else {
        return;
    }

    characters().erase(characters().begin());
}



ScenePtr<Scene> Transporter::select(Platform& pfrm, App& app)
{
    if (auto new_scene = Room::select(pfrm, app)) {
        return new_scene;
    }

    if (recharge_) {
        return null_scene();
    } else if (length(characters())) {

        auto chr = characters().begin();
        if ((*chr)->has_movement_path()) {
            return null_scene();
        }

        if (parent()->has_radar()) {
            // TODO: When the player has a radar, should be able to select which
            // room that they want to transport the character into.
        } else {
            random_transport_occupant(pfrm, app);
        }

        return null_scene();
    } else {
        return null_scene();
    }
}


void Transporter::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(
        layer, position().x, position().y, InteriorTile::transporter_1);
    pfrm.set_tile(
        layer, position().x, position().y + 1, InteriorTile::transporter_2);
}



void Transporter::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_window_2);
}



} // namespace skyland
