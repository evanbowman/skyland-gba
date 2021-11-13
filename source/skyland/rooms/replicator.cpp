#include "replicator.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/replicatorSelectionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Replicator::Replicator(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(full_health()))
{
}



void Replicator::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



bool Replicator::create_replicant(Platform& pfrm, App& app)
{
    int character_count = 0;

    BasicCharacter* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            character_count++;
            found_chr = chr.get();
        }
    }

    if (character_count == 1) {
        const auto chr_pos = found_chr->grid_position();
        const auto chr_health = found_chr->health();

        const Health replicant_health = chr_health * 0.75f;

        const auto dst = [&] {
            if (chr_pos.x ==
                position().x) { // We have two slots where we can place
                                // the replicant.
                return Vec2<u8>{u8(chr_pos.x + 1), chr_pos.y};
            } else {
                return Vec2<u8>{u8(chr_pos.x - 1), chr_pos.y};
            }
        }();

        auto chr = app.alloc_entity<BasicCharacter>(
            pfrm, parent(), found_chr->owner(), dst, true);

        if (chr) {
            network::packet::ReplicantCreated packet;
            packet.src_x_ = dst.x;
            packet.src_y_ = dst.y;
            packet.health_ = replicant_health;
            network::transmit(pfrm, packet);


            chr->apply_damage(255 - replicant_health);
            chr->transported();
            characters().push(std::move(chr));
        }

    } else {
        return false;
    }

    return true;
}



ScenePtr<Scene> Replicator::select(Platform& pfrm, App& app)
{
    if (auto next = Room::select(pfrm, app)) {
        return next;
    }

    int character_count = 0;

    BasicCharacter* found_chr = nullptr;

    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            character_count++;
            found_chr = chr.get();
        }
    }

    if (found_chr) {
        return scene_pool::alloc<ReplicatorSelectionScene>();
    }

    return null_scene();
}



void Replicator::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::empty);
    pfrm.set_tile(layer, position().x + 1, position().y, InteriorTile::empty);

    pfrm.set_tile(
        layer, position().x, position().y + 1, InteriorTile::replicator_1);
    pfrm.set_tile(
        layer, position().x, position().y + 2, InteriorTile::replicator_3);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, InteriorTile::replicator_2);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 2, InteriorTile::replicator_4);
    pfrm.set_tile(
        layer, position().x, position().y + 3, InteriorTile::plain_floor);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 3, InteriorTile::plain_floor);
}



void Replicator::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(
        layer, position().x, position().y + 1, Tile::wall_window_middle_2);
    pfrm.set_tile(
        layer, position().x, position().y + 2, Tile::wall_window_middle_1);
    pfrm.set_tile(layer, position().x, position().y + 3, Tile::wall_window_2);

    pfrm.set_tile(layer, position().x + 1, position().y, Tile::wall_window_1);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, Tile::wall_window_middle_2);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 2, Tile::wall_window_middle_1);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 3, Tile::wall_window_2);
}



} // namespace skyland
