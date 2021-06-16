#include "room.hpp"
#include "globals.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"
#include "scene/moveCharacterScene.hpp"
#include "skyland.hpp"



namespace skyland {


Room::Room(Island* parent,
           const char* name,
           const Vec2<u8>& size,
           const Vec2<u8>& position,
           Health health)
    : parent_(parent),
      characters_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      size_(size), position_(position), health_(health)
{
    auto metatable = room_metatable();

    for (int i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_cmp(name, current->name()) == 0) {
            metaclass_ = &current;
        }
    }

#ifdef __GBA__
    static_assert(sizeof(Room) < 33,
                  "Not actually a hard requirement, just put"
                  "this here as a reminder to keep rooms small."
                  " Room pool entries are only 64 bytes in size."
                  " Increasing the base room size will leave fewer "
                  " bytes for derived rooms. If needed, you could "
                  " increase the room pool size in roomPool.hpp");
#endif
}



void Room::set_injured(Platform& pfrm)
{
    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            pfrm.set_palette(
                parent_->layer(), position().x + x, position().y + y, 13);
        }
    }

    injured_timer_ = milliseconds(250);
}



void Room::update(Platform& pfrm, App& app, Microseconds delta)
{
    update_entities(pfrm, app, delta, characters_);

    if (injured_timer_) {
        if (injured_timer_ > 0) {
            const auto new_timer = injured_timer_ - delta;

            if (new_timer < milliseconds(210) and
                injured_timer_ > milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         14);
                    }
                }
            }

            if (new_timer < milliseconds(170) and
                injured_timer_ > milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         15);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ <= 0) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(
                            parent_->layer(),
                            position().x + x,
                            position().y + y,
                            parent_->layer() == Layer::map_0_ext ? 0 : 2);
                    }
                }
            }
        }
    }
}



Island* Room::other_island(App& app)
{
    if (&app.player_island() == parent_) {
        if (app.encountered_island()) {
            return &*app.encountered_island();
        }
        return nullptr;
    } else {
        return &app.player_island();
    }
}



Vec2<Float> Room::origin() const
{
    auto origin = parent_->origin();
    origin.x += position_.x * 16;
    origin.y += position_.y * 16;
    return origin;
}



Vec2<Float> Room::center() const
{
    auto o = origin();
    o.x += (size_.x * 0.5f) * 16;
    o.y += (size_.y * 0.5f) * 16;

    return o;
}



ScenePtr<Scene> Room::select(Platform& pfrm)
{
    if (parent_->interior_visible()) {
        if (length(characters_)) {
            // TODO...  In the default implementation, select a character if the
            // cursor overlaps with one of the characters assigned to the room.
        }

        // Just a test...
        return scene_pool::alloc<MoveCharacterScene>(pfrm);
    }

    return null_scene();
}



void Room::plot_walkable_zones(bool matrix[16][16])
{
    // By default, treat every cell in the lowest row of a room as walkable for
    // NPCs. A few rooms, like staircases, cannons, walls, etc. will need to
    // provide different implementations.
    for (int x = 0; x < size_.x; ++x) {
        matrix[position_.x + x][position_.y + size_.y - 1] = true;
    }
}



void Room::on_collision(Platform& pfrm, App& app, Entity& entity)
{
}


void Room::apply_damage(Platform& pfrm, App& app, Health damage)
{
    if (damage > health_) {
        health_ = 0;
    } else {
        health_ -= damage;
    }
    set_injured(pfrm);
    parent_->owner().on_room_damaged(pfrm, app, *this);
}




} // namespace skyland
