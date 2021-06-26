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
      size_(size), position_(position), health_(health), max_health_(health)
{
    auto metatable = room_metatable();

    for (int i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_cmp(name, current->name()) == 0) {
            metaclass_ = &current;
            return;
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

    // TODO: actually raise an error for an unassigned metaclass. We would need
    // to pass in a Platform instance in order to raise a fatal error.
    while (true)
        ;
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
        if (app.opponent_island()) {
            return &*app.opponent_island();
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



bool Room::description_visible()
{
    return parent_->interior_visible();
}



ScenePtr<Scene> Room::select(Platform& pfrm, App& app)
{
    if (parent_->interior_visible()) {
        if (length(characters_)) {

            const bool near = parent() == &app.player_island();

            Vec2<u8> cursor_loc;

            if (near) {
                cursor_loc =
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
            } else {
                cursor_loc =
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
            }

            for (auto& character : characters_) {
                if (character->grid_position() == cursor_loc and
                    character->owner() == &app.player()) {

                    return scene_pool::alloc<MoveCharacterScene>(pfrm, near);
                }
            }
        }
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



void Room::plunder(Platform& pfrm, App& app, Health damage)
{
    apply_damage(pfrm, app, damage);

    if (health_ == 0) {
        if (parent() not_eq &app.player_island()) {
            // You get some coins for plundering a room
            app.coins() += (*metaclass())->cost() * 0.3f;
        }

        // Ok, so when a character plunders a room, we don't actually want to
        // leave the health as zero, and let the engine erase the room. Doing so
        // would kill all of the characters attached to the room. Instead, we
        // want to detach all of the room's characters, spawn a bunch of
        // "plundered room" sentinel structures, and reattach each character to
        // the plundered room.

        // NOTE: This buffer should be plenty big enough. Only two characters
        // can occupy one x,y slot in a room (not as a limitation of the code,
        // but as a requirement of the gameplay). The max room size is generally
        // four slots, so we should have eight character entities at most...
        Buffer<EntityRef<BasicCharacter>, 16> chrs;

        for (auto& chr : characters()) {
            chrs.push_back(std::move(chr));
        }

        characters().clear();

        std::optional<RoomPtr<Room>> self;

        // Detach ourself from the parent island's room list:
        for (auto it = parent_->rooms().begin();
             it not_eq parent_->rooms().end();) {
            if ((*it).get() == this) {
                self = std::move(*it);
                it = parent_->rooms().erase(it);
                break;
            } else {
                ++it;
            }
        }

        if (not self) {
            // hmm... not sure why we would ever get here...
            pfrm.fatal("plunder: reassign failure");
        }

        auto plunder_metac = load_metaclass("plundered-room");

        if (not plunder_metac) {
            error(pfrm, "failed to load metaclass");
            return;
        }

        for (int x = 0; x < size_.x; x += (*plunder_metac)->size().x) {
            for (int y = 0; y < size_.y; y += (*plunder_metac)->size().y) {
                const Vec2<u8> pos = {
                    u8(position_.x + x),
                    u8(position_.y + y),
                };
                (*plunder_metac)->create(pfrm, parent_, pos);
            }
        }

        // Now that we've created plundered rooms where the now-detached room
        // was, we can add the characters back to the parent island.
        for (auto& chr : chrs) {
            parent_->add_character(std::move(chr));
        }

        parent_->rooms().push_back(std::move(*self));
    }
}



} // namespace skyland
