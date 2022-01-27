#include "room.hpp"
#include "globals.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"
#include "scene/moveCharacterScene.hpp"
#include "skyland.hpp"
#include "timeStreamEvent.hpp"



namespace skyland {


Room::Room(Island* parent,
           const char* name,
           const Vec2<u8>& size,
           const Vec2<u8>& position)
    : parent_(parent),
      characters_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      size_(size), position_(position), health_(1)
{
    if (name == nullptr) {
        return;
    }

    auto metatable = room_metatable();

    for (int i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_cmp(name, current->name()) == 0) {
            metaclass_ = &current;

            health_ = (*metaclass_)->full_health();
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



static const auto injured_anim_time = milliseconds(250);



void Room::reset_injured_timer(Microseconds value)
{
    injured_timer_ = value;
}



void Room::set_injured(Platform& pfrm)
{
    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            pfrm.set_palette(
                parent_->layer(), position().x + x, position().y + y, 13);
        }
    }

    reset_injured_timer(injured_anim_time);
}



void Room::display(Platform::Screen& screen)
{
    if (parent_->interior_visible()) {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y < 700) {
                screen.draw(c->sprite());
            }
        }
    }
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



void Room::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    if (injured_timer_) {
        if (injured_timer_ < injured_anim_time) {
            const auto new_timer = injured_timer_ + delta;

            if (new_timer > milliseconds(10) and
                injured_timer_ < milliseconds(10)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         15);
                    }
                }
            }

            if (new_timer > milliseconds(210) and
                injured_timer_ < milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         14);
                    }
                }
            }

            if (new_timer > milliseconds(170) and
                injured_timer_ < milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         13);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ >= injured_anim_time) {
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



ScenePtr<Scene> Room::do_select(Platform& pfrm, App& app)
{
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
            if (character->grid_position() == cursor_loc) {

                if (character->owner() == &app.player() or
                    (character->owner() == &app.opponent() and
                     app.game_mode() == App::GameMode::sandbox)) {
                    return scene_pool::alloc<MoveCharacterScene>(pfrm,
                                                                 near);
                }
            }
        }
    }
    return null_scene();
}



ScenePtr<Scene> Room::select(Platform& pfrm, App& app)
{
    if (parent_->interior_visible()) {
        return do_select(pfrm, app);
    }

    return null_scene();
}



void Room::plot_walkable_zones(App& app, bool matrix[16][16])
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
    if (parent_ == &app.player_island()) {
        time_stream::event::PlayerRoomDamaged e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        app.time_stream().push(pfrm, app.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomDamaged e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    if (damage > health_) {
        health_ = 0;
    } else {
        health_ -= damage;
    }
    set_injured(pfrm);
    parent_->owner().on_room_damaged(pfrm, app, *this);
}



void Room::heal(Platform& pfrm, App& app, Health amount)
{
    if (parent_ == &app.player_island()) {
        time_stream::event::PlayerRoomRepaired e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        app.time_stream().push(pfrm, app.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomRepaired e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    const Health new_health = health_ + amount;
    health_ = std::min((*metaclass_)->full_health(), new_health);
}



void Room::plunder(Platform& pfrm, App& app, Health damage)
{
    apply_damage(pfrm, app, damage);

    if (health_ == 0) {
        if (parent() not_eq &app.player_island()) {
            // You get some coins for plundering a room
            pfrm.speaker().play_sound("coin", 2);
            app.set_coins(pfrm, app.coins() + (*metaclass())->cost() * 0.3f);

            time_stream::event::OpponentRoomPlundered e;
            e.x_ = position().x;
            e.y_ = position().y;
            e.type_ = metaclass_index((*metaclass_)->name());
            app.time_stream().push(pfrm, app.level_timer(), e);

        } else {
            time_stream::event::PlayerRoomPlundered e;
            e.x_ = position().x;
            e.y_ = position().y;
            e.type_ = metaclass_index((*metaclass_)->name());
            app.time_stream().push(pfrm, app.level_timer(), e);
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

        app.player().on_room_plundered(pfrm, app, *this);

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
            int y = 0;
            if (size_.y % 2 not_eq 0) {
                // NOTE: plundered-room occupies two tiles vertically. For an
                // odd-sized room height, start at 1.
                y = 1;
            }
            for (; y < size_.y; y += (*plunder_metac)->size().y) {
                const Vec2<u8> pos = {
                    u8(position_.x + x),
                    u8(position_.y + y),
                };
                (*plunder_metac)->create(pfrm, app, parent_, pos);
            }
        }

        // Now that we've created plundered rooms where the now-detached room
        // was, we can add the characters back to the parent island.
        for (auto& chr : chrs) {
            parent_->add_character(std::move(chr));
        }

        parent_->add_room(pfrm, app, std::move(*self));
    }
}



Health Room::max_health() const
{
    return (*metaclass_)->full_health();
}



} // namespace skyland
