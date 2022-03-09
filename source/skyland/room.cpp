#include "room.hpp"
#include "globals.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"
#include "scene/moveCharacterScene.hpp"
#include "script/listBuilder.hpp"
#include "skyland.hpp"
#include "skyland/tile.hpp"
#include "timeStreamEvent.hpp"



namespace skyland {



Room::Room(Island* parent, const char* name, const Vec2<u8>& position)
    : parent_(parent), dispatch_list_(nullptr), health_(1),
      x_position_(position.x), y_position_(position.y)
{

    if (name == nullptr) {
        return;
    }

    finalized_ = 0;
    dispatch_queued_ = 0;

    set_group(Room::Group::none);

    auto metatable = room_metatable();

    for (MetaclassIndex i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_eq(name, current->name())) {
            metaclass_index_ = i;

            auto mt_size = current->size();
            if (mt_size.x > 7 or mt_size.y > 7) {
                Platform::fatal("Room size too large!");
            }
            size_x_ = mt_size.x;
            size_y_ = mt_size.y;
            health_ = current->full_health();

            ready();
            return;
        }
    }

#if defined(__GBA__) or defined(__NDS__)
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
                parent_->layer(), x_position_ + x, y_position_ + y, 13);
        }
    }

    reset_injured_timer(injured_anim_time);
}



const char* Room::name() const
{
    return (*metaclass())->name();
}



RoomMeta* Room::metaclass() const
{
    return &room_metatable().first[metaclass_index_];
}



MetaclassIndex Room::metaclass_index() const
{
    return metaclass_index_;
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



void Room::display_on_hover(Platform::Screen& screen,
                            App& app,
                            const Vec2<u8>& cursor)
{
}



u8 Room::default_palette()
{
    if ((*metaclass())->properties() & RoomProperties::plugin) {
        return 12;
    } else {
        return parent_->layer() == Layer::map_0_ext ? 0 : 2;
    }
}



void Room::update(Platform& pfrm, App& app, Microseconds delta)
{
    dispatch_queued_ = false;

    if (not characters().empty()) {
        ready();
    }

    if ((bool)injured_timer_) {

        ready();

        if (injured_timer_ > 0) {
            const auto new_timer = injured_timer_ - delta;

            if (new_timer < milliseconds(210) and
                injured_timer_ > milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         14);
                    }
                }
            }

            if (new_timer < milliseconds(170) and
                injured_timer_ > milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         15);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ <= 0) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         default_palette());
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
                                         x_position_ + x,
                                         y_position_ + y,
                                         15);
                    }
                }
            }

            if (new_timer > milliseconds(210) and
                injured_timer_ < milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         14);
                    }
                }
            }

            if (new_timer > milliseconds(170) and
                injured_timer_ < milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         13);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ >= injured_anim_time) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         x_position_ + x,
                                         y_position_ + y,
                                         default_palette());
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
    origin.x += x_position_ * 16;
    origin.y += y_position_ * 16;
    return origin;
}



Vec2<Float> Room::center() const
{
    auto o = origin();
    o.x += (size_x_ * 0.5f) * 16;
    o.y += (size_y_ * 0.5f) * 16;

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
            cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
        }

        for (auto& character : characters_) {
            if (character->grid_position() == cursor_loc) {

                if (character->owner() == &app.player() or
                    (character->owner() == &app.opponent() and
                     app.game_mode() == App::GameMode::sandbox)) {
                    return scene_pool::alloc<MoveCharacterScene>(pfrm, near);
                }
            }
        }
    }
    return null_scene();
}



ScenePtr<Scene> Room::setup(Platform& pfrm, App&)
{
    // ...
    return null_scene();
}



ScenePtr<Scene> Room::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    bool chr_at_cursor = false;
    for (auto& chr : characters()) {
        if (chr->grid_position() == cursor and chr->owner() == &app.player()) {

            chr_at_cursor = true;
        }
    }

    if (parent_->interior_visible() or chr_at_cursor) {
        if (not parent_->interior_visible()) {
            show_island_interior(pfrm, app, parent_);
        }
        return do_select(pfrm, app);
    }

    return null_scene();
}



lisp::Value* Room::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    if (health() not_eq max_health()) {
        builder.push_back(L_INT(health()));
    }

    return builder.result();
}



void Room::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 4) {
        __set_health(lisp::get_list(list, 3)->integer().value_);
    }
}



void Room::plot_walkable_zones(App& app, bool matrix[16][16])
{
    // By default, treat every cell in the lowest row of a room as walkable for
    // NPCs. A few rooms, like staircases, cannons, walls, etc. will need to
    // provide different implementations.
    for (int x = 0; x < size_x_; ++x) {
        matrix[x_position_ + x][y_position_ + size_y_ - 1] = true;
    }
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

    ready();
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
    health_ = std::min((*metaclass())->full_health(), new_health);
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
            e.type_ = metaclass_index_;
            app.time_stream().push(pfrm, app.level_timer(), e);

        } else {
            time_stream::event::PlayerRoomPlundered e;
            e.x_ = position().x;
            e.y_ = position().y;
            e.type_ = metaclass_index_;
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

        for (int x = 0; x < size_x_; x += (*plunder_metac)->size().x) {
            int y = 0;
            if (size_y_ % 2 not_eq 0) {
                // NOTE: plundered-room occupies two tiles vertically. For an
                // odd-sized room height, start at 1.
                y = 1;
            }
            for (; y < size_y_; y += (*plunder_metac)->size().y) {
                const Vec2<u8> pos = {
                    u8(((u8)x_position_) + x),
                    u8(((u8)y_position_) + y),
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
    return (*metaclass())->full_health();
}



void Room::finalize(Platform& pfrm, App& app)
{
    finalized_ = true;
}



void Room::ready()
{
    if (not dispatch_queued_) {
        parent_->dispatch_room(this);
    }
}



bool Room::attach_drone(Platform& pfrm, App&, SharedEntityRef<Drone>)
{
    warning(pfrm,
            format("Attach drone to incompatible room %", name()).c_str());

    return false;
}



void Room::detach_drone(Platform&, App&, bool quiet)
{
    // Unimplemented.
}



std::optional<SharedEntityRef<Drone>> Room::drone() const
{
    // Unimplemented.
    // TODO: raise fatal error?

    return {};
}



void Room::set_group(Group group)
{
    group_ = (u8)group;
}



Room::Group Room::group() const
{
    return (Group)group_;
}



Room::~Room()
{
    if (not finalized_) {
        Platform::fatal("room destroyed without invoking finalizer!");
    }

    // Because the dispatch list uses plain unchecked raw pointers, we want to
    // be extra careful. Now, we could scan an island's entire dispatch list and
    // remove ourself, but instead, tell the island to drop its dispatch list
    // and update all rooms on the next update step.
    parent_->cancel_dispatch();
}



void Room::render_scaffolding(App& app, u8 buffer[16][16])
{
    auto p = position();
    auto s = size();

    auto sal = parent()->interior_visible()
                   ? (int)InteriorTile::scaffolding_angled_l
                   : (int)Tile::scaffolding_angled_l;

    auto sar = parent()->interior_visible()
                   ? (int)InteriorTile::scaffolding_angled_r
                   : (int)Tile::scaffolding_angled_r;

    auto strut = parent()->interior_visible() ? (int)InteriorTile::strut
                                              : (int)Tile::strut;

    auto strut_t = parent()->interior_visible() ? (int)InteriorTile::strut_top
                                                : (int)Tile::strut_top;

    bool placed_strut = false;

    if (s.x == 1) {
        for (int y = p.y + s.y; y < 15; ++y) {
            if (buffer[p.x][y]) {
                break;
            }

            if (placed_strut) {
                buffer[p.x][y] = strut;
            } else {
                buffer[p.x][y] = strut_t;
                placed_strut = true;
            }
        }
        placed_strut = false;

        return;
    }

    if (buffer[p.x][p.y + s.y] not_eq 0 and
        buffer[p.x + 1][p.y + s.y] == 0) {

        buffer[p.x + 1][p.y + s.y] = sal;

    } else if (buffer[p.x][p.y + s.y] == 0 and
               buffer[p.x + (s.x - 1)][p.y + s.y] not_eq 0) {

        if (s.x == 2) {
            buffer[p.x + (s.x - 2)][p.y + s.y] = sar;
        } else if (buffer[p.x + (s.x - 2)][p.y + s.y] == 0) {
            buffer[p.x + (s.x - 2)][p.y + s.y] = sar;
        }

    } else if (buffer[p.x][p.y + s.y] == 0 and
               buffer[p.x + (s.x - 1)][p.y + s.y] == 0) {
        for (int x = p.x; x < p.x + s.x; ++x) {
            for (int y = p.y + s.y; y < 15; ++y) {
                if (buffer[x][y] and buffer[x][y]) {
                    break;
                }

                if (placed_strut) {
                    buffer[x][y] = strut;
                } else {
                    buffer[x][y] = strut_t;
                    placed_strut = true;
                }
            }
            placed_strut = false;
        }
    }
}



} // namespace skyland
