////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "rewindScene.hpp"
#include "fatalErrorScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/explosion/explosion2.hpp"
#include "skyland/entity/projectile/antimatter.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/entity/projectile/beam.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/entity/projectile/fireBolt.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/entity/projectile/incineratorBolt.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/entity/projectile/nemesisBlast.hpp"
#include "skyland/entity/projectile/pluginProjectile.hpp"
#include "skyland/entity/projectile/projectile.hpp"
#include "skyland/minimap.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/canvas.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/phaseShifter.hpp"
#include "skyland/rooms/weapon.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



template <typename T, typename E, typename F, typename... Args>
T* respawn_basic_projectile(Island* parent,
                            const E& e,
                            F&& explosion_function,
                            Args&&... args)
{
    auto c =
        APP.alloc_entity<T>(Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                                         Fixnum::from_integer(e.y_pos_.get())},
                            Vec2<Fixnum>{},
                            parent,
                            RoomCoord{e.x_origin_, e.y_origin_},
                            args...);
    if (c) {
        Vec2<Fixnum> step_vector{Fixnum::create(e.x_speed__data_.get()),
                                 Fixnum::create(e.y_speed__data_.get())};
        c->set_step_vector(step_vector);
        c->set_timer(e.timer_.get());
        explosion_function(c->sprite().get_position());
        auto ret = c.get();
        parent->projectiles().push(std::move(c));
        return ret;
    }
    return nullptr;
}



void respawn_plugin_projectile(
    Island* parent,
    const time_stream::event::PluginProjectileDestroyed& e)
{
    auto c = APP.alloc_entity<PluginProjectile>(

        Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                     Fixnum::from_integer(e.y_pos_.get())},
        Vec2<Fixnum>{},
        parent,
        RoomCoord{e.x_origin_, e.y_origin_},
        e.tile_.get(),
        e.damage_.get(),
        e.hflip_);
    if (c) {
        Vec2<Fixnum> step_vector{Fixnum::create(e.x_speed__data_.get()),
                                 Fixnum::create(e.y_speed__data_.get())};
        c->set_step_vector(step_vector);
        c->set_timer(e.timer_.get());
        medium_explosion_inv(c->sprite().get_position());
        parent->projectiles().push(std::move(c));
    }
}



void respawn_missile(Island* parent, time_stream::event::MissileDestroyed& e)
{
    auto m = APP.alloc_entity<Missile>(

        Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                     Fixnum::from_integer(e.y_pos_.get())},
        Vec2<Fixnum>{
            Fixnum::from_integer(e.target_x_.get()),
            0.0_fixed // TODO: change target parameter to simple float, y unused.
        },
        (u8)e.source_x_,
        (u8)e.source_y_,
        parent);

    if (m) {
        m->set_timer(e.timer_.get());
        m->set_state((Missile::State)e.state_);

        medium_explosion_inv(m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void respawn_rocketbomb(Island* parent, time_stream::event::MissileDestroyed& e)
{
    auto m = APP.alloc_entity<RocketBomb>(

        Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                     Fixnum::from_integer(e.y_pos_.get())},
        Vec2<Fixnum>{
            Fixnum::from_integer(e.target_x_.get()),
            0.0_fixed // TODO: change target parameter to simple float, y unused.
        },
        (u8)e.source_x_,
        (u8)e.source_y_,
        parent);

    if (m) {
        m->set_timer(e.timer_.get());
        m->set_state((Missile::State)e.state_);

        medium_explosion_inv(m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void respawn_clumpmissile(Island* parent,
                          time_stream::event::MissileDestroyed& e)
{
    auto m = APP.alloc_entity<ClumpMissile>(

        Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                     Fixnum::from_integer(e.y_pos_.get())},
        Vec2<Fixnum>{
            Fixnum::from_integer(e.target_x_.get()),
            0.0_fixed // TODO: change target parameter to simple float, y unused.
        },
        (u8)e.source_x_,
        (u8)e.source_y_,
        parent);

    if (m) {
        m->set_timer(e.timer_.get());
        m->set_state((Missile::State)e.state_);

        medium_explosion_inv(m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void respawn_atomic(Island* parent, time_stream::event::MissileDestroyed& e)
{
    auto m = APP.alloc_entity<AtomicMissile>(

        Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
                     Fixnum::from_integer(e.y_pos_.get())},
        Vec2<Fixnum>{
            Fixnum::from_integer(e.target_x_.get()),
            0.0_fixed // TODO: change target parameter to simple float, y unused.
        },
        (u8)e.source_x_,
        (u8)e.source_y_,
        parent);

    if (m) {
        m->set_timer(e.timer_.get());
        m->set_state((Missile::State)e.state_);

        medium_explosion_inv(m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void RewindScene::print_timestamp()
{
    if (not text_) {
        text_.emplace(OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
    }
    const auto level_seconds = APP.level_timer().whole_seconds();
    int hours = 0;
    int minutes = 0;
    int seconds = level_seconds;
    while (seconds > 60 * 60) {
        seconds -= 60 * 60;
        ++hours;
    }
    while (seconds > 60) {
        seconds -= 60;
        ++minutes;
    }
    StringBuffer<30> fmt;
    if (hours < 10) {
        fmt += "0";
    }
    fmt += stringify(hours);
    fmt += ":";
    if (minutes < 10) {
        fmt += "0";
    }
    fmt += stringify(minutes);
    fmt += ":";
    if (seconds < 10) {
        fmt += "0";
    }
    fmt += stringify(seconds);

    text_->assign(fmt.c_str());
}



ScenePtr RewindScene::update(Time)
{
    bool speed_changed = false;

    if (APP.player().key_down(Key::up) and speed_ > 0) {
        --speed_;
        speed_changed = true;
    }
    if (APP.player().key_down(Key::down) and speed_ < 2) {
        ++speed_;
        speed_changed = true;
    }

    if (speed_changed) {
        switch (speed_) {
        case 0:
            PLATFORM.set_tile(Layer::overlay, 0, 16, 475);
            PLATFORM.set_tile(Layer::overlay, 0, 17, 112);
            PLATFORM.set_tile(Layer::overlay, 0, 18, 112);
            PLATFORM.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed);
            break;

        case 1:
            PLATFORM.set_tile(Layer::overlay, 0, 16, 112);
            PLATFORM.set_tile(Layer::overlay, 0, 17, 475);
            PLATFORM.set_tile(Layer::overlay, 0, 18, 112);
            PLATFORM.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed4x);
            break;

        case 2:
            PLATFORM.set_tile(Layer::overlay, 0, 16, 112);
            PLATFORM.set_tile(Layer::overlay, 0, 17, 112);
            PLATFORM.set_tile(Layer::overlay, 0, 18, 475);
            PLATFORM.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed8x);
            break;
        }
    }

    // Playback history at a fixed delta.
    Time delta = 2 * (seconds(1) / 60);
    if (speed_ == 1) {
        delta *= 2;
    } else if (speed_ == 2) {
        delta *= 4;
    }

    APP.delta_fp() = delta;


    // Some functions called in this block may try to call other functions that
    // push events to the time stream, which we don't want to do, as we're
    // rewinding.
    APP.time_stream().enable_pushes(false);


    APP.level_timer().count_down(delta);
    APP.time_stream().rewind(delta);


    APP.environment().rewind(delta);


    print_timestamp();


    bool move_region = false;

    if (far_camera_) {
        auto& cursor_loc = globals().far_cursor_loc_;
        APP.camera()->update(*APP.opponent_island(), cursor_loc, delta, false);
    } else {
        auto& cursor_loc = globals().near_cursor_loc_;
        APP.camera()->update(APP.player_island(), cursor_loc, delta, true);
    }


    auto is_decimator_firing = [&] {
        for (auto& r : APP.player_island().rooms()) {
            if (auto dec = r->cast<Decimator>()) {
                if (dec->firing_) {
                    return true;
                }
            }
        }
        if (APP.opponent_island()) {
            for (auto& r : APP.opponent_island()->rooms()) {
                if (auto dec = r->cast<Decimator>()) {
                    if (dec->firing_) {
                        return true;
                    }
                }
            }
        }
        return false;
    };


    const auto current_timestamp = APP.level_timer().total();
    auto end_timestamp = APP.time_stream().end_timestamp();

    if (not APP.opponent_island() or not end_timestamp or
        APP.player().key_down(Key::alt_1) or
        (exit_requested_ and not is_decimator_firing())) {

        if (APP.player().key_down(Key::alt_1)) {
            if (is_decimator_firing()) {
                exit_requested_ = true;
                return null_scene();
            }
        }

        APP.time_stream().enable_pushes(true);

        if (not end_timestamp) {
            // If we've rewound all of the way, enqueue another Initial event.
            // The event really contains nothing at all, but designates the
            // beginning of the event sequence, giving us something to rewind
            // to. Our implementation only allows rewinding until the earliest
            // existing event.
            time_stream::event::Initial e;
            APP.time_stream().push(APP.level_timer(), e);
        }

        if (far_camera_) {
            return make_scene<InspectP2Scene>();
        } else {
            return make_scene<ReadyScene>();
        }
    }

    // NOTE: IMPORTANT: any code called in this loop should not push a
    // time_stream event! You could get stuck in an endless loop!
    while (end_timestamp and ((*end_timestamp > current_timestamp) or
                              (APP.level_timer().total() == 0))) {
        auto end = APP.time_stream().end();
        switch ((time_stream::event::Type)end->type_) {

        case time_stream::event::Type::initial: {
            APP.time_stream().pop(sizeof(time_stream::event::Initial));
            break;
        }


        case time_stream::event::Type::achievement: {
            auto e = (time_stream::event::Achievement*)end;
            achievements::lock(e->which_);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_created: {
            auto e = (time_stream::event::PlayerRoomCreated*)end;
            APP.player_island().destroy_room({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_created: {
            auto e = (time_stream::event::OpponentRoomCreated*)end;
            APP.opponent_island()->destroy_room({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::canvas_block_destroyed: {
            auto e = (time_stream::event::CanvasBlockDestroyed*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (island) {
                if (auto room = island->get_room({e->x_, e->y_})) {
                    if (auto c = room->cast<Canvas>()) {
                        img::Image tmp;
                        memcpy(&tmp, e->data_, sizeof tmp);
                        c->bind_graphics(tmp);
                    }
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_destroyed: {
            auto e = (time_stream::event::PlayerRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(&APP.player_island(), {e->x_, e->y_});
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(room->origin());
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_destroyed_with_group: {
            auto e = (time_stream::event::PlayerRoomDestroyedWithGroup*)end;
            (*load_metaclass(e->type_))
                ->create(&APP.player_island(), {e->x_, e->y_});
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(room->origin());
                }
                room->set_group((Room::Group)e->group_);
                APP.player_island().repaint();
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_destroyed: {
            auto e = (time_stream::event::OpponentRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(APP.opponent_island(), {e->x_, e->y_});
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(room->origin());
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_powerchange: {
            auto e = (time_stream::event::PlayerRoomPowerchange*)end;
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                room->set_powerdown(e->status_);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_powerchange: {
            auto e = (time_stream::event::OpponentRoomPowerchange*)end;
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                room->set_powerdown(e->status_);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_transmuted: {
            auto e = (time_stream::event::PlayerRoomTransmuted*)end;

            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                room->__unsafe__transmute(e->prev_type_);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_transmuted: {
            auto e = (time_stream::event::OpponentRoomTransmuted*)end;

            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                room->__unsafe__transmute(e->prev_type_);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_salvaged: {
            auto e = (time_stream::event::PlayerRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(&APP.player_island(), {e->x_, e->y_});
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                if (room->metaclass_index() == e->type_) { // sanity check
                    room->__set_health(e->health_.get());
                    room->set_group((Room::Group)e->group_);
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_salvaged: {
            auto e = (time_stream::event::OpponentRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(APP.opponent_island(), {e->x_, e->y_});
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                if (room->metaclass_index() == e->type_) {
                    room->__set_health(e->health_.get());
                    room->set_group((Room::Group)e->group_);
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_moved: {
            auto e = (time_stream::event::PlayerRoomMoved*)end;
            if (auto r = APP.player_island().get_room({e->x_, e->y_})) {
                APP.player_island().move_room({e->x_, e->y_},
                                              {e->prev_x_, e->prev_y_});
                if (move_region) {
                    r->set_hidden(true);
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_moved: {
            auto e = (time_stream::event::OpponentRoomMoved*)end;
            if (auto r = APP.opponent_island()->get_room({e->x_, e->y_})) {
                APP.opponent_island()->move_room({e->x_, e->y_},
                                                 {e->prev_x_, e->prev_y_});
                if (move_region) {
                    r->set_hidden(true);
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::move_region_begin: {
            auto e = (time_stream::event::MoveRegionBegin*)end;
            move_region = false;
            for (auto& room : APP.player_island().rooms()) {
                room->set_hidden(false);
            }
            for (auto& room : APP.opponent_island()->rooms()) {
                room->set_hidden(false);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::move_region_end: {
            auto e = (time_stream::event::MoveRegionEnd*)end;
            move_region = true;
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_fire_created: {
            auto e = (time_stream::event::PlayerFireCreated*)end;
            APP.player_island().fire_extinguish({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_fire_created: {
            auto e = (time_stream::event::OpponentFireCreated*)end;
            APP.opponent_island()->fire_extinguish({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_fire_extinguished: {
            auto e = (time_stream::event::PlayerFireExtinguished*)end;
            APP.player_island().fire_create({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_fire_extinguished: {
            auto e = (time_stream::event::OpponentFireExtinguished*)end;
            APP.opponent_island()->fire_create({e->x_, e->y_});
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::attach_reconstruction_queue: {
            auto e = (time_stream::event::AttachReconstructionQueue*)end;
            RoomCoord pos = {e->db_x_, e->db_y_};
            if (auto room = APP.player_island().get_room(pos)) {
                if (auto db = room->cast<DroneBay>()) {
                    static_assert(std::is_trivially_copyable_v<
                                  ReconstructionQueue::ValueType>);
                    db->rq_.mem_ = e->previous_queue_memory_;
                    db->rq_.count_ = e->previous_queue_size_;
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_plundered:
        case time_stream::event::Type::opponent_room_plundered: {
            auto e = (time_stream::event::RoomPlundered*)end;
            Buffer<EntityRef<Character>, 16> chrs;

            Island* island =
                (time_stream::event::Type)end->type_ ==
                        time_stream::event::Type::player_room_plundered
                    ? &APP.player_island()
                    : APP.opponent_island();

            // NOTE: the plundered room was already technically re-created, by
            // the room destroyed event issued by the zero-health room after it
            // was pundered. But we want to splice characters back into the
            // original room, and the easiest way is to destroy everything and
            // re-create it.

            // First, we want to remove all characters inhabiting the
            // plundered-room structures that comprise the room. Cache them in
            // buffer chrs.
            auto meta = load_metaclass(e->type_);
            for (u8 x = e->x_; x < e->x_ + (*meta)->size().x; ++x) {
                for (u8 y = e->y_; y < e->y_ + (*meta)->size().y; ++y) {
                    // NOTE: scan rooms manually, because both the
                    // plundered-room structures and the original room currently
                    // exist.
                    for (auto& room : island->rooms()) {
                        if (room->position().x == x and
                            room->position().y == y) {
                            for (auto& chr : room->characters()) {
                                chrs.push_back(std::move(chr));
                                room->edit_characters().clear();
                            }
                        }
                    }
                }
            }

            // Destroy plundered-room structures in the location where we want
            // to re-create the original room.
            for (u8 x = e->x_; x < e->x_ + (*meta)->size().x; ++x) {
                for (u8 y = e->y_; y < e->y_ + (*meta)->size().y; ++y) {
                    if (island->get_room({x, y})) {
                        island->destroy_room({x, y});
                    }
                }
            }

            // Re-create the original room.
            (*meta)->create(island, RoomCoord{e->x_, e->y_});

            // Add characters back.
            if (auto room = island->get_room({e->x_, e->y_})) {
                for (auto& chr : chrs) {
                    room->edit_characters().push(std::move(chr));
                }
            } else {
                auto err = "rewind salvage: attempt to re-attach character"
                           " to non-existent room.";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_nemesis_blast_destroyed: {
            auto e = (time_stream::event::PlayerNemesisBlastDestroyed*)end;
            if (auto v = respawn_basic_projectile<NemesisBlast>(
                    &APP.player_island(), *e, medium_explosion_inv)) {
                v->set_variant(e->variant_);
                APP.camera()->shake(8);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_nemesis_blast_destroyed: {
            auto e = (time_stream::event::PlayerNemesisBlastDestroyed*)end;
            if (auto v = respawn_basic_projectile<NemesisBlast>(
                    APP.opponent_island(), *e, medium_explosion_inv)) {
                v->set_variant(e->variant_);
                APP.camera()->shake(8);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_plugin_projectile_destroyed: {
            auto e = (time_stream::event::PlayerPluginProjectileDestroyed*)end;
            respawn_plugin_projectile(&APP.player_island(), *e);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_plugin_projectile_destroyed: {
            auto e =
                (time_stream::event::OpponentPluginProjectileDestroyed*)end;
            respawn_plugin_projectile(APP.opponent_island(), *e);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_ballista_bolt_destroyed: {
            auto e = (time_stream::event::PlayerBallistaBoltDestroyed*)end;
            auto pos = Vec2<Fixnum>{Fixnum::from_integer(e->x_pos_.get()),
                                    Fixnum::from_integer(e->y_pos_.get())};
            alignas(BallistaBolt::State)
                u8 state_mem[sizeof(BallistaBolt::State)];
            memcpy(state_mem, e->state_, sizeof state_mem);
            auto& state = *reinterpret_cast<BallistaBolt::State*>(state_mem);
            auto b = APP.alloc_entity<BallistaBolt>(pos, state);
            if (b) {
                medium_explosion_inv(b->sprite().get_position());
                APP.player_island().projectiles().push(std::move(b));
            }
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_cannonball_destroyed: {
            auto e = (time_stream::event::PlayerCannonballDestroyed*)end;
            auto p = respawn_basic_projectile<Cannonball>(
                &APP.player_island(), *e, medium_explosion_inv);
            if (p) {
                p->set_variant(e->variant_);
            }
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_cannonball_destroyed: {
            auto e = (time_stream::event::OpponentCannonballDestroyed*)end;
            auto p = respawn_basic_projectile<Cannonball>(
                APP.opponent_island(), *e, medium_explosion_inv);
            if (p) {
                p->set_variant(e->variant_);
            }
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_incineratorbolt_destroyed: {
            auto e = (time_stream::event::PlayerIncineratorboltDestroyed*)end;
            respawn_basic_projectile<IncineratorBolt>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(14);
            break;
        }


        case time_stream::event::Type::opponent_incineratorbolt_destroyed: {
            auto e = (time_stream::event::OpponentIncineratorboltDestroyed*)end;
            respawn_basic_projectile<IncineratorBolt>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(14);
            break;
        }


        case time_stream::event::Type::player_beam_destroyed: {
            auto e = (time_stream::event::PlayerBeamDestroyed*)end;
            auto t = respawn_basic_projectile<Beam>(
                &APP.player_island(), *e, medium_explosion_inv, e->index_);
            if (t) {
                t->restore_blocks_hit(*e);
            }
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_beam_destroyed: {
            auto e = (time_stream::event::OpponentBeamDestroyed*)end;
            auto t = respawn_basic_projectile<Beam>(
                APP.opponent_island(), *e, medium_explosion_inv, e->index_);
            if (t) {
                t->restore_blocks_hit(*e);
            }
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_arcbolt_destroyed: {
            auto e = (time_stream::event::PlayerArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_arcbolt_destroyed: {
            auto e = (time_stream::event::OpponentArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_firebolt_destroyed: {
            auto e = (time_stream::event::PlayerFireboltDestroyed*)end;
            respawn_basic_projectile<FireBolt>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_firebolt_destroyed: {
            auto e = (time_stream::event::OpponentFireboltDestroyed*)end;
            respawn_basic_projectile<FireBolt>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_flak_destroyed: {
            auto e = (time_stream::event::PlayerFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_flak_destroyed: {
            auto e = (time_stream::event::OpponentFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_ionburst_destroyed: {
            auto e = (time_stream::event::PlayerIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_ionburst_destroyed: {
            auto e = (time_stream::event::OpponentIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_antimatter_destroyed: {
            auto e = (time_stream::event::PlayerAntimatterDestroyed*)end;
            respawn_basic_projectile<Antimatter>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_antimatter_destroyed: {
            auto e = (time_stream::event::OpponentAntimatterDestroyed*)end;
            respawn_basic_projectile<Antimatter>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_decimator_burst_created: {
            auto e = (time_stream::event::PlayerDecimatorBurstCreated*)end;
            RoomCoord c;
            c.x = e->src_x_;
            c.y = e->src_y_;
            if (auto room = APP.player_island().get_room(c)) {
                if (auto dec = room->cast<Decimator>()) {
                    dec->rewind_projectile_created(e->prev_counter_);
                    if (e->prev_counter_ == 0) {
                        dec->rewind_started_firing();
                    }
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_decimator_burst_created: {
            auto e = (time_stream::event::OpponentDecimatorBurstCreated*)end;
            RoomCoord c;
            c.x = e->src_x_;
            c.y = e->src_y_;
            APP.with_opponent_island([&](auto& isle) {
                if (auto room = isle.get_room(c)) {
                    if (auto dec = room->template cast<Decimator>()) {
                        dec->rewind_projectile_created(e->prev_counter_);
                        if (e->prev_counter_ == 0) {
                            dec->rewind_started_firing();
                        }
                    }
                }
            });
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_decimator_burst_destroyed: {
            auto e = (time_stream::event::PlayerDecimatorBurstDestroyed*)end;
            respawn_basic_projectile<DecimatorBurst>(
                &APP.player_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(26);
            break;
        }


        case time_stream::event::Type::opponent_decimator_burst_destroyed: {
            auto e = (time_stream::event::OpponentDecimatorBurstDestroyed*)end;
            respawn_basic_projectile<DecimatorBurst>(
                APP.opponent_island(), *e, medium_explosion_inv);
            APP.time_stream().pop(sizeof *e);
            APP.camera()->shake(26);
            break;
        }


        case time_stream::event::Type::player_missile_destroyed: {
            auto e = (time_stream::event::PlayerMissileDestroyed*)end;
            respawn_missile(&APP.player_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_missile_destroyed: {
            auto e = (time_stream::event::OpponentMissileDestroyed*)end;
            respawn_missile(APP.opponent_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_rocketbomb_destroyed: {
            auto e = (time_stream::event::PlayerRocketBombDestroyed*)end;
            respawn_rocketbomb(&APP.player_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_rocketbomb_destroyed: {
            auto e = (time_stream::event::OpponentRocketBombDestroyed*)end;
            respawn_rocketbomb(APP.opponent_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_clumpmissile_destroyed: {
            auto e = (time_stream::event::PlayerClumpMissileDestroyed*)end;
            respawn_clumpmissile(&APP.player_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_clumpmissile_destroyed: {
            auto e = (time_stream::event::OpponentClumpMissileDestroyed*)end;
            respawn_clumpmissile(APP.opponent_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_atomic_destroyed: {
            auto e = (time_stream::event::PlayerAtomicDestroyed*)end;
            respawn_atomic(&APP.player_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_atomic_destroyed: {
            auto e = (time_stream::event::OpponentAtomicDestroyed*)end;
            respawn_atomic(APP.opponent_island(), *e);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_damaged_small: {
            auto e = (time_stream::event::PlayerRoomDamagedSmall*)end;
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(room->health() + e->diff_);
                room->reset_injured_timer(1);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_damaged_small: {
            auto e = (time_stream::event::OpponentRoomDamagedSmall*)end;
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(room->health() + e->diff_);
                room->reset_injured_timer(1);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_damaged: {
            auto e = (time_stream::event::PlayerRoomDamaged*)end;
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
                room->reset_injured_timer(1);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_damaged: {
            auto e = (time_stream::event::OpponentRoomDamaged*)end;
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
                room->reset_injured_timer(1);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_repaired: {
            auto e = (time_stream::event::PlayerRoomRepaired*)end;
            if (auto room = APP.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_repaired: {
            auto e = (time_stream::event::OpponentRoomRepaired*)end;
            if (auto room = APP.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::coins_changed: {
            auto e = (time_stream::event::CoinsChanged*)end;
            APP.set_coins(e->previous_value_.get());
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_movement_path_assigned: {
            auto e = (time_stream::event::CharacterMovementPathAssigned*)end;
            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->drop_movement_path();
            } else {
                auto err = "rewind chr mv path asgn: invalid chr id!";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::character_position_jump: {
            auto e = (time_stream::event::CharacterPositionJump*)end;
            auto isle = &APP.player_island();
            if (auto chr = isle->find_character_by_id(e->id_.get()).first) {
                chr->set_grid_position({e->previous_x_, e->previous_y_});
                chr->drop_movement_path();
                chr->set_idle();
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_moved: {
            auto e = (time_stream::event::CharacterMoved*)end;
            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->rewind_movement_step({e->previous_x_, e->previous_y_});
                if (e->superpinned_) {
                    chr->superpin(false);
                } else {
                    chr->un_superpin();
                }
            } else {
                auto err = "rewind chr mv path asgn: invalid chr id! (2)";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_stats_changed: {
            auto e = (time_stream::event::CharacterStatsChanged*)end;
            auto chr_info = Character::find_by_id(e->id_.get());
            if (chr_info.first) {
                chr_info.first->stats().info_ = e->prev_stats_;
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_died: {
            auto e = (time_stream::event::CharacterDied*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            Player* owner =
                e->owned_by_player_ ? &APP.player() : &APP.opponent();

            const bool is_replicant = e->is_replicant_;

            auto alloc_chr = [&] {
                return APP.alloc_entity<Character>(
                    island, owner, RoomCoord{e->x_, e->y_}, is_replicant);
                ;
            };

            auto chr = alloc_chr();

            if (not chr) {
                // A playable character is more essential than a projectile. We
                // should never fail to allocate a character because the screen
                // is full of effects and projectiles.
                if (not APP.player_island().projectiles().pop_last()) {
                    if (APP.opponent_island()) {
                        APP.opponent_island()->projectiles().pop_last();
                    }
                }

                chr = alloc_chr();
            }

            if (chr) {
                chr->__assign_id(e->id_.get());
                chr->set_race((Character::Race)e->race_);
                chr->set_icon(e->icon_);
                chr->set_max_health(e->max_health_);
                chr->__set_health(e->health_);
                chr->stats().info_ = e->stats_;

                island->add_character(std::move(chr));
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::replicant_created: {
            auto e = (time_stream::event::ReplicantCreated*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (auto room = island->get_room({e->x_, e->y_})) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();) {
                    const bool player_chr = (*it)->owner() == &APP.player();
                    if (player_chr == e->owned_by_player_ and
                        (*it)->grid_position() == RoomCoord{e->x_, e->y_}) {
                        if ((*it)->is_replicant()) {
                            room->edit_characters().erase(it);
                        } else {
                            auto err = "rewind error: rewind replicant"
                                       "is not replicant?!";
                            APP.time_stream().enable_pushes(true);
                            return make_scene<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_health_changed: {
            auto e = (time_stream::event::CharacterHealthChanged*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->__set_health(e->previous_health_);
            } else {
                auto err = "rewind chr health changed: invalid chr id!";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_transported: {
            auto e = (time_stream::event::CharacterTransported*)end;

            Island* source_island =
                e->source_near_ ? &APP.player_island() : APP.opponent_island();

            Island* dest_island = not e->source_near_ ? &APP.player_island()
                                                      : APP.opponent_island();

            // Ok, so here, we want to revert the transport. Find the
            // transported character at the destination island, and move it back
            // to the original location at the source island.

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                auto err = "rewind chr_transported: Invalid character id!";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            auto dest_room = chr_info.second;
            if (dest_room) {
                for (auto it = dest_room->characters().begin();
                     it not_eq dest_room->characters().end();) {

                    if ((*it).get() == chr_info.first) {
                        auto detached = std::move(*it);
                        dest_room->edit_characters().erase(it);
                        detached->set_grid_position(
                            {e->previous_x_, e->previous_y_});
                        detached->set_parent(source_island);
                        detached->drop_movement_path();
                        if (auto source_room = source_island->get_room(
                                {e->previous_x_, e->previous_y_})) {
                            source_room->edit_characters().push(
                                std::move(detached));

                            // Give the transport back to the transporter, as
                            // we've reverted it.
                            source_room->___rewind___ability_used();

                        } else {
                            auto err = "fatal error when rewinding "
                                       "transport: source room missing.";
                            APP.time_stream().enable_pushes(true);
                            return make_scene<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                auto err = "error rewinding transport: dest room missing.";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_disembark: {
            auto e = (time_stream::event::CharacterDisembark*)end;

            Island* source_island =
                e->chr_near_ ? &APP.player_island() : APP.opponent_island();

            Island* dest_island =
                not e->chr_near_ ? &APP.player_island() : APP.opponent_island();

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                auto err = "rewind chr_disembark: Invalid character id!";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            auto dest_room = chr_info.second;
            if (dest_room) {
                for (auto it = dest_room->characters().begin();
                     it not_eq dest_room->characters().end();) {

                    if ((*it).get() == chr_info.first) {

                        // If the character disembarked, it must have ended up
                        // in a transporter. As we're rewinding things, give the
                        // transporter back its transport.
                        dest_room->___rewind___ability_used();

                        auto detached = std::move(*it);
                        dest_room->edit_characters().erase(it);
                        const RoomCoord pos{e->previous_x_, e->previous_y_};
                        detached->set_grid_position(pos);
                        detached->set_parent(source_island);
                        detached->drop_movement_path();
                        detached->set_idle();
                        if (auto source_room = source_island->get_room(pos)) {
                            source_room->edit_characters().push(
                                std::move(detached));
                        } else {
                            auto err = "fatal error when rewinding "
                                       "disembark: source room missing";
                            APP.time_stream().enable_pushes(true);
                            return make_scene<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                auto err = "error rewinding disembark: dest room missing";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::weapon_set_group: {
            auto e = (time_stream::event::WeaponSetGroup*)end;
            if (auto room =
                    APP.player_island().get_room({e->room_x_, e->room_y_})) {
                room->set_group((Room::Group)e->prev_group_);
                APP.player_island().repaint();
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_target_queue_pop: {
            auto e = (time_stream::event::DroneTargetQueuePop*)end;
            auto isle = e->destination_near_ ? &APP.player_island()
                                             : APP.opponent_island();
            if (isle) {
                if (auto drone = isle->get_drone({e->x_pos_, e->y_pos_})) {
                    (*drone)->__rewind_push_target_queue(
                        {e->queue_elem_x_, e->queue_elem_y_});
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_target_queue_clear: {
            auto e = (time_stream::event::DroneTargetQueueClear*)end;
            auto isle = e->destination_near_ ? &APP.player_island()
                                             : APP.opponent_island();
            if (isle) {
                if (auto drone = isle->get_drone({e->x_pos_, e->y_pos_})) {
                    (*drone)->clear_target_queue();
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::target_queue_clear: {
            auto e = (time_stream::event::TargetQueueClear*)end;
            auto& isle = APP.player_island();
            if (auto room = isle.get_room({e->room_x_, e->room_y_})) {
                if (auto w = room->cast_weapon()) {
                    w->clear_target_queue();
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::target_queue_pop: {
            auto e = (time_stream::event::TargetQueuePop*)end;
            auto& isle = APP.player_island();
            if (auto room = isle.get_room({e->room_x_, e->room_y_})) {
                if (auto w = room->cast_weapon()) {
                    w->__rewind_push_target_queue(
                        {e->queue_elem_x_, e->queue_elem_y_});
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::weapon_set_target: {
            auto e = (time_stream::event::WeaponSetTarget*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (auto room = island->get_room({e->room_x_, e->room_y_})) {
                if (e->has_previous_target_) {
                    room->set_target(
                        RoomCoord{e->previous_target_x_, e->previous_target_y_},
                        e->previous_target_pinned_);
                } else {
                    room->unset_target();
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_reload_complete: {
            auto e = (time_stream::event::PlayerRoomReloadComplete*)end;
            if (auto room =
                    APP.player_island().get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload();
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_reload_complete: {
            auto e = (time_stream::event::OpponentRoomReloadComplete*)end;
            if (auto room =
                    APP.opponent_island()->get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload();
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_island_drift_changed: {
            auto e = (time_stream::event::OpponentIslandDriftChanged*)end;
            auto val = Fixnum::create(e->previous_speed__data_.get());
            APP.opponent_island()->set_drift(val);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::island_terrain_changed: {
            auto e = (time_stream::event::IslandTerrainChanged*)end;

            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            island->init_terrain(e->previous_terrain_size_);
            island->repaint();

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_deployed: {
            auto e = (time_stream::event::DroneDeployed*)end;

            Island* dest_island = e->destination_near_ ? &APP.player_island()
                                                       : APP.opponent_island();

            for (auto& drone_sp : dest_island->drones()) {
                if (drone_sp->position().x == e->x_pos_ and
                    drone_sp->position().y == e->y_pos_) {

                    drone_sp->__override_state(Drone::State::launch,
                                               e->duration_.get(),
                                               e->duration_.get());
                    break;
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_health_changed: {
            auto e = (time_stream::event::DroneHealthChanged*)end;

            Island* dest_island = e->destination_near_ ? &APP.player_island()
                                                       : APP.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    drone->__set_health(e->previous_health_.get());
                    break;
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_set_target: {
            auto e = (time_stream::event::DroneSetTarget*)end;

            Island* dest_island = e->destination_near_ ? &APP.player_island()
                                                       : APP.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    if (e->has_previous_target_) {
                        drone->set_target(
                            {e->previous_target_x_, e->previous_target_y_},
                            false,
                            e->previous_target_near_);
                    } else {
                        drone->drop_target();
                    }
                    break;
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_reload_complete: {
            auto e = (time_stream::event::DroneReloadComplete*)end;

            Island* dest_island = e->destination_near_ ? &APP.player_island()
                                                       : APP.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    drone->___rewind___finished_reload();
                    break;
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_destroyed: {
            auto e = (time_stream::event::DroneDestroyed*)end;

            Island* dest_island = e->destination_near_ ? &APP.player_island()
                                                       : APP.opponent_island();

            Island* parent_island =
                e->parent_near_ ? &APP.player_island() : APP.opponent_island();

            auto drone_class = &drone_metatable().first[e->type_];
            if (auto drone = (*drone_class)
                                 ->create(parent_island,
                                          dest_island,
                                          RoomCoord{e->db_x_pos_,
                                                    u8(e->db_y_pos_ - 1)})) {

                (*drone)->set_movement_target(RoomCoord{e->x_pos_, e->y_pos_});

                (*drone)->__override_state((Drone::State)e->state_,
                                           e->duration_.get(),
                                           e->timer_.get());
                (*drone)->on_rewind_drone_destroyed();

                if (auto room =
                        parent_island->get_room({e->db_x_pos_, e->db_y_pos_})) {
                    if (room->attach_drone(*drone)) {
                        dest_island->drones().push(*drone);
                    } else {
                        auto err = "rewind: attempt to attach drone to non"
                                   " drone-bay";
                        APP.time_stream().enable_pushes(true);
                        return make_scene<FatalErrorScene>(err);
                    }
                } else {
                    StringBuffer<64> fmt =
                        "rewind: drone attachment point dne: ";
                    fmt += stringify(e->db_x_pos_);
                    fmt += ", ";
                    fmt += stringify(e->db_y_pos_);
                    Platform::fatal(fmt.c_str());
                }
            } else {
                auto err = "rewind: failed to alloc drone";
                APP.time_stream().enable_pushes(true);
                return make_scene<FatalErrorScene>(err);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::rng_changed: {
            auto e = (time_stream::event::RngChanged*)end;
            rng::critical_state = e->previous_state_.get();
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::sound_completed: {
            auto e = (time_stream::event::SoundCompleted*)end;
            auto name = (const char*)(intptr_t)e->sound_name_ptr_.get();
            PLATFORM.speaker().play_sound(name, 3);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::cannon_sound_completed: {
            auto e = (time_stream::event::CannonSoundCompleted*)end;
            PLATFORM.speaker().play_sound("cannon", 3);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::missile_sound_completed: {
            auto e = (time_stream::event::MissileSoundCompleted*)end;
            PLATFORM.speaker().play_sound("missile", 3);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::hit_sound_completed: {
            auto e = (time_stream::event::HitSoundCompleted*)end;
            PLATFORM.speaker().play_sound("impact", 3);
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::lightning: {
            auto e = (time_stream::event::Lightning*)end;

            for (auto& room : player_island().rooms()) {
                room->on_lightning_rewind();
            }

            if (opponent_island()) {
                for (auto& room : opponent_island()->rooms()) {
                    room->on_lightning_rewind();
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::lightning_done: {
            auto e = (time_stream::event::LightningDone*)end;
            APP.environment().rewind_lightning();
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::weather_changed: {
            auto e = (time_stream::event::WeatherChanged*)end;
            environment_init(e->prev_weather_);
            PLATFORM.screen().set_shader(APP.environment().shader());
            PLATFORM.speaker().stream_music(APP.environment().music()->c_str(),
                                            0);
            APP.time_stream().pop(sizeof *e);
            PLATFORM.screen().schedule_fade(1.f);
            PLATFORM.screen().schedule_fade(0.f);
            break;
        }


        case time_stream::event::player_room_ai_awareness: {
            auto e = (time_stream::event::PlayerRoomAiAwareness*)end;
            const RoomCoord c{e->room_x_, e->room_y_};
            if (auto room = APP.player_island().get_room(c)) {
                room->set_ai_aware(e->prev_aware_);
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::opponent_room_ai_awareness: {
            auto e = (time_stream::event::OpponentRoomAiAwareness*)end;
            const RoomCoord c{e->room_x_, e->room_y_};
            if (APP.opponent_island()) {
                if (auto room = APP.opponent_island()->get_room(c)) {
                    room->set_ai_aware(e->prev_aware_);
                }
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::explosion: {
            auto e = (time_stream::event::Explosion*)end;
            Vec2<Fixnum> pos;
            pos.x = e->x_.get();
            pos.y = e->y_.get();
            const u8 ha = e->half_angle_;
            const u8 sp = e->spd_;
            if (auto exp = APP.alloc_entity<Explosion2>(pos, ha, sp)) {
                exp->restore(*e);
                APP.effects().push(std::move(exp));
            }
            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::core_explosion: {
            auto e = (time_stream::event::CoreExplosion*)end;

            auto dt = PLATFORM.make_dynamic_texture();
            if (dt) {
                Vec2<Fixnum> p;
                p.x = e->x_.get();
                p.y = e->y_.get();
                auto make_segment = [&](int q) {
                    if (auto e =
                            APP.alloc_entity<CoreExplosionQuarter>(*dt, p, q)) {
                        e->jump_to_end();
                        return APP.effects().push(std::move(e));
                    }
                };
                make_segment(3);
                make_segment(2);
                make_segment(1);
                make_segment(0);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_small: {
            auto e = (time_stream::event::ScoreIncreasedSmall__packed*)end;
            int amount = e->amount_;
            for (int i = 0; i < e->mul_10_; ++i) {
                amount *= 10;
            }
            APP.score().set(APP.score().get() - amount);
            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_large: {
            auto e = (time_stream::event::ScoreIncreasedLarge*)end;
            int amount = e->amount_.get();
            APP.score().set(APP.score().get() - amount);
            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_huge: {
            auto e = (time_stream::event::ScoreIncreasedHuge*)end;
            int amount = e->amount_.get();
            APP.score().set(APP.score().get() - amount);
            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_decreased: {
            auto e = (time_stream::event::ScoreDecreased*)end;
            int amount = e->amount_.get();
            APP.score().set(APP.score().get() + amount);
            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::cargo_bay_contents: {
            auto e = (time_stream::event::CargoBayContents*)end;
            Island* island =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (island) {
                if (auto r = island->get_room({e->x_, e->y_})) {
                    if (auto cb = r->cast<CargoBay>()) {
                        StringBuffer<20> temp;
                        for (int i = 0; i < e->count_; ++i) {
                            temp.push_back(e->cargo_[i]);
                        }
                        cb->set_cargo(temp.c_str(), e->count_);
                    }
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::isle_phase_change: {
            auto e = (time_stream::event::IslePhaseChange*)end;

            Island* isle =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (isle) {
                isle->set_phase(e->prev_phase_);
            }

            APP.time_stream().pop(sizeof *e);
            break;
        };


        case time_stream::event::phase_shifter_state_change: {
            auto e = (time_stream::event::PhaseShifterStateChange*)end;

            Island* isle =
                e->near_ ? &APP.player_island() : APP.opponent_island();

            if (isle) {
                if (auto room = isle->get_room({e->x_, e->y_})) {
                    if (auto ps = room->cast<PhaseShifter>()) {
                        ps->rewind_state(e->prev_mode_);
                    }
                }
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::bird_left_map: {
            auto e = (time_stream::event::BirdLeftMap*)end;

            if (auto dt = PLATFORM.make_dynamic_texture()) {
                Vec2<Fixnum> pos;
                pos.x = e->x_pos_.get();
                pos.y = e->y_pos_.get();

                RoomCoord coord;
                coord.x = e->x_coord_;
                coord.y = e->y_coord_;

                const auto tm = e->flight_timer_.get();

                Float speed;
                memcpy(&speed, e->speed_, sizeof speed);

                APP.birds().push(APP.alloc_entity<GenericBird>(*dt,
                                                               coord,
                                                               pos,
                                                               speed,
                                                               tm,
                                                               (u8)e->color_,
                                                               (bool)e->near_,
                                                               (bool)e->flip_));
            }

            APP.time_stream().pop(sizeof *e);
            break;
        }


        default:
            auto err = "invalid event from time stream";
            APP.time_stream().enable_pushes(true);
            return make_scene<FatalErrorScene>(err);
        }

        if (APP.time_stream().end()) {
            end_timestamp = APP.time_stream().end_timestamp();
        } else {
            end_timestamp.reset();
        }
    }

    APP.update_parallax(-delta);

    rewind_entities(delta, APP.effects());
    rewind_entities(delta, APP.birds());

    APP.player_island().rewind(delta);
    APP.opponent_island()->rewind(delta);

    // Potential bugfix: some moved blocks caused the move region begin message
    // to age-out of the history buffer.
    if (move_region) {
        for (auto& room : APP.player_island().rooms()) {
            room->set_hidden(false);
        }
        for (auto& room : APP.opponent_island()->rooms()) {
            room->set_hidden(false);
        }
    }

    return null_scene();
}



void RewindScene::enter(Scene& prev)
{
    if (not APP.time_stream().pushes_enabled()) {
        Platform::fatal("entering rewind scene with recording disabled");
    }

    if (auto p = prev.cast_world_scene()) {
        p->set_gamespeed(GameSpeed::rewind);
    } else {
        Platform::fatal("entering rewind scene from non-overworld scene");
    }

    speed_text1_.emplace("2x", OverlayCoord{1, 16});
    speed_text2_.emplace("4x", OverlayCoord{1, 17});
    speed_text3_.emplace("8x", OverlayCoord{1, 18});

    PLATFORM.set_tile(Layer::overlay, 0, 16, 475);
    PLATFORM.set_tile(Layer::overlay, 0, 17, 112);
    PLATFORM.set_tile(Layer::overlay, 0, 18, 112);
}



void RewindScene::exit(Scene& next)
{
    speed_text1_.reset();
    speed_text2_.reset();
    speed_text3_.reset();

    PLATFORM.set_tile(Layer::overlay, 0, 16, 0);
    PLATFORM.set_tile(Layer::overlay, 0, 17, 0);
    PLATFORM.set_tile(Layer::overlay, 0, 18, 0);

    if (not APP.time_stream().pushes_enabled()) {
        Platform::fatal("sanity check: exit rewind scene, pushes not enabled");
    }

    if (auto p = next.cast_world_scene()) {
        p->set_gamespeed(GameSpeed::stopped);
    } else {
        if (APP.is_developer_mode()) {
            Platform::fatal("exiting rewind scene into non-overworld scene");
        }
    }

    // Reset update dispatch when coming out of a rewind. Perhaps unnecessary.
    APP.player_island().cancel_dispatch();

    if (APP.opponent_island()) {
        APP.opponent_island()->cancel_dispatch();
    }

    for (auto& room : APP.player_island().rooms()) {
        room->update_description();
    }

    if (APP.opponent_island()) {
        for (auto& room : APP.opponent_island()->rooms()) {
            room->update_description();
        }
    }
}



void RewindScene::display()
{
    APP.player_island().display();
    APP.player_island().display_fires();

    if (APP.opponent_island()) {
        APP.opponent_island()->display();
        APP.opponent_island()->display_fires();
    }

    APP.environment().display();

    for (auto& effect : APP.effects()) {
        PLATFORM.screen().draw(effect->sprite());
    }

    for (auto& bird : APP.birds()) {
        PLATFORM.screen().draw(bird->sprite());
    }
}



} // namespace skyland
