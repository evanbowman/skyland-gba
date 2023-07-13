////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/mindControl.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"


namespace skyland
{



template <typename T, typename E, typename F, typename... Args>
T* respawn_basic_projectile(Platform& pfrm,
                            App& app,
                            Island* parent,
                            const E& e,
                            F&& explosion_function,
                            Args&&... args)
{
    auto c =
        app.alloc_entity<T>(pfrm,
                            Vec2<Fixnum>{Fixnum::from_integer(e.x_pos_.get()),
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
        explosion_function(pfrm, app, c->sprite().get_position());
        auto ret = c.get();
        parent->projectiles().push(std::move(c));
        return ret;
    }
    return nullptr;
}



void respawn_plugin_projectile(
    Platform& pfrm,
    App& app,
    Island* parent,
    const time_stream::event::PluginProjectileDestroyed& e)
{
    auto c = app.alloc_entity<PluginProjectile>(
        pfrm,
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
        medium_explosion_inv(pfrm, app, c->sprite().get_position());
        parent->projectiles().push(std::move(c));
    }
}



void respawn_missile(Platform& pfrm,
                     App& app,
                     Island* parent,
                     time_stream::event::MissileDestroyed& e)
{
    auto m = app.alloc_entity<Missile>(
        pfrm,
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

        medium_explosion_inv(pfrm, app, m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void respawn_rocketbomb(Platform& pfrm,
                        App& app,
                        Island* parent,
                        time_stream::event::MissileDestroyed& e)
{
    auto m = app.alloc_entity<RocketBomb>(
        pfrm,
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

        medium_explosion_inv(pfrm, app, m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void respawn_clumpmissile(Platform& pfrm,
                          App& app,
                          Island* parent,
                          time_stream::event::MissileDestroyed& e)
{
    auto m = app.alloc_entity<ClumpMissile>(
        pfrm,
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

        medium_explosion_inv(pfrm, app, m->sprite().get_position());

        parent->projectiles().push(std::move(m));
    }
}



void RewindScene::print_timestamp(Platform& pfrm, App& app)
{
    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
    }
    const auto level_seconds = app.level_timer().whole_seconds();
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



void environment_init(App& app, int type);



void restore_boarding_pod_entity(Platform& pfrm,
                                 App& app,
                                 Room& src,
                                 time_stream::event::BoardingPodLanded& e);



ScenePtr<Scene> RewindScene::update(Platform& pfrm, App& app, Microseconds)
{
    bool speed_changed = false;

    if (app.player().key_down(pfrm, Key::up) and speed_ > 0) {
        --speed_;
        speed_changed = true;
    }
    if (app.player().key_down(pfrm, Key::down) and speed_ < 3) {
        ++speed_;
        speed_changed = true;
    }

    if (speed_changed) {
        switch (speed_) {
        case 0:
            pfrm.set_tile(Layer::overlay, 0, 16, 475);
            pfrm.set_tile(Layer::overlay, 0, 17, 112);
            pfrm.set_tile(Layer::overlay, 0, 18, 112);
            pfrm.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed);
            break;

        case 1:
            pfrm.set_tile(Layer::overlay, 0, 16, 112);
            pfrm.set_tile(Layer::overlay, 0, 17, 475);
            pfrm.set_tile(Layer::overlay, 0, 18, 112);
            pfrm.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed4x);
            break;

        case 2:
            pfrm.set_tile(Layer::overlay, 0, 16, 112);
            pfrm.set_tile(Layer::overlay, 0, 17, 112);
            pfrm.set_tile(Layer::overlay, 0, 18, 475);
            pfrm.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::reversed8x);
            break;
        }
    }

    // Playback history at a fixed delta.
    Microseconds delta = 2 * (seconds(1) / 60);
    if (speed_ == 1) {
        delta *= 2;
    } else if (speed_ == 2) {
        delta *= 4;
    }

    app.delta_fp() = delta;


    // Some functions called in this block may try to call other functions that
    // push events to the time stream, which we don't want to do, as we're
    // rewinding.
    app.time_stream().enable_pushes(false);


    app.level_timer().count_down(delta);
    app.time_stream().rewind(delta);


    app.environment().rewind(pfrm, app, delta);


    print_timestamp(pfrm, app);


    bool move_region = false;

    if (far_camera_) {
        auto& cursor_loc = globals().far_cursor_loc_;
        app.camera()->update(
            pfrm, app, *app.opponent_island(), cursor_loc, delta, false);
    } else {
        auto& cursor_loc = globals().near_cursor_loc_;
        app.camera()->update(
            pfrm, app, app.player_island(), cursor_loc, delta, true);
    }



    const auto current_timestamp = app.level_timer().total();
    auto end_timestamp = app.time_stream().end_timestamp();

    if (not app.opponent_island() or not end_timestamp or
        app.player().key_down(pfrm, Key::alt_1)) {

        app.time_stream().enable_pushes(true);

        if (not end_timestamp) {
            // If we've rewound all of the way, enqueue another Initial event.
            // The event really contains nothing at all, but designates the
            // beginning of the event sequence, giving us something to rewind
            // to. Our implementation only allows rewinding until the earliest
            // existing event.
            time_stream::event::Initial e;
            app.time_stream().push(app.level_timer(), e);
        }

        if (far_camera_) {
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }

    // NOTE: IMPORTANT: any code called in this loop should not push a
    // time_stream event! You could get stuck in an endless loop!
    while (end_timestamp and ((*end_timestamp > current_timestamp) or
                              (app.level_timer().total() == 0))) {
        auto end = app.time_stream().end();
        switch ((time_stream::event::Type)end->type_) {

        case time_stream::event::Type::initial: {
            app.time_stream().pop(sizeof(time_stream::event::Initial));
            break;
        }


        case time_stream::event::Type::achievement: {
            auto e = (time_stream::event::Achievement*)end;
            achievements::lock(pfrm, app, e->which_);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_created: {
            auto e = (time_stream::event::PlayerRoomCreated*)end;
            app.player_island().destroy_room(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_created: {
            auto e = (time_stream::event::OpponentRoomCreated*)end;
            app.opponent_island()->destroy_room(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_destroyed: {
            auto e = (time_stream::event::PlayerRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &app.player_island(), {e->x_, e->y_});
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(pfrm, app, room->origin());
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_destroyed_with_group: {
            auto e = (time_stream::event::PlayerRoomDestroyedWithGroup*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &app.player_island(), {e->x_, e->y_});
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(pfrm, app, room->origin());
                }
                room->set_group((Room::Group)e->group_);
                app.player_island().repaint(pfrm, app);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_destroyed: {
            auto e = (time_stream::event::OpponentRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, app.opponent_island(), {e->x_, e->y_});
            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                const bool quiet = (*room->metaclass())->properties() &
                                   RoomProperties::destroy_quietly;
                if (not quiet) {
                    medium_explosion_inv(pfrm, app, room->origin());
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_transmuted: {
            auto e = (time_stream::event::PlayerRoomTransmuted*)end;

            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                room->__unsafe__transmute(pfrm, app, e->prev_type_);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_transmuted: {
            auto e = (time_stream::event::OpponentRoomTransmuted*)end;

            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                room->__unsafe__transmute(pfrm, app, e->prev_type_);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_salvaged: {
            auto e = (time_stream::event::PlayerRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &app.player_island(), {e->x_, e->y_});
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                if (room->metaclass_index() == e->type_) { // sanity check
                    room->__set_health(e->health_.get());
                    room->set_group((Room::Group)e->group_);
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_salvaged: {
            auto e = (time_stream::event::OpponentRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, app.opponent_island(), {e->x_, e->y_});
            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                if (room->metaclass_index() == e->type_) {
                    room->__set_health(e->health_.get());
                    room->set_group((Room::Group)e->group_);
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_moved: {
            auto e = (time_stream::event::PlayerRoomMoved*)end;
            if (auto r = app.player_island().get_room({e->x_, e->y_})) {
                app.player_island().move_room(
                    pfrm, app, {e->x_, e->y_}, {e->prev_x_, e->prev_y_});
                if (move_region) {
                    r->set_hidden(true);
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_moved: {
            auto e = (time_stream::event::OpponentRoomMoved*)end;
            if (auto r = app.opponent_island()->get_room({e->x_, e->y_})) {
                app.opponent_island()->move_room(
                    pfrm, app, {e->x_, e->y_}, {e->prev_x_, e->prev_y_});
                if (move_region) {
                    r->set_hidden(true);
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::move_region_begin: {
            auto e = (time_stream::event::MoveRegionBegin*)end;
            move_region = false;
            for (auto& room : app.player_island().rooms()) {
                room->set_hidden(false);
            }
            for (auto& room : app.opponent_island()->rooms()) {
                room->set_hidden(false);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::move_region_end: {
            auto e = (time_stream::event::MoveRegionEnd*)end;
            move_region = true;
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_fire_created: {
            auto e = (time_stream::event::PlayerFireCreated*)end;
            app.player_island().fire_extinguish(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_fire_created: {
            auto e = (time_stream::event::OpponentFireCreated*)end;
            app.opponent_island()->fire_extinguish(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_fire_extinguished: {
            auto e = (time_stream::event::PlayerFireExtinguished*)end;
            app.player_island().fire_create(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_fire_extinguished: {
            auto e = (time_stream::event::OpponentFireExtinguished*)end;
            app.opponent_island()->fire_create(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_plundered:
        case time_stream::event::Type::opponent_room_plundered: {
            auto e = (time_stream::event::RoomPlundered*)end;
            Buffer<EntityRef<BasicCharacter>, 16> chrs;

            Island* island =
                (time_stream::event::Type)end->type_ ==
                        time_stream::event::Type::player_room_plundered
                    ? &app.player_island()
                    : app.opponent_island();

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
                        island->destroy_room(pfrm, app, {x, y});
                    }
                }
            }

            // Re-create the original room.
            (*meta)->create(pfrm, app, island, RoomCoord{e->x_, e->y_});

            // Add characters back.
            if (auto room = island->get_room({e->x_, e->y_})) {
                for (auto& chr : chrs) {
                    room->edit_characters().push(std::move(chr));
                }
            } else {
                auto err = "rewind salvage: attempt to re-attach character"
                           " to non-existent room.";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_nemesis_blast_destroyed: {
            auto e = (time_stream::event::PlayerNemesisBlastDestroyed*)end;
            if (auto v = respawn_basic_projectile<NemesisBlast>(
                    pfrm,
                    app,
                    &app.player_island(),
                    *e,
                    medium_explosion_inv)) {
                v->set_variant(e->variant_);
                app.camera()->shake(8);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_nemesis_blast_destroyed: {
            auto e = (time_stream::event::PlayerNemesisBlastDestroyed*)end;
            if (auto v = respawn_basic_projectile<NemesisBlast>(
                    pfrm,
                    app,
                    app.opponent_island(),
                    *e,
                    medium_explosion_inv)) {
                v->set_variant(e->variant_);
                app.camera()->shake(8);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_plugin_projectile_destroyed: {
            auto e = (time_stream::event::PlayerPluginProjectileDestroyed*)end;
            respawn_plugin_projectile(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_plugin_projectile_destroyed: {
            auto e =
                (time_stream::event::OpponentPluginProjectileDestroyed*)end;
            respawn_plugin_projectile(pfrm, app, app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_cannonball_destroyed: {
            auto e = (time_stream::event::PlayerCannonballDestroyed*)end;
            respawn_basic_projectile<Cannonball>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_cannonball_destroyed: {
            auto e = (time_stream::event::OpponentCannonballDestroyed*)end;
            respawn_basic_projectile<Cannonball>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_incineratorbolt_destroyed: {
            auto e = (time_stream::event::PlayerIncineratorboltDestroyed*)end;
            respawn_basic_projectile<IncineratorBolt>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(14);
            break;
        }


        case time_stream::event::Type::opponent_incineratorbolt_destroyed: {
            auto e = (time_stream::event::OpponentIncineratorboltDestroyed*)end;
            respawn_basic_projectile<IncineratorBolt>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(14);
            break;
        }


        case time_stream::event::Type::player_beam_destroyed: {
            auto e = (time_stream::event::PlayerBeamDestroyed*)end;
            auto t = respawn_basic_projectile<Beam>(pfrm,
                                                    app,
                                                    &app.player_island(),
                                                    *e,
                                                    medium_explosion_inv,
                                                    e->index_);
            if (t) {
                t->restore_blocks_hit(*e);
            }
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_beam_destroyed: {
            auto e = (time_stream::event::OpponentBeamDestroyed*)end;
            auto t = respawn_basic_projectile<Beam>(pfrm,
                                                    app,
                                                    app.opponent_island(),
                                                    *e,
                                                    medium_explosion_inv,
                                                    e->index_);
            if (t) {
                t->restore_blocks_hit(*e);
            }
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_arcbolt_destroyed: {
            auto e = (time_stream::event::PlayerArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_arcbolt_destroyed: {
            auto e = (time_stream::event::OpponentArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_firebolt_destroyed: {
            auto e = (time_stream::event::PlayerFireboltDestroyed*)end;
            respawn_basic_projectile<FireBolt>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_firebolt_destroyed: {
            auto e = (time_stream::event::OpponentFireboltDestroyed*)end;
            respawn_basic_projectile<FireBolt>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_flak_destroyed: {
            auto e = (time_stream::event::PlayerFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_flak_destroyed: {
            auto e = (time_stream::event::OpponentFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_ionburst_destroyed: {
            auto e = (time_stream::event::PlayerIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_ionburst_destroyed: {
            auto e = (time_stream::event::OpponentIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_antimatter_destroyed: {
            auto e = (time_stream::event::PlayerAntimatterDestroyed*)end;
            respawn_basic_projectile<Antimatter>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::opponent_antimatter_destroyed: {
            auto e = (time_stream::event::OpponentAntimatterDestroyed*)end;
            respawn_basic_projectile<Antimatter>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(8);
            break;
        }


        case time_stream::event::Type::player_decimator_burst_destroyed: {
            auto e = (time_stream::event::PlayerDecimatorBurstDestroyed*)end;
            respawn_basic_projectile<DecimatorBurst>(
                pfrm, app, &app.player_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(26);
            break;
        }


        case time_stream::event::Type::opponent_decimator_burst_destroyed: {
            auto e = (time_stream::event::OpponentDecimatorBurstDestroyed*)end;
            respawn_basic_projectile<DecimatorBurst>(
                pfrm, app, app.opponent_island(), *e, medium_explosion_inv);
            app.time_stream().pop(sizeof *e);
            app.camera()->shake(26);
            break;
        }


        case time_stream::event::Type::player_missile_destroyed: {
            auto e = (time_stream::event::PlayerMissileDestroyed*)end;
            respawn_missile(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_missile_destroyed: {
            auto e = (time_stream::event::OpponentMissileDestroyed*)end;
            respawn_missile(pfrm, app, app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_rocketbomb_destroyed: {
            auto e = (time_stream::event::PlayerRocketBombDestroyed*)end;
            respawn_rocketbomb(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_rocketbomb_destroyed: {
            auto e = (time_stream::event::OpponentRocketBombDestroyed*)end;
            respawn_rocketbomb(pfrm, app, app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_clumpmissile_destroyed: {
            auto e = (time_stream::event::PlayerClumpMissileDestroyed*)end;
            respawn_clumpmissile(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_clumpmissile_destroyed: {
            auto e = (time_stream::event::OpponentClumpMissileDestroyed*)end;
            respawn_clumpmissile(pfrm, app, app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_damaged_small: {
            auto e = (time_stream::event::PlayerRoomDamagedSmall*)end;
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(room->health() + e->diff_);
                room->reset_injured_timer(1);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_damaged_small: {
            auto e = (time_stream::event::OpponentRoomDamagedSmall*)end;
            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(room->health() + e->diff_);
                room->reset_injured_timer(1);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_damaged: {
            auto e = (time_stream::event::PlayerRoomDamaged*)end;
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
                room->reset_injured_timer(1);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_damaged: {
            auto e = (time_stream::event::OpponentRoomDamaged*)end;
            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
                room->reset_injured_timer(1);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_repaired: {
            auto e = (time_stream::event::PlayerRoomRepaired*)end;
            if (auto room = app.player_island().get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_repaired: {
            auto e = (time_stream::event::OpponentRoomRepaired*)end;
            if (auto room = app.opponent_island()->get_room({e->x_, e->y_})) {
                room->__set_health(e->previous_health_.get());
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::coins_changed: {
            auto e = (time_stream::event::CoinsChanged*)end;
            app.set_coins(pfrm, e->previous_value_.get());
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_movement_path_assigned: {
            auto e = (time_stream::event::CharacterMovementPathAssigned*)end;
            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->drop_movement_path();
            } else {
                auto err = "rewind chr mv path asgn: invalid chr id!";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_moved: {
            auto e = (time_stream::event::CharacterMoved*)end;
            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->rewind_movement_step(pfrm,
                                          {e->previous_x_, e->previous_y_});
            } else {
                auto err = "rewind chr mv path asgn: invalid chr id! (2)";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_died: {
            auto e = (time_stream::event::CharacterDied*)end;

            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            Player* owner =
                e->owned_by_player_ ? &app.player() : &app.opponent();

            const bool is_replicant = e->is_replicant_;

            auto chr = app.alloc_entity<BasicCharacter>(
                pfrm, island, owner, RoomCoord{e->x_, e->y_}, is_replicant);

            chr->__assign_id(e->id_.get());
            chr->set_race(e->race_);

            island->add_character(std::move(chr));

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::replicant_created: {
            auto e = (time_stream::event::ReplicantCreated*)end;

            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            if (auto room = island->get_room({e->x_, e->y_})) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();) {
                    const bool player_chr = (*it)->owner() == &app.player();
                    if (player_chr == e->owned_by_player_ and
                        (*it)->grid_position() == RoomCoord{e->x_, e->y_}) {
                        if ((*it)->is_replicant()) {
                            room->edit_characters().erase(it);
                        } else {
                            auto err = "rewind error: rewind replicant"
                                       "is not replicant?!";
                            return scene_pool::alloc<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_health_changed: {
            auto e = (time_stream::event::CharacterHealthChanged*)end;

            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->__set_health(e->previous_health_);
            } else {
                auto err = "rewind chr health changed: invalid chr id!";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_transported: {
            auto e = (time_stream::event::CharacterTransported*)end;

            Island* source_island =
                e->source_near_ ? &app.player_island() : app.opponent_island();

            Island* dest_island = not e->source_near_ ? &app.player_island()
                                                      : app.opponent_island();

            // Ok, so here, we want to revert the transport. Find the
            // transported character at the destination island, and move it back
            // to the original location at the source island.

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                auto err = "rewind chr_transported: Invalid character id!";
                return scene_pool::alloc<FatalErrorScene>(err);
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
                            source_room->___rewind___ability_used(pfrm, app);

                        } else {
                            auto err = "fatal error when rewinding "
                                       "transport: source room missing.";
                            return scene_pool::alloc<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                auto err = "error rewinding transport: dest room missing.";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_disembark: {
            auto e = (time_stream::event::CharacterDisembark*)end;

            Island* source_island =
                e->chr_near_ ? &app.player_island() : app.opponent_island();

            Island* dest_island =
                not e->chr_near_ ? &app.player_island() : app.opponent_island();

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                auto err = "rewind chr_disembark: Invalid character id!";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            auto dest_room = chr_info.second;
            if (dest_room) {
                for (auto it = dest_room->characters().begin();
                     it not_eq dest_room->characters().end();) {

                    if ((*it).get() == chr_info.first) {

                        // If the character disembarked, it must have ended up
                        // in a transporter. As we're rewinding things, give the
                        // transporter back its transport.
                        dest_room->___rewind___ability_used(pfrm, app);

                        auto detached = std::move(*it);
                        dest_room->edit_characters().erase(it);
                        const RoomCoord pos{e->previous_x_, e->previous_y_};
                        detached->set_grid_position(pos);
                        detached->set_parent(source_island);
                        detached->drop_movement_path();
                        detached->set_idle(app);
                        if (auto source_room = source_island->get_room(pos)) {
                            source_room->edit_characters().push(
                                std::move(detached));
                        } else {
                            auto err = "fatal error when rewinding "
                                       "disembark: source room missing";
                            return scene_pool::alloc<FatalErrorScene>(err);
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                auto err = "error rewinding disembark: dest room missing";
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::weapon_set_group: {
            auto e = (time_stream::event::WeaponSetGroup*)end;
            if (auto room =
                    app.player_island().get_room({e->room_x_, e->room_y_})) {
                room->set_group((Room::Group)e->prev_group_);
                app.player_island().repaint(pfrm, app);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::weapon_set_target: {
            auto e = (time_stream::event::WeaponSetTarget*)end;

            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            if (auto room = island->get_room({e->room_x_, e->room_y_})) {
                if (e->has_previous_target_) {
                    room->set_target(pfrm,
                                     app,
                                     RoomCoord{e->previous_target_x_,
                                               e->previous_target_y_});
                } else {
                    room->unset_target(pfrm, app);
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_reload_complete: {
            auto e = (time_stream::event::PlayerRoomReloadComplete*)end;
            if (auto room =
                    app.player_island().get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload(pfrm, app);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_reload_complete: {
            auto e = (time_stream::event::OpponentRoomReloadComplete*)end;
            if (auto room =
                    app.opponent_island()->get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload(pfrm, app);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_island_drift_changed: {
            auto e = (time_stream::event::OpponentIslandDriftChanged*)end;
            auto val = Fixnum::create(e->previous_speed__data_.get());
            app.opponent_island()->set_drift(pfrm, app, val);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::island_terrain_changed: {
            auto e = (time_stream::event::IslandTerrainChanged*)end;

            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

            island->init_terrain(pfrm, e->previous_terrain_size_);
            island->repaint(pfrm, app);

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_deployed: {
            auto e = (time_stream::event::DroneDeployed*)end;

            Island* dest_island = e->destination_near_ ? &app.player_island()
                                                       : app.opponent_island();

            for (auto& drone_sp : dest_island->drones()) {
                if (drone_sp->position().x == e->x_pos_ and
                    drone_sp->position().y == e->y_pos_) {

                    drone_sp->__override_state(Drone::State::launch,
                                               e->duration_.get(),
                                               e->duration_.get());
                    break;
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_health_changed: {
            auto e = (time_stream::event::DroneHealthChanged*)end;

            Island* dest_island = e->destination_near_ ? &app.player_island()
                                                       : app.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    drone->__set_health(e->previous_health_.get());
                    break;
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_set_target: {
            auto e = (time_stream::event::DroneSetTarget*)end;

            Island* dest_island = e->destination_near_ ? &app.player_island()
                                                       : app.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    if (e->has_previous_target_) {
                        drone->set_target(
                            pfrm,
                            app,
                            {e->previous_target_x_, e->previous_target_y_},
                            e->previous_target_near_);
                    } else {
                        drone->drop_target(pfrm, app);
                    }
                    break;
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_reload_complete: {
            auto e = (time_stream::event::DroneReloadComplete*)end;

            Island* dest_island = e->destination_near_ ? &app.player_island()
                                                       : app.opponent_island();

            for (auto& drone : dest_island->drones()) {
                if (drone->position().x == e->x_pos_ and
                    drone->position().y == e->y_pos_) {

                    drone->___rewind___finished_reload(pfrm, app);
                    break;
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::drone_destroyed: {
            auto e = (time_stream::event::DroneDestroyed*)end;

            Island* dest_island = e->destination_near_ ? &app.player_island()
                                                       : app.opponent_island();

            Island* parent_island =
                e->parent_near_ ? &app.player_island() : app.opponent_island();

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

                if (auto room =
                        parent_island->get_room({e->db_x_pos_, e->db_y_pos_})) {
                    if (room->attach_drone(pfrm, app, *drone)) {
                        dest_island->drones().push(*drone);
                    } else {
                        auto err = "rewind: attempt to attach drone to non"
                                   " drone-bay";
                        return scene_pool::alloc<FatalErrorScene>(err);
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
                return scene_pool::alloc<FatalErrorScene>(err);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::rng_changed: {
            auto e = (time_stream::event::RngChanged*)end;
            rng::critical_state = e->previous_state_.get();
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::sound_completed: {
            auto e = (time_stream::event::SoundCompleted*)end;
            auto name = (const char*)(intptr_t)e->sound_name_ptr_.get();
            pfrm.speaker().play_sound(name, 3);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::cannon_sound_completed: {
            auto e = (time_stream::event::CannonSoundCompleted*)end;
            pfrm.speaker().play_sound("cannon", 3);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::missile_sound_completed: {
            auto e = (time_stream::event::MissileSoundCompleted*)end;
            pfrm.speaker().play_sound("missile", 3);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::hit_sound_completed: {
            auto e = (time_stream::event::HitSoundCompleted*)end;
            pfrm.speaker().play_sound("impact", 3);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::lightning: {
            auto e = (time_stream::event::Lightning*)end;

            for (auto& room : player_island(app).rooms()) {
                room->on_lightning_rewind(pfrm, app);
            }

            if (opponent_island(app)) {
                for (auto& room : opponent_island(app)->rooms()) {
                    room->on_lightning_rewind(pfrm, app);
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::lightning_done: {
            auto e = (time_stream::event::LightningDone*)end;
            app.environment().rewind_lightning();
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::weather_changed: {
            auto e = (time_stream::event::WeatherChanged*)end;
            environment_init(app, e->prev_weather_);
            pfrm.screen().set_shader(app.environment().shader(app));
            pfrm.speaker().play_music(app.environment().music(), 0);
            app.time_stream().pop(sizeof *e);
            pfrm.screen().schedule_fade(1.f);
            pfrm.screen().schedule_fade(0.f);
            break;
        }


        case time_stream::event::player_room_ai_awareness: {
            auto e = (time_stream::event::PlayerRoomAiAwareness*)end;
            const RoomCoord c{e->room_x_, e->room_y_};
            if (auto room = app.player_island().get_room(c)) {
                room->set_ai_aware(pfrm, app, e->prev_aware_);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::opponent_room_ai_awareness: {
            auto e = (time_stream::event::OpponentRoomAiAwareness*)end;
            const RoomCoord c{e->room_x_, e->room_y_};
            if (app.opponent_island()) {
                if (auto room = app.opponent_island()->get_room(c)) {
                    room->set_ai_aware(pfrm, app, e->prev_aware_);
                }
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::explosion: {
            auto e = (time_stream::event::Explosion*)end;
            Vec2<Fixnum> pos;
            pos.x = e->x_.get();
            pos.y = e->y_.get();
            const u8 ha = e->half_angle_;
            const u8 sp = e->spd_;
            if (auto exp = app.alloc_entity<Explosion2>(pfrm, pos, ha, sp)) {
                exp->restore(*e);
                app.effects().push(std::move(exp));
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::core_explosion: {
            auto e = (time_stream::event::CoreExplosion*)end;

            auto dt = pfrm.make_dynamic_texture();
            if (dt) {
                Vec2<Fixnum> p;
                p.x = e->x_.get();
                p.y = e->y_.get();
                auto make_segment = [&](int q) {
                    if (auto e = app.alloc_entity<CoreExplosionQuarter>(
                            pfrm, *dt, p, q)) {
                        e->jump_to_end();
                        return app.effects().push(std::move(e));
                    }
                };
                make_segment(3);
                make_segment(2);
                make_segment(1);
                make_segment(0);
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::boarding_pod_landed: {
            auto e = (time_stream::event::BoardingPodLanded*)end;
            auto dt = pfrm.make_dynamic_texture();
            RoomCoord c;
            c.x = e->room_x_;
            c.y = e->room_y_;
            Room* room = nullptr;
            Island* island = nullptr;
            if (e->source_near_) {
                island = app.opponent_island();
            } else {
                island = &app.player_island();
            }
            if (island) {
                room = island->get_room(c);
            }
            if (room) {
                restore_boarding_pod_entity(pfrm, app, *room, *e);
                island->destroy_room(pfrm, app, c);
            }
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_small: {
            auto e = (time_stream::event::ScoreIncreasedSmall__packed*)end;
            int amount = e->amount_;
            for (int i = 0; i < e->mul_10_; ++i) {
                amount *= 10;
            }
            app.score().set(app.score().get() - amount);
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_large: {
            auto e = (time_stream::event::ScoreIncreasedLarge*)end;
            int amount = e->amount_.get();
            app.score().set(app.score().get() - amount);
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_increased_huge: {
            auto e = (time_stream::event::ScoreIncreasedHuge*)end;
            int amount = e->amount_.get();
            app.score().set(app.score().get() - amount);
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::score_decreased: {
            auto e = (time_stream::event::ScoreDecreased*)end;
            int amount = e->amount_.get();
            app.score().set(app.score().get() + amount);
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::mind_control_started: {
            auto e = (time_stream::event::MindControlStarted*)end;
            // Island* ctrl_island = nullptr;
            // if (e->controller_near_) {
            //     ctrl_island = &app.player_island();
            // } else {
            //     ctrl_island = app.opponent_island();
            // }
            // if (ctrl_island) {
            //     Vec2<u8> pos{e->controller_x_,
            //                  e->controller_y_};
            //     if (auto room = ctrl_island->get_room(pos)) {
            //         if (auto ctrl = room->cast<MindControl>()) {
            //             auto current = ctrl->bound_character();
            //             ctrl->bind_character(e->prev_id_.get());
            //             auto [chr, room] = BasicCharacter::find_by_id(app, current);
            //             if (chr) {
            //                 Player* owner = &app.opponent();
            //                 if (ctrl_island == app.opponent_island()) {
            //                     owner = &app.player();
            //                 }
            //                 chr->stop_mind_control(app, owner, room);
            //             }
            //         }
            //     }
            // }
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::mind_control_stopped: {
            auto e = (time_stream::event::MindControlStopped*)end;
            // Island* ctrl_island = nullptr;
            // if (e->controller_near_) {
            //     ctrl_island = &app.player_island();
            // } else {
            //     ctrl_island = app.opponent_island();
            // }
            // if (ctrl_island) {
            //     Vec2<u8> pos{e->controller_x_, e->controller_y_};
            //     if (auto room = ctrl_island->get_room(pos)) {
            //         if (auto ctrl = room->cast<MindControl>()) {
            //             ctrl->bind_character(e->id_.get());
            //             auto [chr, room] = BasicCharacter::find_by_id(app, e->id_.get());
            //             if (chr) {
            //                 Player* owner = &app.player();
            //                 if (ctrl_island == app.opponent_island()) {
            //                     owner = &app.opponent();
            //                 }
            //                 chr->start_mind_control(app, owner, room);
            //             }
            //         }
            //     }
            // }
            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::cargo_bay_contents: {
            auto e = (time_stream::event::CargoBayContents*)end;
            Island* island =
                e->near_ ? &app.player_island() : app.opponent_island();

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

            app.time_stream().pop(sizeof *e);
            break;
        }

        case time_stream::event::bird_left_map: {
            auto e = (time_stream::event::BirdLeftMap*)end;

            if (auto dt = pfrm.make_dynamic_texture()) {
                Vec2<Fixnum> pos;
                pos.x = e->x_pos_.get();
                pos.y = e->y_pos_.get();

                RoomCoord coord;
                coord.x = e->x_coord_;
                coord.y = e->y_coord_;

                const auto tm = e->flight_timer_.get();

                Float speed;
                memcpy(&speed, e->speed_, sizeof speed);

                app.birds().push(app.alloc_entity<GenericBird>(pfrm,
                                                               *dt,
                                                               coord,
                                                               pos,
                                                               speed,
                                                               tm,
                                                               (u8)e->color_,
                                                               (bool)e->near_,
                                                               (bool)e->flip_));
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        default:
            auto err = "invalid event from time stream";
            return scene_pool::alloc<FatalErrorScene>(err);
        }

        if (app.time_stream().end()) {
            end_timestamp = app.time_stream().end_timestamp();
        } else {
            end_timestamp.reset();
        }
    }

    app.update_parallax(-delta);

    rewind_entities(pfrm, app, delta, app.effects());
    rewind_entities(pfrm, app, delta, app.birds());

    app.player_island().rewind(pfrm, app, delta);
    app.opponent_island()->rewind(pfrm, app, delta);

    // Potential bugfix: some moved blocks caused the move region begin message
    // to age-out of the history buffer.
    if (move_region) {
        for (auto& room : app.player_island().rooms()) {
            room->set_hidden(false);
        }
        for (auto& room : app.opponent_island()->rooms()) {
            room->set_hidden(false);
        }
    }

    return null_scene();
}



void RewindScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not app.time_stream().pushes_enabled()) {
        Platform::fatal("entering rewind scene with recording disabled");
    }

    if (auto p = prev.cast_world_scene()) {
        p->set_gamespeed(pfrm, app, GameSpeed::rewind);
    } else {
        Platform::fatal("entering rewind scene from non-overworld scene");
    }

    speed_text1_.emplace(pfrm, "2x", OverlayCoord{1, 16});
    speed_text2_.emplace(pfrm, "4x", OverlayCoord{1, 17});
    speed_text3_.emplace(pfrm, "8x", OverlayCoord{1, 18});

    pfrm.set_tile(Layer::overlay, 0, 16, 475);
    pfrm.set_tile(Layer::overlay, 0, 17, 112);
    pfrm.set_tile(Layer::overlay, 0, 18, 112);
}



void RewindScene::exit(Platform& pfrm, App& app, Scene& next)
{
    speed_text1_.reset();
    speed_text2_.reset();
    speed_text3_.reset();

    pfrm.set_tile(Layer::overlay, 0, 16, 0);
    pfrm.set_tile(Layer::overlay, 0, 17, 0);
    pfrm.set_tile(Layer::overlay, 0, 18, 0);

    if (not app.time_stream().pushes_enabled()) {
        Platform::fatal("sanity check: exit rewind scene, pushes not enabled");
    }

    if (auto p = next.cast_world_scene()) {
        p->set_gamespeed(pfrm, app, GameSpeed::stopped);
    } else {
        Platform::fatal("exiting rewind scene into non-overworld scene");
    }

    // Reset update dispatch when coming out of a rewind. Perhaps unnecessary.
    app.player_island().cancel_dispatch();

    if (app.opponent_island()) {
        app.opponent_island()->cancel_dispatch();
    }

    for (auto& room : app.player_island().rooms()) {
        room->update_description();
    }

    if (app.opponent_island()) {
        for (auto& room : app.opponent_island()->rooms()) {
            room->update_description();
        }
    }
}



void RewindScene::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm, app);
    app.player_island().display_fires(pfrm);

    if (app.opponent_island()) {
        app.opponent_island()->display(pfrm, app);
        app.opponent_island()->display_fires(pfrm);
    }

    app.environment().display(pfrm, app);

    for (auto& effect : app.effects()) {
        pfrm.screen().draw(effect->sprite());
    }

    for (auto& bird : app.birds()) {
        pfrm.screen().draw(bird->sprite());
    }
}



} // namespace skyland
