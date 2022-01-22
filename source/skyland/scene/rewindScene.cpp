#include "rewindScene.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/entity/projectile/projectile.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



template <typename T>
void respawn_basic_projectile(Platform& pfrm,
                              App& app,
                              Island* parent,
                              time_stream::event::BasicProjectileDestroyed& e)
{
    auto c = alloc_entity<T>(
        Vec2<Float>{(Float)e.x_pos_.get(), (Float)e.y_pos_.get()},
        Vec2<Float>{},
        parent,
        Vec2<u8>{e.x_origin_, e.y_origin_});
    if (c) {
        Vec2<Float> step_vector;
        memcpy(&step_vector.x, e.x_speed_, sizeof(Float));
        memcpy(&step_vector.y, e.y_speed_, sizeof(Float));
        c->set_step_vector(step_vector);
        c->set_timer(e.timer_.get());
        parent->projectiles().push(std::move(c));
    }
}



void respawn_missile(Platform& pfrm,
                     App& app,
                     Island* parent,
                     time_stream::event::MissileDestroyed& e)
{
    auto m = alloc_entity<Missile>(Vec2<Float>{
            (Float)e.x_pos_.get(),
            (Float)e.y_pos_.get()
        },
        Vec2<Float>{
            (Float)e.target_x_.get(),
            0.f // TODO: change target parameter to simple float, y unused.
        },
        (u8)e.source_x_,
        (u8)e.source_y_,
        parent);

    if (m) {
        m->set_timer(e.timer_.get());
        m->set_state((Missile::State)e.state_);

        parent->projectiles().push(std::move(m));
    }
}



ScenePtr<Scene> RewindScene::update(Platform& pfrm, App& app, Microseconds)
{
    // Playback history at a fixed delta.
    const Microseconds delta = 2.5f * (seconds(1) / 60);


    app.level_timer().count_down(delta);


    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    app.camera().update(pfrm, app.player_island(), cursor_loc, delta, true);


    const auto current_timestamp = app.level_timer().total();
    auto end_timestamp = app.time_stream().end_timestamp();

    if (not app.opponent_island() or not end_timestamp or
        app.level_timer().whole_seconds() == 0 or
        app.player().key_down(pfrm, Key::alt_1)) {

        app.game_speed() = GameSpeed::stopped;
        return scene_pool::alloc<ReadyScene>();
    }

    // NOTE: IMPORTANT: any code called in this loop should not push a
    // time_stream event! You could get stuck in an endless loop!
    while (end_timestamp and *end_timestamp > current_timestamp) {
        auto end = app.time_stream().end();
        switch ((time_stream::event::Type)end->type_) {

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
            app.time_stream().pop(sizeof *e);
            app.camera().shake(18);
            break;
        }


        case time_stream::event::Type::opponent_room_destroyed: {
            auto e = (time_stream::event::PlayerRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &*app.opponent_island(), {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            app.camera().shake(18);
            break;
        }


        case time_stream::event::Type::player_room_salvaged: {
            auto e = (time_stream::event::PlayerRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &app.player_island(), {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_salvaged: {
            auto e = (time_stream::event::OpponentRoomSalvaged*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &*app.opponent_island(), {e->x_, e->y_});
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_cannonball_destroyed: {
            auto e = (time_stream::event::PlayerCannonballDestroyed*)end;
            respawn_basic_projectile<Cannonball>(
                pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::opponent_cannonball_destroyed: {
            auto e = (time_stream::event::OpponentCannonballDestroyed*)end;
            respawn_basic_projectile<Cannonball>(
                pfrm, app, &*app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::player_arcbolt_destroyed: {
            auto e = (time_stream::event::PlayerArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::opponent_arcbolt_destroyed: {
            auto e = (time_stream::event::OpponentArcboltDestroyed*)end;
            respawn_basic_projectile<ArcBolt>(
                pfrm, app, &*app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::player_flak_destroyed: {
            auto e = (time_stream::event::PlayerFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::opponent_flak_destroyed: {
            auto e = (time_stream::event::OpponentFlakDestroyed*)end;
            respawn_basic_projectile<Flak>(
                pfrm, app, &*app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::player_ionburst_destroyed: {
            auto e = (time_stream::event::PlayerIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::opponent_ionburst_destroyed: {
            auto e = (time_stream::event::OpponentIonBurstDestroyed*)end;
            respawn_basic_projectile<IonBurst>(
                pfrm, app, &*app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(8);
            break;
        }


        case time_stream::event::Type::player_missile_destroyed: {
            auto e = (time_stream::event::PlayerMissileDestroyed*)end;
            respawn_missile(pfrm, app, &app.player_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(18);
            break;
        }


        case time_stream::event::Type::opponent_missile_destroyed: {
            auto e = (time_stream::event::OpponentMissileDestroyed*)end;
            respawn_missile(pfrm, app, &*app.opponent_island(), *e);
            app.time_stream().pop(sizeof *e);
            app.camera().shake(18);
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
            app.set_coins(pfrm, e->previous_value_.get(), false);
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_moved: {
            auto e = (time_stream::event::CharacterMoved*)end;
            Island* island =
                e->near_ ? &app.player_island() : &*app.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->reposition({e->previous_x_, e->previous_y_});
            } else {
                Platform::fatal("rewind chr moved: invalid chr id!");
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_died: {
            auto e = (time_stream::event::CharacterDied*)end;

            Island* island =
                e->near_ ? &app.player_island() : &*app.opponent_island();

            Player* owner =
                e->owned_by_player_ ? &app.player() : &app.opponent();

            const bool is_replicant = e->is_replicant_;

            auto chr = app.alloc_entity<BasicCharacter>(
                pfrm, island, owner, Vec2<u8>{e->x_, e->y_}, is_replicant);

            chr->__assign_id(e->id_.get());

            island->add_character(std::move(chr));

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::replicant_created: {
            auto e = (time_stream::event::ReplicantCreated*)end;

            Island* island =
                e->near_ ? &app.player_island() : &*app.opponent_island();

            if (auto room = island->get_room({e->x_, e->y_})) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();) {
                    const bool player_chr = (*it)->owner() == &app.player();
                    if (player_chr == e->owned_by_player_ and
                        (*it)->grid_position() == Vec2<u8>{e->x_, e->y_}) {
                        if ((*it)->is_replicant()) {
                            room->characters().erase(it);
                        } else {
                            Platform::fatal("rewind error: rewind replicant"
                                            "is not replicant?!");
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
                e->near_ ? &app.player_island() : &*app.opponent_island();

            if (auto chr = island->find_character_by_id(e->id_.get()).first) {
                chr->__set_health(e->previous_health_.get());
            } else {
                Platform::fatal("rewind chr health changed: invalid chr id!");
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_transported: {
            auto e = (time_stream::event::CharacterTransported*)end;

            Island* source_island = e->source_near_ ? &app.player_island()
                                                    : &*app.opponent_island();

            Island* dest_island = not e->source_near_ ? &app.player_island()
                                                      : &*app.opponent_island();

            // Ok, so here, we want to revert the transport. Find the
            // transported character at the destination island, and move it back
            // to the original location at the source island.

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                Platform::fatal("rewind chr_transported: Invalid character id!");
            }

            auto dest_room = chr_info.second;
            if (dest_room) {
                for (auto it = dest_room->characters().begin();
                     it not_eq dest_room->characters().end();) {

                    if ((*it).get() == chr_info.first) {
                        auto detached = std::move(*it);
                        dest_room->characters().erase(it);
                        detached->set_grid_position({e->previous_x_, e->previous_y_});
                        detached->set_parent(source_island);
                        detached->drop_movement_path();
                        if (auto source_room = source_island->get_room(
                                {e->previous_x_, e->previous_y_})) {
                            source_room->characters().push(std::move(detached));

                            // Give the transport back to the transporter, as
                            // we've reverted it.
                            source_room->___rewind___ability_used();

                        } else {
                            Platform::fatal("fatal error when rewinding "
                                            "transport: source room missing.");
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                Platform::fatal(
                    "error rewinding transport: dest room missing.");
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::character_disembark: {
            auto e = (time_stream::event::CharacterDisembark*)end;

            Island* source_island = e->chr_near_ ?
                &app.player_island() : &*app.opponent_island();

            Island* dest_island = not e->chr_near_ ?
                &app.player_island() : &*app.opponent_island();

            auto chr_info = dest_island->find_character_by_id(e->id_.get());
            if (chr_info.first == nullptr) {
                Platform::fatal("rewind chr_disembark: Invalid character id!");
            }

            auto dest_room = chr_info.second;
            if (dest_room) {
                for (auto it = dest_room->characters().begin();
                     it not_eq dest_room->characters().end(); ) {

                    if ((*it).get() == chr_info.first) {

                        // If the character disembarked, it must have ended up
                        // in a transporter. As we're rewinding things, give the
                        // transporter back its transport.
                        dest_room->___rewind___ability_used();

                        auto detached = std::move(*it);
                        dest_room->characters().erase(it);
                        const Vec2<u8> pos{e->previous_x_, e->previous_y_};
                        detached->set_grid_position(pos);
                        detached->set_parent(source_island);
                        detached->drop_movement_path();
                        if (auto source_room = source_island->get_room(pos)) {
                            source_room->characters().push(std::move(detached));
                        } else {
                            Platform::fatal("fata error when rewinding "
                                            "disembark: source room missing");
                        }
                        break;
                    } else {
                        ++it;
                    }
                }
            } else {
                Platform::fatal("error rewinding disembark: dest room missing");
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::weapon_set_target: {
            auto e = (time_stream::event::WeaponSetTarget*)end;

            Island* island =
                e->near_ ? &app.player_island() : &*app.opponent_island();

            if (auto room = island->get_room({e->room_x_, e->room_y_})) {
                if (e->has_previous_target_) {
                    room->set_target(pfrm, app, Vec2<u8>{
                        e->previous_target_x_,
                        e->previous_target_y_
                    }, false);
                } else {
                    room->unset_target(pfrm, app, false);
                }
            }

            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::player_room_reload_complete: {
            auto e = (time_stream::event::PlayerRoomReloadComplete*)end;
            if (auto room = app.player_island().get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload();
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        case time_stream::event::Type::opponent_room_reload_complete: {
            auto e = (time_stream::event::OpponentRoomReloadComplete*)end;
            if (auto room = app.opponent_island()->get_room({e->room_x_, e->room_y_})) {
                room->___rewind___finished_reload();
            }
            app.time_stream().pop(sizeof *e);
            break;
        }


        default:
            Platform::fatal("invalid event from time stream");
        }

        if (app.time_stream().end()) {
            end_timestamp = app.time_stream().end_timestamp();
        } else {
            end_timestamp.reset();
        }
    }


    app.update_parallax(-delta);

    auto rewind_projectiles = [&](Island& island) {
        auto& projectiles = island.projectiles();
        for (auto it = projectiles.begin(); it not_eq projectiles.end();) {
            if ((*it)->health() == 0) {
                it = projectiles.erase(it);
            } else {
                if (auto p = dynamic_cast<Projectile*>(&**it)) {
                    p->rewind(pfrm, app, delta);
                } else {
                    // Raise error: why is a non-projectile in the island's
                    // projectile list?
                }
                ++it;
            }
        }
    };

    rewind_entities(pfrm, app, delta, app.effects());

    rewind_projectiles(app.player_island());
    rewind_projectiles(*app.opponent_island());

    for (auto& room : app.player_island().rooms()) {
        room->rewind(pfrm, app, delta);
        for (auto& chr : room->characters()) {
            chr->rewind(pfrm, app, delta);
        }
    }

    for (auto& room : app.opponent_island()->rooms()) {
        room->rewind(pfrm, app, delta);
        for (auto& chr : room->characters()) {
            chr->rewind(pfrm, app, delta);
        }
    }


    return null_scene();
}



void RewindScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    // pfrm.screen().enable_night_mode(true);
    // pfrm.screen().fade(0.1f);
    // pfrm.screen().fade(0.f);
}



void RewindScene::exit(Platform& pfrm, App& app, Scene& next)
{
    // pfrm.screen().enable_night_mode(false);
    // pfrm.screen().fade(0.1f);
    // pfrm.screen().fade(0.f);
}



void RewindScene::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm);

    if (app.opponent_island()) {
        app.opponent_island()->display(pfrm);
    }
}



} // namespace skyland
