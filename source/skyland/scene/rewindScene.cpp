#include "rewindScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/entity/projectile/projectile.hpp"
#include "readyScene.hpp"
#include "localization.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



ScenePtr<Scene> RewindScene::update(Platform& pfrm, App& app, Microseconds)
{
    // Playback history at a fixed delta of 60 fps.
    const auto delta = 2 * (seconds(1) / 60);


    app.level_timer().count_down(delta);


    const auto current_timestamp = app.level_timer().total();
    auto end_timestamp = app.time_stream().end_timestamp();

    if (not app.opponent_island() or
        not end_timestamp or
        app.level_timer().whole_seconds() == 0 or
        app.player().key_down(pfrm, Key::alt_1)) {

        app.game_speed() = GameSpeed::stopped;
        return scene_pool::alloc<ReadyScene>();
    }

    while (end_timestamp and *end_timestamp > current_timestamp) {
        auto end = app.time_stream().end();
        switch (end->type_) {
        case time_stream::event::Type::player_room_created: {
            auto e = (time_stream::event::PlayerRoomCreated*)end;
            app.player_island().destroy_room(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof(*e));
            break;
        }

        case time_stream::event::Type::opponent_room_created: {
            auto e = (time_stream::event::OpponentRoomCreated*)end;
            app.opponent_island()->destroy_room(pfrm, app, {e->x_, e->y_});
            app.time_stream().pop(sizeof(*e));
            break;
        }

        case time_stream::event::Type::player_room_destroyed: {
            auto e = (time_stream::event::PlayerRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &app.player_island(), {e->x_, e->y_});
            app.time_stream().pop(sizeof(*e));
            break;
        }

        case time_stream::event::Type::opponent_room_destroyed: {
            auto e = (time_stream::event::PlayerRoomDestroyed*)end;
            (*load_metaclass(e->type_))
                ->create(pfrm, app, &*app.opponent_island(), {e->x_, e->y_});
            app.time_stream().pop(sizeof(*e));
            break;
        }
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

    rewind_projectiles(app.player_island());
    rewind_projectiles(*app.opponent_island());


    return null_scene();
}




void RewindScene::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm);
}



}
