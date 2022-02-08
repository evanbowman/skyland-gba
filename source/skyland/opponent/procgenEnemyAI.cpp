#include "procgenEnemyAI.hpp"
#include "skyland/skyland.hpp"
#include "skyland/rooms/core.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);



void ProcgenEnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.opponent_island()) {
        generate_level(pfrm, app);
    } else {
        EnemyAI::update(pfrm, app, delta);
    }
}



void ProcgenEnemyAI::generate_level(Platform& pfrm, App& app)
{
    for (auto& room : app.player_island().rooms()) {
        // TODO: gather information about the layout of the player's castle.
        (void)room;
    }

    pfrm.set_scroll(Layer::map_1_ext, -250, -374);

    app.create_opponent_island(pfrm, 4);

    app.opponent_island()->add_room<Core>(pfrm, app, {1, 13});

    prep_level(pfrm, app);
    show_island_exterior(pfrm, app, app.opponent_island());
}



}
