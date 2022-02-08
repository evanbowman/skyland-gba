#include "procgenEnemyAI.hpp"
#include "skyland/skyland.hpp"
#include "skyland/rooms/core.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);
void set_island_positions(Island& left_island, Island& right_island);



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

    app.create_opponent_island(pfrm, 4);
    set_island_positions(app.player_island(), *app.opponent_island());

    app.opponent_island()->add_room<Core>(pfrm, app, {1, 13});

    prep_level(pfrm, app);
    show_island_exterior(pfrm, app, app.opponent_island());
}



}
