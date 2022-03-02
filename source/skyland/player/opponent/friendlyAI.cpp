#include "friendlyAI.hpp"
#include "enemyAI.hpp"
#include "skyland/scene/scriptHookScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void FriendlyAI::on_room_damaged(Platform& pfrm, App& app, Room&)
{
    // What!? The player attacked us! We're no longer a friendly AI.
    app.swap_opponent<EnemyAI>();

    invoke_hook(pfrm, app, "on-hostile-transition");

    // You attacked a neutral opponent! Not good!
    app.score().set(0);
}



} // namespace skyland
