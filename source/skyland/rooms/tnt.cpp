#include "tnt.hpp"
#include "skyland/skyland.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



TNT::TNT(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void TNT::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (health() not_eq max_health()) {
        Room::ready();

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(pfrm, app, 5);
            damage_timer_ = 0;
        }
    }
}



ScenePtr<Scene> TNT::select(Platform& pfrm, App& app)
{
    Room::apply_damage(pfrm, app, 1);

    return null_scene();
}



void TNT::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::tnt;
}



void TNT::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::tnt;
}



static SharedVariable tnt_damage("tnt_damage", 30);
static SharedVariable tnt_range("tnt_range", 1);



void TNT::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    auto flak_smoke = [](Platform& pfrm, App& app, const Vec2<Float>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            pfrm, rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(pfrm, app, center());
    flak_smoke(pfrm, app, center());

    app.on_timeout(pfrm,
                   milliseconds(190),
                   [pos = center(), flak_smoke](
                       Platform& pf, App& app) { flak_smoke(pf, app, pos); });

    auto targets = allocate_dynamic<Buffer<Room*, 300>>(pfrm);

    for (int x = -tnt_range; x < tnt_range + 1; ++x) {
        for (int y = -tnt_range; y < tnt_range + 1; ++y) {
            if (position().x + x < 0 or position().y + y < 0 or
                position().x + x > 15 or position().y + y > 15) {
                continue;
            }
            if (auto room = parent()->get_room({
                        u8(position().x + x),
                        u8(position().y + y)})) {
                bool found = false;
                for (auto& target : *targets) {
                    if (target == room) {
                        found = true;
                        break;
                    }
                }
                if (not found) {
                    targets->push_back(room);
                }
            }
        }
    }

    for (auto& room : *targets) {
        room->apply_damage(pfrm, app, tnt_damage);
    }
}



}
