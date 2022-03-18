#include "radiator.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Radiator::Radiator(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Radiator::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_radiator)->c_str();
}



void Radiator::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    damage_timer_ += delta;

    if (damage_timer_ > seconds(1)) {
        damage_timer_ -= seconds(1);

        emit_radiation(pfrm, app);
    }
}



static SharedVariable radiation_damage("radiation_damage", 20);



void Radiator::emit_radiation(Platform& pfrm, App& app)
{
    Buffer<BasicCharacter*, 10> queue;

    auto pos = position();
    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        for (int y = pos.y - 2; y < pos.y + 3; ++y) {
            if (x < 0 or y < 0) {
                continue;
            }
            if (auto room = parent()->get_room({u8(x), u8(y)})) {
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() not_eq Vec2<u8>{(u8)x, (u8)y}) {
                        continue;
                    }
                    const bool found = [&] {
                        for (auto pushed : queue) {
                            if (pushed == chr.get()) {
                                return true;
                            }
                        }
                        return false;
                    }();
                    if (not found) {
                        queue.push_back(chr.get());
                    }
                }
            }
        }
    }

    for (auto& chr : queue) {
        chr->apply_damage(pfrm, app, radiation_damage);
    }
}



void Radiator::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::radiator;
}



void Radiator::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radiator;
}



} // namespace skyland
