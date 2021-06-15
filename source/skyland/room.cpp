#include "room.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"
#include "skyland.hpp"
#include "globals.hpp"



namespace skyland {


Room::Room(Island* parent,
           const char* name,
           const Vec2<u8>& size,
           const Vec2<u8>& position,
           Health health)
    : parent_(parent),
      characters_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      size_(size),
      position_(position),
      health_(health)
{
    auto metatable = room_metatable();

    for (int i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_cmp(name, current->name()) == 0) {
            metaclass_ = &current;
        }
    }

#ifdef __GBA__
    static_assert(sizeof(Room) < 33,
                  "Not actually a hard requirement, just put"
                  "this here as a reminder to keep rooms small."
                  " Room pool entries are only 64 bytes in size."
                  " Increasing the base room size will leave fewer "
                  " bytes for derived rooms. If needed, you could "
                  " increase the room pool size in roomPool.hpp");
#endif
}



void Room::set_injured(Platform& pfrm)
{
    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            pfrm.set_palette(
                parent_->layer(), position().x + x, position().y + y, 13);
        }
    }

    injured_timer_ = milliseconds(250);
}



void Room::update(Platform& pfrm, App& app, Microseconds delta)
{
    for (auto& character : characters_) {
        character->update(pfrm, app, delta);
    }

    if (injured_timer_) {
        if (injured_timer_ > 0) {
            const auto new_timer = injured_timer_ - delta;

            if (new_timer < milliseconds(210) and
                injured_timer_ > milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         14);
                    }
                }
            }

            if (new_timer < milliseconds(170) and
                injured_timer_ > milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(parent_->layer(),
                                         position().x + x,
                                         position().y + y,
                                         15);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ <= 0) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        pfrm.set_palette(
                            parent_->layer(),
                            position().x + x,
                            position().y + y,
                            parent_->layer() == Layer::map_0_ext ? 0 : 2);
                    }
                }
            }
        }
    }
}



Island* Room::other_island(App& app)
{
    if (&app.player_island() == parent_) {
        if (app.encountered_island()) {
            return &*app.encountered_island();
        }
        return nullptr;
    } else {
        return &app.player_island();
    }
}



Vec2<Float> Room::origin() const
{
    auto origin = parent_->origin();
    origin.x += position_.x * 16;
    origin.y += position_.y * 16;
    return origin;
}



Vec2<Float> Room::center() const
{
    auto o = origin();
    o.x += size_.x * 16;
    o.y += size_.y * 16;

    if (size_.x % 2 == 1) {
        o.x -= 8;
    }

    if (size_.y % 2 == 1) {
        o.y -= 8;
    }

    return o;
}



} // namespace skyland
