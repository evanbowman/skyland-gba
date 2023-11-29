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


#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Crane final : public Room
{
public:
    struct Discoveries
    {
        enum Item {
            bomb,
            coins,

            old_boot,
            data_disk1,
            data_disk2,
            data_disk3,

            count,
        };

        host_u64 items_;


        Buffer<Item, count> items() const
        {
            Buffer<Item, count> result;

            for (int i = coins + 1; i < count; ++i) {
                if ((items_.get() & (1 << i))) {
                    result.push_back((Item)i);
                }
            }

            return result;
        }


        Buffer<Item, count> unallocated_items() const
        {
            Buffer<Item, count> result;

            for (int i = coins + 1; i < count; ++i) {
                if ((items_.get() & (1 << i)) == 0) {
                    result.push_back((Item)i);
                }
            }

            return result;
        }
    };

    static Discoveries load_discoveries();
    static void store_discoveries(const Discoveries& d);


    Crane(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Microseconds delta) override;
    void display(Platform::Screen& screen) override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void rewind(Microseconds delta) override;



    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::skyland_forever_unsupported |
               RoomProperties::adventure_mode_only |
               RoomProperties::not_constructible |
               RoomProperties::multiplayer_unsupported;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float atp_value()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "crane";
    }


    static SystemString ui_name()
    {
        return SystemString::block_crane;
    }


    static Icon icon()
    {
        return 3304;
    }


    static Icon unsel_icon()
    {
        return 3320;
    }


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


    void retract()
    {
        state_ = State::retract;
    }


    void set_item(int item)
    {
        item_ = item;
    }


    void apply_damage(Health damage) override
    {
        if (state_ == State::idle) {
            Room::apply_damage(damage);
        } else {
            // Does not take damage while dropping/retracting.
        }
    }


private:
    Microseconds timer_;

    enum class State {
        idle,
        drop,
        retract,
    } state_ = State::idle;

    int item_ = 0;
};



} // namespace skyland
