////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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


    void update(Time delta) override;
    void display(Platform::Screen& screen) override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void rewind(Time delta) override;



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


    static ATP atp_value()
    {
        return 1.0_atp;
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


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void retract()
    {
        state_ = State::retract;
    }


    void set_item(int item)
    {
        item_ = item;
    }


    void apply_damage(Health damage, const DamageConfiguration& conf) override
    {
        if (state_ == State::idle) {
            Room::apply_damage(damage, conf);
        } else {
            // Does not take damage while dropping/retracting.
        }
    }


private:
    Time timer_;

    enum class State {
        idle,
        drop,
        retract,
    } state_ = State::idle;

    int item_ = 0;
};



} // namespace skyland
