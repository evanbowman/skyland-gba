////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
