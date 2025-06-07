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


#include "bitvector.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "metaclassIndex.hpp"
#include "room.hpp"
#include "rooms/pluginRoom.hpp"
#include "script/value.hpp"
#include "systemString.hpp"



namespace skyland
{



// Why metaclasses? We need to be able to request info about a room before
// instantiating one, so mostly an organizational choice.
struct RoomMeta
{

    struct Info
    {
        virtual ~Info()
        {
        }

        virtual void
        construct(void* address, Island* parent, const RoomCoord& position) = 0;

        // DO NOT CALL THIS FUNCTION UNLESS YOU REALLY KNOW WHAT YOU'RE DOING. I
        // added this interface as a hack to make multiboot stuff easier to
        // write.

        virtual void
        create(Island*, const RoomCoord&, bool do_repaint = true) const = 0;
        // virtual RoomPtr<Room> create(Island*, const RoomCoord&) const = 0;
        virtual const char* name() const = 0;
        virtual SystemStringBuffer ui_name() const = 0;
        virtual Vec2<u8> size() const = 0;
        virtual Coins cost() const = 0;
        virtual ATP atp_value() const = 0;
        virtual Power consumes_power() const = 0;
        virtual RoomProperties::Bitmask properties() const = 0;
        virtual Room::Icon icon() const = 0;
        virtual Room::Icon unsel_icon() const = 0;
        virtual Health full_health() const = 0;
        virtual Room::Category category() const = 0;
        virtual void format_description(StringBuffer<512>& buffer) const = 0;
        virtual Room::WeaponOrientation weapon_orientation() const = 0;

        virtual void configure(Health health, Coins cost, Power power)
        {
        }
    };



    static constexpr int align = 8;
    static constexpr int max_size = 10 * sizeof(void*);

    alignas(align) u8 buffer_[max_size];


    template <typename T> void init();


    void init_plugin();


    RoomMeta()
    {
    }

    RoomMeta(const RoomMeta&) = delete;

    Info* operator->()
    {
        return reinterpret_cast<Info*>(buffer_);
    }

    const Info* operator->() const
    {
        return reinterpret_cast<const Info*>(buffer_);
    }

    Info* box()
    {
        return reinterpret_cast<Info*>(buffer_);
    }

    ~RoomMeta()
    {
        reinterpret_cast<Info*>(buffer_)->~Info();
    }
};



std::pair<RoomMeta*, int> room_metatable();



MetaclassIndex plugin_rooms_begin();



bool is_enabled(MetaclassIndex index);



void set_enabled(MetaclassIndex index, bool enabled);



void plugin_rooms_unregister();



bool plugin_room_register(lisp::Value* config);



MetaclassIndex metaclass_index(const char* name);



RoomMeta* load_metaclass(const char* name);
RoomMeta& require_metaclass(const char* name);
RoomMeta* load_metaclass(MetaclassIndex index);


// Some cached metaclass pointers, for convenience.
extern const RoomMeta* forcefield_mt;
extern const RoomMeta* forcefield2_mt;
extern const RoomMeta* cannon_mt;
extern const RoomMeta* missile_silo_mt;
extern const RoomMeta* ion_cannon_mt;
extern const RoomMeta* bulkhead_mt;
extern const RoomMeta* drone_bay_mt;
extern const RoomMeta* decimator_mt;
extern const RoomMeta* flak_gun_mt;
extern const RoomMeta* radiator_mt;
extern const RoomMeta* infirmary_mt;
extern const RoomMeta* transporter_mt;
extern const RoomMeta* chaos_core_mt;


inline bool is_forcefield(RoomMeta* m)
{
    return m == forcefield_mt or m == forcefield2_mt;
}



void room_set_hidden(MetaclassIndex idx, bool hidden);
bool room_hidden(MetaclassIndex);



void load_hidden_rooms();
void store_hidden_rooms();



} // namespace skyland
