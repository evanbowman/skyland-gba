#pragma once


#include "bitvector.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "metaclassIndex.hpp"
#include "room.hpp"
#include "rooms/pluginRoom.hpp"
#include "script/lisp.hpp"
#include "systemString.hpp"



namespace skyland {



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
        create(Platform&, App&, Island*, const Vec2<u8>&) const = 0;
        virtual RoomPtr<Room>
        create(Platform&, Island*, const Vec2<u8>&) const = 0;
        virtual const char* name() const = 0;
        virtual SystemStringBuffer ui_name(Platform& pfrm) const = 0;
        virtual Vec2<u8> size() const = 0;
        virtual Coins cost() const = 0;
        virtual Float ai_base_weight() const = 0;
        virtual Power consumes_power() const = 0;
        virtual u32 properties() const = 0;
        virtual Room::Icon icon() const = 0;
        virtual Room::Icon unsel_icon() const = 0;
        virtual Health full_health() const = 0;
        virtual Room::Category category() const = 0;
        virtual void format_description(Platform& pfrm,
                                        StringBuffer<512>& buffer) const = 0;


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



bool is_hidden(MetaclassIndex index);



void set_hidden(MetaclassIndex index, bool hidden);



void plugin_rooms_unregister();



bool plugin_room_register(lisp::Value* config);



MetaclassIndex metaclass_index(const char* name);



RoomMeta* load_metaclass(const char* name);
RoomMeta& require_metaclass(const char* name);
RoomMeta* load_metaclass(MetaclassIndex index);


// Some cached metaclass pointers, for convenience.
extern const RoomMeta* forcefield_mt;
extern const RoomMeta* cannon_mt;
extern const RoomMeta* missile_silo_mt;
extern const RoomMeta* ion_cannon_mt;
extern const RoomMeta* bulkhead_mt;
extern const RoomMeta* drone_bay_mt;


} // namespace skyland
