#pragma once


#include "coins.hpp"
#include "island.hpp"
#include "room.hpp"



namespace skyland {



struct RoomMeta {
    struct Box {
        virtual ~Box()
        {
        }

        virtual void create(Platform&, Island*, const Vec2<u8>&) const = 0;
        virtual const char* name() const = 0;
        virtual Vec2<u8> size() const = 0;
        virtual Coins cost() const = 0;
        virtual Float ai_base_weight() const = 0;
        virtual Power consumes_power() const = 0;
        virtual Conditions::Value conditions() const = 0;
        virtual Room::Icon icon() const = 0;
        virtual Room::Icon unsel_icon() const = 0;
    };

    template <typename T> struct BoxImpl : public Box {
        void create(Platform& pfrm,
                    Island* parent,
                    const Vec2<u8>& position) const override
        {
            parent->add_room<T>(pfrm, position);
        }

        const char* name() const override
        {
            return T::name();
        }

        Room::Icon icon() const override
        {
            return T::icon();
        }

        Room::Icon unsel_icon() const override
        {
            return T::unsel_icon();
        }

        virtual Vec2<u8> size() const override
        {
            return T::size();
        }

        virtual Coins cost() const override
        {
            return T::cost();
        }

        virtual Float ai_base_weight() const override
        {
            return T::ai_base_weight();
        }

        virtual Power consumes_power() const override
        {
            return T::consumes_power();
        }

        virtual Conditions::Value conditions() const override
        {
            return T::conditions();
        }
    };

    static constexpr int align = 8;

    alignas(align) u8 buffer_[8];


    template <typename T> void init()
    {
        static_assert(sizeof buffer_ >= sizeof(BoxImpl<T>));
        static_assert(align >= alignof(BoxImpl<T>));

        new (buffer_) BoxImpl<T>();
    }

    RoomMeta()
    {
    }

    RoomMeta(const RoomMeta&) = delete;

    Box* operator->()
    {
        return reinterpret_cast<Box*>(buffer_);
    }

    const Box* operator->() const
    {
        return reinterpret_cast<const Box*>(buffer_);
    }

    ~RoomMeta()
    {
        reinterpret_cast<Box*>(buffer_)->~Box();
    }
};


template <typename... Rooms> struct RoomMetatable {
public:
    template <size_t i, typename First, typename... Rest> void init()
    {
        table_[i].template init<First>();

        if constexpr (sizeof...(Rest) > 0) {
            init<i + 1, Rest...>();
        }
    }

    RoomMetatable()
    {
        init<0, Rooms...>();
    }

    constexpr int size()
    {
        return sizeof...(Rooms);
    }

    RoomMeta table_[sizeof...(Rooms)];
};


std::pair<RoomMeta*, int> room_metatable();



using MetaclassIndex = u16;



MetaclassIndex metaclass_index(const char* name);



RoomMeta* load_metaclass(const char* name);
RoomMeta* load_metaclass(MetaclassIndex index);


// Some cached metaclass pointers, for convenience.
extern const RoomMeta* forcefield_mt;
extern const RoomMeta* cannon_mt;
extern const RoomMeta* missile_silo_mt;
extern const RoomMeta* ion_cannon_mt;
extern const RoomMeta* bulkhead_mt;


} // namespace skyland
