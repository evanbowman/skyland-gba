#pragma once


#include "island.hpp"
#include "room.hpp"
#include "coins.hpp"



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

        virtual Vec2<u8> size() const override
        {
            return T::size();
        }

        virtual Coins cost() const override
        {
            return T::cost();
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


} // namespace skyland
