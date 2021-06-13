#pragma once


#include "room.hpp"
#include "island.hpp"




namespace skyland {



struct RoomMeta {
    struct Spawner {
        virtual ~Spawner() {}
        virtual void create(Platform&, Island*, const Vec2<u8>&) = 0;
    };

    template <typename T>
    struct SpawnerImpl : public Spawner {
        void create(Platform& pfrm,
                    Island* parent,
                    const Vec2<u8>& position) override
        {
            parent->add_room<T>(pfrm, position);
        }
    };

    static constexpr int align = 8;

    template <typename T>
    void init()
    {
        static_assert(sizeof buffer_ >= sizeof(SpawnerImpl<T>));
        static_assert(align >= alignof(SpawnerImpl<T>));


        new (buffer_) SpawnerImpl<T>();
    }

    RoomMeta()
    {
    }

    RoomMeta(const RoomMeta&) = delete;

    Spawner& spawner()
    {
        return *reinterpret_cast<Spawner*>(buffer_);
    }

    ~RoomMeta()
    {
        reinterpret_cast<Spawner*>(buffer_)->~Spawner();
    }

    const Vec2<u8> size_;
    const char* name_;
    alignas(align) u8 buffer_[8];
};



template <typename ...Rooms>
struct RoomMetatable {
public:
    template <size_t i, typename First, typename ...Rest>
    void init()
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



}
