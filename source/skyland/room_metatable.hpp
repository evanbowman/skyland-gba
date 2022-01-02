#pragma once


#include "coins.hpp"
#include "island.hpp"
#include "room.hpp"
#include "rooms/pluginRoom.hpp"
#include "script/lisp.hpp"



namespace skyland {



// Why metaclasses? We need to be able to request info about a room before
// instantiating one, so mostly an organizational choice.
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
        virtual Health full_health() const = 0;

        virtual void configure(Health health, Coins cost, Power power)
        {
        }
    };

    // A metatable entry backed by a lisp datastructure, allowing users to
    // define their own rooms via scripts.
    struct PluginBox : public Box {
        RoomMeta* mt_;
        mutable std::optional<lisp::Protected> info_;


        PluginBox(RoomMeta* mt) : mt_(mt)
        {
        }


        void create(Platform& pfrm,
                    Island* parent,
                    const Vec2<u8>& position) const override
        {
            parent->add_room<PluginRoom>(pfrm, position, mt_);
        }


        struct PluginInfo {
            enum Tag {
                size,
                update,
                name,
                ai_weight,
                coins,
                power,
                full_health,
            };
        };


        template <PluginInfo::Tag info, typename T> T& fetch_info() const
        {
            if (info_) {
                return lisp::get_list(*info_, info)->expect<T>();
            }

            Platform::fatal("plugin room info unassigned");
        }


        virtual const char* name() const
        {
            return fetch_info<PluginInfo::name, lisp::Symbol>().name_;
        }


        virtual Vec2<u8> size() const
        {
            auto& pair = fetch_info<PluginInfo::size, lisp::Cons>();
            return {(u8)pair.car()->expect<lisp::Integer>().value_,
                    (u8)pair.cdr()->expect<lisp::Integer>().value_};
        }

        virtual Coins cost() const
        {
            return fetch_info<PluginInfo::coins, lisp::Integer>().value_;
        }

        virtual Float ai_base_weight() const
        {
            return fetch_info<PluginInfo::ai_weight, lisp::Integer>().value_;
        }

        virtual Power consumes_power() const
        {
            return fetch_info<PluginInfo::power, lisp::Integer>().value_;
        }

        virtual Conditions::Value conditions() const
        {
            return Conditions::plugin;
        }

        virtual Room::Icon icon() const
        {
            // TODO...
            return 0;
        }

        virtual Room::Icon unsel_icon() const
        {
            // TODO...
            return 0;
        }

        virtual Health full_health() const
        {
            return fetch_info<PluginInfo::full_health, lisp::Integer>().value_;
        }
    };

    template <typename T> struct BoxImpl : public Box {
        BoxImpl()
            // NOTE: the game will fill in these parameters from configuration
            // later on.
            : health_(10), cost_(10), power_(10)
        {
        }

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

        Vec2<u8> size() const override
        {
            return T::size();
        }

        Coins cost() const override
        {
            return cost_;
        }

        Float ai_base_weight() const override
        {
            return T::ai_base_weight();
        }

        Power consumes_power() const override
        {
            return power_;
        }

        Conditions::Value conditions() const override
        {
            return T::conditions();
        }

        Health full_health() const override
        {
            return health_;
        }

        void configure(Health health, Coins cost, Power power) override
        {
            health_ = health;
            cost_ = cost;
            power_ = power;
        }

        s16 health_;
        s16 cost_;
        s16 power_;
    };

    static constexpr int align = 8;

    alignas(align) u8 buffer_[sizeof(PluginBox)];


    template <typename T> void init()
    {
        static_assert(sizeof buffer_ >= sizeof(BoxImpl<T>));
        static_assert(align >= alignof(BoxImpl<T>));

        new (buffer_) BoxImpl<T>();
    }


    void init_plugin()
    {
        new (buffer_) PluginBox(this);
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


// NOTE: I wanted users to be able to script their own room objects. The plugin
// slots represent room metaclasses defined as lisp scripts.
template <int plugin_slots, typename... Rooms> struct RoomMetatable {
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

        for (int i = 0; i < plugin_slots; ++i) {
            table_[sizeof...(Rooms) + i].init_plugin();
        }
    }

    constexpr int size()
    {
        return sizeof...(Rooms);
    }

    RoomMeta table_[sizeof...(Rooms) + plugin_slots];
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
extern const RoomMeta* drone_bay_mt;


} // namespace skyland
