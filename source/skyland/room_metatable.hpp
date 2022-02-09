#pragma once


#include "bitvector.hpp"
#include "coins.hpp"
#include "island.hpp"
#include "metaclassIndex.hpp"
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

        virtual void
        create(Platform&, App&, Island*, const Vec2<u8>&) const = 0;
        virtual RoomPtr<Room>
        create(Platform&, Island*, const Vec2<u8>&) const = 0;
        virtual const char* name() const = 0;
        virtual Vec2<u8> size() const = 0;
        virtual Coins cost() const = 0;
        virtual Float ai_base_weight() const = 0;
        virtual Power consumes_power() const = 0;
        virtual u32 properties() const = 0;
        virtual Room::Icon icon() const = 0;
        virtual Room::Icon unsel_icon() const = 0;
        virtual Health full_health() const = 0;
        virtual Room::Category category() const = 0;
        virtual void format_description(StringBuffer<512>& buffer) const = 0;


        virtual void configure(Health health, Coins cost, Power power)
        {
        }
    };

    // A metatable entry backed by a lisp datastructure, allowing users to
    // define their own rooms via scripts.
    struct PluginBox : public Box {
        RoomMeta* mt_;
        mutable std::optional<lisp::Protected> info_;

        s16 health_ = 10;
        s16 cost_ = 10;
        s16 power_ = 10;


        PluginBox(RoomMeta* mt) : mt_(mt)
        {
        }


        void create(Platform& pfrm,
                    App& app,
                    Island* parent,
                    const Vec2<u8>& position) const override
        {
            parent->add_room<PluginRoom>(pfrm, app, position, mt_);
        }


        RoomPtr<Room> create(Platform& pfrm,
                             Island* parent,
                             const Vec2<u8>& position) const override
        {
            return room_pool::alloc<PluginRoom>(parent, position, mt_);
        }


        struct PluginInfo {
            enum Tag {
                name,
                size,
                graphics_list,
                update_frequency,
                update,
            };
        };


        template <PluginInfo::Tag info, typename T> T& fetch_info() const
        {
            if (info_) {
                return lisp::get_list(*info_, info)->expect<T>();
            }

            Platform::fatal("plugin room info unassigned");
        }


        const char* name() const override
        {
            return fetch_info<PluginInfo::name, lisp::Symbol>().name_;
        }


        Vec2<u8> size() const override
        {
            auto& pair = fetch_info<PluginInfo::size, lisp::Cons>();
            return {(u8)pair.car()->expect<lisp::Integer>().value_,
                    (u8)pair.cdr()->expect<lisp::Integer>().value_};
        }

        Coins cost() const override
        {
            return cost_;
        }

        Float ai_base_weight() const override
        {
            // FIXME!
            return 2;
        }

        Power consumes_power() const override
        {
            return power_;
        }

        u32 properties() const override
        {
            return RoomProperties::plugin | RoomProperties::disallow_chimney |
                   RoomProperties::roof_hidden |
                   RoomProperties::locked_by_default;
        }

        Room::Category category() const override
        {
            return Room::Category::misc; // TODO...
        }

        void format_description(StringBuffer<512>&) const override
        {
            Platform::fatal("attempt to fetch desciption for a plugin room.");
        }

        Room::Icon icon() const override
        {
            return PluginRoom::icon();
        }

        Room::Icon unsel_icon() const override
        {
            return PluginRoom::unsel_icon();
        }

        Health full_health() const override
        {
            return health_;
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
                    App& app,
                    Island* parent,
                    const Vec2<u8>& position) const override
        {
            parent->add_room<T>(pfrm, app, position);
        }

        RoomPtr<Room> create(Platform& pfrm,
                             Island* parent,
                             const Vec2<u8>& position) const override
        {
            return room_pool::alloc<T>(parent, position);
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

        u32 properties() const override
        {
            return T::properties();
        }

        Room::Category category() const override
        {
            return T::category(); // TODO...
        }

        void format_description(StringBuffer<512>& buffer) const override
        {
            return T::format_description(buffer);
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

    Box* box()
    {
        return reinterpret_cast<Box*>(buffer_);
    }

    ~RoomMeta()
    {
        reinterpret_cast<Box*>(buffer_)->~Box();
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
extern const RoomMeta* cannon_mt;
extern const RoomMeta* missile_silo_mt;
extern const RoomMeta* ion_cannon_mt;
extern const RoomMeta* bulkhead_mt;
extern const RoomMeta* drone_bay_mt;


} // namespace skyland
