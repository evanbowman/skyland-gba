#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



extern SharedVariable transporter_reload_ms;



class Transporter : public Room
{
public:
    Transporter(Island* parent, const Vec2<u8>& position);


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void update(Platform&, App&, Microseconds delta) override;
    void rewind(Platform&, App&, Microseconds delta) override;

    void ___rewind___finished_reload(Platform&, App&) override;

    void ___rewind___ability_used(Platform&, App&) override;



    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor) override;


    void transport_occupant(Platform& pfrm,
                            App& app,
                            // NOTE: if you do not pass a destination, the
                            // transporter logic will select a random one.
                            std::optional<Vec2<u8>> destination = {});


    void recover_character(Platform&, App& app, const Vec2<u8>& pos);


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "transporter";
    }


    static SystemString ui_name()
    {
        return SystemString::block_transporter;
    }


    static Float ai_base_weight()
    {
        return 900.f;
    }


    static RoomProperties::Value properties()
    {
        return RoomProperties::workshop_required;
    }


    static Icon icon()
    {
        return 904;
    }


    static Icon unsel_icon()
    {
        return 888;
    }


    bool ready() const;


    Microseconds reload_time_remaining() const override
    {
        return recharge_;
    }


private:
    Microseconds recharge_ = 1000 * transporter_reload_ms;
};



void transport_character_impl(App& app,
                              bool ai_controlled,
                              Island* src_island,
                              Island* dst_island,
                              const Vec2<u8>& src,
                              const Vec2<u8>& dst,
                              int signal = 0);



} // namespace skyland
