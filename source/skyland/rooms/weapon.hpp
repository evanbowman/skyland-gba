#pragma once

#include "skyland/room.hpp"


namespace skyland {



class Weapon : public Room {
public:

    Weapon(Island* parent,
           const char* name,
           const Vec2<u8>& size,
           const Vec2<u8>& position,
           Microseconds reload_time);


    virtual Microseconds reload() const = 0;


    virtual void fire(Platform& pfrm, App& app) = 0;


    Microseconds reload_time_remaining() const override
    {
        return reload_timer_;
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;


    void set_target(Platform& pfrm,
                    App& app,
                    const Vec2<u8>& target,
                    bool sequenced) override;


    void unset_target(Platform& pfrm,
                      App& app,
                      bool sequenced) override;


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


protected:
    std::optional<Vec2<u8>> target_;
    Microseconds reload_timer_;
};



}
