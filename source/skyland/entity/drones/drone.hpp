#pragma once

#include "memory/rc.hpp"
#include "skyland/entity.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class Island;



class Drone : public Entity, public IntrusiveRcControlBlock<Drone> {
public:
    Drone(const char* name,
          Island* parent,
          Island* destination,
          const Vec2<u8>& grid_pos);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;



    virtual void ___rewind___ability_used(Platform& pfrm, App& app)
    {
    }


    virtual void ___rewind___finished_reload(Platform& pfrm, App& app)
    {
        ___rewind___ability_used(pfrm, app);
    }


    enum State : u8 {
        launch,
        ready,
    };


    u8 state() const
    {
        return state_;
    }


    // Intended for the rewind logic, nothing more. Generally, do not call.
    void
    __override_state(State state, Microseconds duration, Microseconds timer)
    {
        state_ = state;
        duration_ = duration;
        timer_ = timer;
    }


    void __set_health(Health health)
    {
        health_ = health;
    }


    const Vec2<u8>& position() const
    {
        return grid_pos_;
    }


    void set_movement_target(const Vec2<u8>& position);



    void set_target(Platform& pfrm,
                    App& app,
                    const Vec2<u8>& target,
                    bool target_near = false);


    void drop_target(Platform& pfrm, App& app);


    Island* parent() const
    {
        return parent_;
    }


    Island* destination() const
    {
        return destination_;
    }


    virtual const char* name() const = 0;


    virtual ScenePtr<Scene> select(Platform& pfrm, App&) = 0;


    virtual Microseconds reload_time_remaining() const = 0;


    u8 metaclass_index() const
    {
        return metaclass_index_;
    }


    void apply_damage(Platform& pfrm, App& app, Health amount) override;


    Microseconds timer() const
    {
        return timer_;
    }


    Microseconds duration() const
    {
        return duration_;
    }


protected:
    u8 state_ = State::launch;

    Microseconds timer_ = 0;
    Microseconds duration_ = 0;

    void update_sprite(Platform& pfrm, App& app);

private:
    Island* parent_;
    Island* destination_;
    Vec2<u8> grid_pos_;
    Vec2<s16> anchor_;

protected:
    std::optional<Vec2<u8>> target_;
    u8 target_near_ : 1;

private:
    u8 metaclass_index_ : 7;
};



} // namespace skyland
