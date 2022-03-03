#pragma once

#include "bird.hpp"
#include "platform/platform.hpp"



namespace skyland {



class GenericBird : public Bird
{
public:
    GenericBird(Platform::DynamicTexturePtr dt,
                const Vec2<u8>& position,
                bool near = false);


    void update(Platform&, App&, Microseconds delta) override;


    void signal(Platform&, App&) override;



    Island* island(App& app) override;


    static void spawn(Platform& pfrm, App& app, Island& island, int count);


    static void generate(Platform& pfrm, App& app);


    Vec2<u8> coordinate() override
    {
        return position_;
    }


private:
    Platform::DynamicTexturePtr dt_;
    Vec2<u8> position_;
    bool near_;
    bool alerted_ = false;

    enum class State {
        roost,
        fly,
    } state_ = State::roost;

    Microseconds anim_timer_ = 0;

    u8 anim_index_ = 0;
    u8 color_ = 0;
    Float speed_ = 0;
};



} // namespace skyland
