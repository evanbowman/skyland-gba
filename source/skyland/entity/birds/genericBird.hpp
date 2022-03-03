#pragma once

#include "platform/platform.hpp"
#include "bird.hpp"



namespace skyland {



class GenericBird : public Bird {
public:

    GenericBird(Platform::DynamicTexturePtr dt,
                const Vec2<u8>& position,
                bool near = false);


    void update(Platform&, App&, Microseconds delta) override;


    void signal(Platform&, App&) override;



    Island* island(App& app) override;


private:
    Platform::DynamicTexturePtr dt_;
    Vec2<u8> position_;
    bool near_;

    enum class State {
        roost,
        fly,
    } state_ = State::roost;

    Microseconds anim_timer_ = 0;

    u8 anim_index_ = 0;
    u8 color_ = 0;
    Float speed_ = 0;
};



}
