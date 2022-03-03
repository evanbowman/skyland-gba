#pragma once

#include "skyland/entity.hpp"
#include "platform/platform.hpp"



namespace skyland {



class GenericBird : public Entity {
public:

    GenericBird(Platform::DynamicTexturePtr dt,
                const Vec2<u8>& position,
                bool near = false);


    void update(Platform&, App&, Microseconds delta) override;


private:
    Platform::DynamicTexturePtr dt_;
    Vec2<u8> position_;
    bool near_;

    enum class State {
        roost,
        fly,
    } state_ = State::roost;
};



}
