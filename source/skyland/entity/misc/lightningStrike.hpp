#pragma once

#include "platform/platform.hpp"
#include "skyland/entity.hpp"



namespace skyland
{



class LightningStrike : public Entity
{
public:
    LightningStrike() : Entity({})
    {
        sprite_.set_alpha(Sprite::Alpha::transparent);
        PLATFORM.screen().schedule_fade(
            0.6f, {ColorConstant::rich_black, true, false, false, true, true});

        PLATFORM.screen().schedule_fade(
            0.6f,
            {ColorConstant::silver_white, false, false, true, false, true});
    }

    enum class LightningState : u8 {
        none,
        begin1,
        begin2,
        hold,
        fade,
    } ls_ = LightningState::begin1;

    Time lightning_timer_ = milliseconds(65);


    void update(Time delta) override
    {
        switch (ls_) {
        case LightningState::none:
            break;

        case LightningState::begin1:
            lightning_timer_ -= delta;
            if (lightning_timer_ <= 0) {
                lightning_timer_ = milliseconds(48);
                ls_ = LightningState::begin2;
                PLATFORM.screen().schedule_fade(0.25f,
                                                {ColorConstant::rich_black,
                                                 true,
                                                 false,
                                                 false,
                                                 true,
                                                 true});

                PLATFORM.screen().schedule_fade(0.25f,
                                                {ColorConstant::silver_white,
                                                 false,
                                                 false,
                                                 true,
                                                 false,
                                                 true});
            }
            break;

        case LightningState::begin2:
            lightning_timer_ -= delta;
            if (lightning_timer_ <= 0) {
                lightning_timer_ = milliseconds(100);
                ls_ = LightningState::hold;

                for (auto& room : player_island().rooms()) {
                    room->on_lightning();
                }

                if (opponent_island()) {
                    for (auto& room : opponent_island()->rooms()) {
                        room->on_lightning();
                    }
                }

                PLATFORM.screen().schedule_fade(1.f,
                                                {ColorConstant::rich_black,
                                                 true,
                                                 false,
                                                 false,
                                                 true,
                                                 true});

                PLATFORM.screen().schedule_fade(1.f,
                                                {ColorConstant::silver_white,
                                                 false,
                                                 false,
                                                 true,
                                                 false,
                                                 true});
            }
            break;

        case LightningState::hold:
            lightning_timer_ -= delta;
            if (lightning_timer_ <= 0) {
                lightning_timer_ = milliseconds(430);
                ls_ = LightningState::fade;
            }
            break;

        case LightningState::fade:
            lightning_timer_ -= delta;
            if (lightning_timer_ <= 0) {
                lightning_timer_ = 0;
                ls_ = LightningState::none;

                PLATFORM.screen().schedule_fade(0.f);

            } else {
                const auto amount =
                    smoothstep(0.f, milliseconds(430), lightning_timer_);
                PLATFORM.screen().schedule_fade(amount,
                                                {ColorConstant::rich_black,
                                                 true,
                                                 false,
                                                 false,
                                                 true,
                                                 true});

                PLATFORM.screen().schedule_fade(amount,
                                                {ColorConstant::silver_white,
                                                 false,
                                                 false,
                                                 true,
                                                 false,
                                                 true});
            }
            break;
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }
};



} // namespace skyland
