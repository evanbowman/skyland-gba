#include "bell.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/selectMenuScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void Bell::register_select_menu_options(SelectMenuScene& sel)
{
    sel.register_option(
        SystemString::sel_menu_eight_bells,
        [par = parent(), c = position()]() -> ScenePtr {
            auto room = par->get_room(c);
            if (not room) {
                return null_scene();
            }

            auto b = room->cast<Bell>();
            if (not b) {
                return null_scene();
            }

            const auto rx = room->position().x;

            for (auto& r : par->rooms()) {
                for (auto& c : r->characters()) {
                    c->set_spr_flip(c->grid_position().x < rx);
                    c->drop_movement_path();
                }
            }
            auto interval = milliseconds(1300);
            b->schedule_chimes(interval, 4, 1, milliseconds(500));
            auto delay = interval * 4;
            APP.player().delay_crew_automation(delay);


            APP.on_timeout(delay, [isle = par] {
                Buffer<RoomCoord, 20> positions;
                for (auto& r : isle->rooms()) {
                    for (auto& c : r->characters()) {
                        positions.push_back(c->grid_position());
                    }
                }

                rng::shuffle(positions, rng::utility_state);
                for (auto& r : isle->rooms()) {
                    for (auto& c : r->characters()) {
                        auto p1 = c->grid_position();
                        auto p2 = positions.back();
                        positions.pop_back();

                        auto path = find_path(isle, c.get(), p1, p2);
                        if (path and *path) {
                            c->set_movement_path(std::move(*path));
                            c->pin();

                            network::packet::ChrSetTargetV2 packet;
                            packet.target_x_ = p2.x;
                            packet.target_y_ = p2.y;
                            packet.chr_id_.set(c->id());
                            packet.near_island_ =
                                isle not_eq &APP.player_island();
                            network::transmit(packet);
                        }
                    }
                }
            });

            return null_scene();
        });
}



void Bell::update(Time delta)
{
    Room::update(delta);

    if (chime_count_) {
        Room::ready();
        timer_ += delta;
        if (timer_ >= chime_spacing_) {
            timer_ -= chime_spacing_;
            --chime_count_;
            ring();
            if (chime_repeat_) {
                repeat_on_ = true;
                reps_completed_ = 0;
                repeat_timer_ = 0;
            }
        }
    }

    if (repeat_on_) {
        Room::ready();

        const auto repeat_interval = milliseconds(350);

        repeat_timer_ += delta;
        if (repeat_timer_ >= repeat_interval) {
            repeat_timer_ -= repeat_interval;

            ring();
            ++reps_completed_;

            if (reps_completed_ == chime_repeat_) {
                repeat_on_ = false;
            }
        }
    }
}



void Bell::ring()
{
    PLATFORM.speaker().play_sound("ship_bell.raw", 7);
    auto p = visual_center();
    p.y -= 1.0_fixed;
    make_construction_effect(p);
}



void Bell::schedule_chimes(Time chime_spacing,
                           u8 chime_count,
                           u8 chime_repeat,
                           Time start_delay)
{
    timer_ = chime_spacing - start_delay;
    chime_count_ = chime_count;
    chime_spacing_ = chime_spacing;
    chime_repeat_ = chime_repeat;
    repeat_on_ = false;
    reps_completed_ = 0;
    Room::ready();
}



ScenePtr Bell::select_impl(const RoomCoord& cursor)
{
    ring();
    return null_scene();
}



} // namespace skyland
