////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "reconstructionDrone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/minimap.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class SetupReconstructionQueueScene : public ActiveWorldScene
{
public:
    SharedEntityRef<Drone> recons_drone_;


    Optional<Text> selected_text_;


    SetupReconstructionQueueScene(SharedEntityRef<Drone> recons_drone)
        : recons_drone_(recons_drone)
    {
        if (auto attached = recons_drone->attached_to()) {
            if (auto db = attached->cast<DroneBay>()) {
                for (int i = 0; i < db->rq_.size(); ++i) {
                    reconstruction_queue_.push_back(db->rq_[i]);
                }
            }
        }
    }


    void show_count()
    {
        auto st = calc_screen_tiles();

        if (not selected_text_) {
            selected_text_.emplace(OverlayCoord{0, (u8)(st.y - 1)});
        }

        auto str = format(SYSTR(automate_reconstruction_prompt_2)->c_str(),
                          reconstruction_queue_.size(),
                          reconstruction_queue_.capacity());
        selected_text_->assign(str.c_str());
    }


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        Text::print(SYSTR(automate_reconstruction_prompt_1)->c_str(),
                    OverlayCoord{});
        show_count();
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);
        selected_text_.reset();
        PLATFORM.fill_overlay(0);
    }


    ScenePtr update(Time delta) override
    {
        if (auto scene = ActiveWorldScene::update(delta)) {
            return scene;
        }

        auto test_key = [&](Key k) {
            return APP.player().test_key(
                k, milliseconds(500), milliseconds(150));
        };

        auto& cursor_loc = globals().near_cursor_loc_;

        if (test_key(Key::action_2)) {
            if (auto attached = recons_drone_->attached_to()) {
                if (auto db = attached->cast<DroneBay>()) {

                    time_stream::event::AttachReconstructionQueue e;
                    e.db_x_ = attached->position().x;
                    e.db_y_ = attached->position().y;
                    e.previous_queue_memory_ = db->rq_.mem_;
                    e.previous_queue_size_ = db->rq_.count_;
                    APP.time_stream().push(APP.level_timer(), e);

                    db->rq_.clear();
                    for (auto& elem : reconstruction_queue_) {
                        db->rq_.push_back(elem);
                    }

                    db->ready();
                }
            }
            return make_scene<ReadyScene>();
        }

        if (APP.player().key_down(Key::action_1)) {
            if (auto r = APP.player_island().get_room(cursor_loc)) {
                auto mt = r->metaclass();
                auto prop = (*mt)->properties();
                if (prop & RoomProperties::not_constructible or
                    prop & RoomProperties::only_constructible_in_sandbox) {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    return null_scene();
                }
                bool found = false;
                for (auto it = reconstruction_queue_.begin();
                     it not_eq reconstruction_queue_.end();) {

                    auto rc = RoomCoord{it->x_, it->y_};
                    if (APP.player_island().get_room(rc) ==
                        APP.player_island().get_room(cursor_loc)) {
                        found = true;
                        it = reconstruction_queue_.erase(it);
                    } else {
                        ++it;
                    }
                }
                if (not found) {
                    if (reconstruction_queue_.full()) {
                        PLATFORM.speaker().play_sound("beep_error", 3);
                        return null_scene();
                    }
                    reconstruction_queue_.push_back({(u8)r->metaclass_index(),
                                                     r->position().x,
                                                     r->position().y});
                }
                show_count();
            } else {
                for (auto it = reconstruction_queue_.begin();
                     it not_eq reconstruction_queue_.end();) {
                    if (RoomCoord{it->x_, it->y_} == cursor_loc) {
                        it = reconstruction_queue_.erase(it);
                    } else {
                        ++it;
                    }
                }
            }
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                ++cursor_loc.x;
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
            }
        }

        return null_scene();
    }


    void display() override
    {
        auto origin = APP.player_island().visual_origin();

        auto& cursor_loc = globals().near_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16 + 3);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_priority(0);
        sprite.set_texture_index(62);
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_flip({true, false});

        PLATFORM.screen().draw(sprite);

        Sprite sel;
        sel.set_size(Sprite::Size::w16_h16);
        sel.set_tidx_16x16(13, 0);

        for (auto& e : reconstruction_queue_) {
            auto origin = APP.player_island().visual_origin();

            auto mt = load_metaclass(e.block_metaclass_);
            auto sz = (*mt)->size();

            for (int x = 0; x < sz.x; ++x) {
                for (int y = 0; y < sz.y; ++y) {
                    auto o2 = origin;
                    o2.x += Fixnum::from_integer((e.x_ + x) * 16);
                    o2.y += Fixnum::from_integer((e.y_ + y) * 16);

                    sel.set_position(o2);

                    PLATFORM.screen().draw(sel);
                }
            }
        }

        ActiveWorldScene::display();
    }

private:
    Buffer<ReconstructionQueueEntry, reconstruction_queue_size>
        reconstruction_queue_;
};



Coins get_room_cost(Island* island, const RoomMeta& meta);



static Vec2<Fixnum> calc_pos(Island* island, const RoomCoord& grid_coord)
{
    auto o = island->visual_origin();
    o.x += Fixnum::from_integer(grid_coord.x * 16);
    o.y += Fixnum::from_integer(grid_coord.y * 16);
    return o;
}



ReconstructionDrone::ReconstructionDrone(Island* parent,
                                         Island* destination,
                                         const RoomCoord& grid_pos)
    : Drone(get_name(), parent, destination, grid_pos)
{
    sprite_.set_texture_index(50);
    health_ = 100;
}



void ReconstructionDrone::apply_damage(Health amount)
{
    sprite_.set_texture_index(125);
    dodge_frames_ = 6;
}



void ReconstructionDrone::update(Time delta)
{
    if (destination() == APP.opponent_island()) {
        kill();
        return;
    }

    if (dodge_frames_ > 0) {
        --dodge_frames_;
        if (dodge_frames_ == 0) {
            sprite_.set_texture_index(50);
        }
    }

    switch (state_) {
    case State::launch_2:
        state_ = Drone::State::launch;
        Drone::update(delta);
        if (state_ == Drone::State::launch) {
            state_ = State::launch_2;
        }
        break;

    case Drone::State::launch:
        Drone::update(delta);
        break;

    case Drone::State::ready:
        suppress_time_stream_ = true;
        update_sprite();
        sprite_.set_flip({false, false});
        state_ = State::active;
        timer_ = 0;
        if (recons_index_ > -1) {
            auto room = attached_to();
            if (auto db = room->cast<DroneBay>()) {
                if (recons_index_ < db->rq_.size()) {
                    auto entry = db->rq_[recons_index_];
                    RoomCoord pos = {entry.x_, entry.y_};
                    if (not parent()->get_room(pos)) {
                        auto metaclass = load_metaclass(entry.block_metaclass_);
                        auto cost = get_room_cost(parent(), *metaclass);
                        if (cost <= APP.coins()) {
                            APP.set_coins(APP.coins() - cost);
                            (*metaclass)->create(parent(), pos);
                            if (auto room = parent()->get_room(pos)) {
                                make_construction_effect(room->visual_center());
                            }
                            if (not PLATFORM.speaker().is_sound_playing(
                                    "build0")) {
                                PLATFORM.speaker().play_sound("build0", 4);
                            }
                            time_stream::event::PlayerRoomCreated p;
                            p.x_ = pos.x;
                            p.y_ = pos.y;
                            APP.time_stream().push(APP.level_timer(), p);

                            network::packet::RoomConstructed packet;
                            packet.metaclass_index_.set(entry.block_metaclass_);
                            packet.x_ = pos.x;
                            packet.y_ = pos.y;
                            network::transmit(packet);
                        }
                    }
                }
            }
            recons_index_ = -1;
            auto prev = position();
            auto o = calc_pos(parent(), prev);
            anchor_ = Vec2<s16>{(s16)ivec(o).x, (s16)ivec(o).y};
            sprite_.set_flip({prev.x > return_to_.x, false});
            set_movement_target(return_to_);
            timer_ = 1;
            state_ = State::launch_2;
        } else {
            return_to_ = position();
        }
        break;

    case State::active:
        duration_ += delta;
        update_sprite();
        timer_ += delta;
        if (timer_ > milliseconds(400)) {
            health_ += 25;
            health_ = clamp(health_, Health{0}, Health{100});
            timer_ -= milliseconds(400);
            auto room = attached_to();
            if (auto db = room->cast<DroneBay>()) {
                for (int i = 0; i < db->rq_.size(); ++i) {
                    auto entry = db->rq_[i];
                    RoomCoord pos = {entry.x_, entry.y_};
                    bool destroyed =
                        minimap::player_destroyed_rooms.get(pos.x, pos.y);
                    if (destroyed and not parent()->get_room(pos)) {
                        auto o = calc_pos(parent(), position());
                        anchor_ = Vec2<s16>{(s16)ivec(o).x, (s16)ivec(o).y};
                        sprite_.set_flip({pos.x < position().x, false});
                        set_movement_target(pos);
                        timer_ = 1;
                        state_ = State::launch_2;
                        recons_index_ = i;
                    }
                }
            }
        }
        break;
    }
}



ScenePtr ReconstructionDrone::select()
{
    PLATFORM.speaker().play_sound("drone_beep", 1);
    return make_scene<SetupReconstructionQueueScene>(shared_from_this());
}



} // namespace skyland
