////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "constructionScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "modules/sandboxLoaderModule.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "salvageRoomScene.hpp"
#include "selectMenuScene.hpp"
#include "skyland/minimap.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ConstructionAnim : public Entity
{
public:
    ConstructionAnim(Vec2<Fixnum> pos) : Entity({})
    {
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_tidx_16x16(31, 0);
        sprite_.set_position(pos);
        sprite_.set_origin({});
        sprite_.set_mix({custom_color(0x80a5cd), 40});
    }


    void update(Time delta) override
    {
        // The game manipulates the time delta for slow motion stuff, etc. But
        // we always want this UI effect to play at the same rate.
        delta = PLATFORM.delta_clock().last_delta();

        timer_ += delta;
        if (timer_ >= milliseconds(80)) {
            timer_ -= milliseconds(80);
            auto t = sprite_.get_texture_index();
            if (t == 31 * 2 + 5) {
                kill();
                return;
            }

            ++t;
            sprite_.set_texture_index(t);
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }


    Sprite& sprite()
    {
        return sprite_;
    }

private:
    Time timer_ = 0;
};



void make_construction_effect(Vec2<Fixnum> pos)
{
    auto segment = [&](Fixnum xoff, Fixnum yoff, bool xflip, bool yflip) {
        auto p = pos;
        p.x += xoff;
        p.y += yoff;
        if (auto e = APP.alloc_entity<ConstructionAnim>(p)) {
            e->sprite().set_flip({xflip, yflip});
            APP.effects().push(std::move(e));
        }
    };

    segment(Fixnum::from_integer(-16), Fixnum::from_integer(-16), false, false);
    segment(Fixnum::from_integer(-16), 0.0_fixed, false, true);
    segment(0.0_fixed, Fixnum::from_integer(-16), true, false);
    segment(0.0_fixed, 0.0_fixed, true, true);
}



u16 room_category_icon(Room::Category category)
{
    return 379 + static_cast<u16>(category);
}



Island* ConstructionScene::island()
{
    if (near_) {
        return &APP.player_island();
    } else if (APP.opponent_island()) {
        return APP.opponent_island();
    } else {
        return nullptr;
    }
}



static Sound sound_openbag("openbag");



bool ConstructionScene::constrain_;



bool ConstructionScene::camera_update_check_key()
{
    if (state_ == State::choose_building) {
        return false;
    }
    return APP.player().key_pressed(Key::left) or
           APP.player().key_pressed(Key::right) or
           APP.player().key_pressed(Key::up) or
           APP.player().key_pressed(Key::down) or
           APP.player().key_pressed(Key::select);
}



Coins get_room_cost(Island* island, const RoomMeta& meta)
{
    Coins cost = meta->cost();
    for (int i = 0; i < island->workshop_count() + island->manufactory_count();
         ++i) {
        cost *= 0.9f;
    }
    if (cost < meta->cost() * salvage_factor) {
        // You shouldn't be able to farm coins by building a bunch of workshops
        // and salvaging rooms repeatedly.
        return meta->cost() * salvage_factor;
    }
    return cost;
}



static bool show_construction_icons = true;



bool tapped_topleft_corner();



Optional<RoomCoord> get_local_tapclick(Island* island, const Vec2<u32>& pos)
{
    const auto view_offset = PLATFORM.screen().get_view().int_center();

    auto island_pos = island->get_position();
    island_pos.x -= Fixnum::from_integer(view_offset.x);
    island_pos.y -= Fixnum::from_integer(view_offset.y);

    if (Fixnum::from_integer(pos.x) >= island_pos.x and
        Fixnum::from_integer(pos.x) <=
            island_pos.x +
                Fixnum::from_integer((1 + island->terrain().size()) * 16)) {

        int x_tile = -((island_pos.x.as_integer() - pos.x) / 16);
        int y_tile = -((island_pos.y.as_integer() - pos.y) / 16);

        y_tile += 31; // FIXME!

        return {{(u8)x_tile, (u8)y_tile}};
    }

    return {};
}



void shift_rooms_right(Island& island)
{
    auto tmp = allocate_dynamic<Buffer<Room*, 100>>("shift-buf");
    for (auto& room : island.rooms()) {
        tmp->push_back(room.get());
    }
    for (auto& r : reversed(*tmp)) {
        island.move_room(r->position(),
                         {(u8)(r->position().x + 1), r->position().y});
    }
    // NOTE: because we shifted all blocks to the right by one
    // coordinate, a drone may now be inside of a block, which we
    // don't want to deal with at the moment, so just destroy them
    // all.
    for (auto& d : island.drones()) {
        d->apply_damage(999);
        if (d->alive()) { // For damage-impervious drones
            d->kill();
        }
    }
    // Furthermore... all weapons on the players' island need to
    // have their targets adjusted accordingly:
    Island* other_island = (is_player_island(&island)) ? APP.opponent_island()
                                                       : &APP.player_island();

    if (other_island) {
        for (auto& r : other_island->rooms()) {
            if (auto t = r->get_target()) {
                t->x += 1;
                r->set_target(*t, r->target_pinned());
            }
        }
    }
}



ScenePtr ConstructionScene::update(Time delta)
{
    auto& cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    auto fixup_cursor = [&] {
        if ((s8)cursor_loc.x < 0) {
            cursor_loc.x = 0;
        }
        if (cursor_loc.y == 15) {
            cursor_loc.y = 14;
        }
    };

    auto exit_scene = [this, fixup_cursor]() -> ScenePtr {
        fixup_cursor();
        if (near_) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<InspectP2Scene>();
        }
    };

    if (auto new_scene = ActiveWorldScene::update(delta)) {
        fixup_cursor();
        return new_scene;
    }

    if (APP.player().key_down(Key::select)) {
        return make_scene<SelectMenuScene>();
    }

    if (not island()) {
        return exit_scene();
    }

    flicker_timer_ += delta;
    if (flicker_timer_ > milliseconds(300)) {
        flicker_timer_ -= milliseconds(300);
        flicker_on_ = not flicker_on_;
    }

    if (tapped_topleft_corner() or APP.player().key_down(Key::alt_2) or
        (state_ == State::select_loc and
         APP.player().key_down(Key::action_2))) {
        if (not data_->construction_sites_.empty()) {
            cursor_loc.x = data_->construction_sites_[selector_].x;
            cursor_loc.y = data_->construction_sites_[selector_].y;
        }
        return exit_scene();
    }


    auto tapclick = [&]() -> std::optional<Vec2<s8>> {
        if (auto pos = APP.player().tap_released()) {
            auto clk = get_local_tapclick(island(), *pos);

            if (clk) {
                return clk->cast<s8>();
            }
        }
        return std::nullopt;
    }();


    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    bool sync_cursor = false;


    switch (state_) {
    case State::select_loc:

        if ( // NOTE: just because I don't want to retest the tutorials.
            APP.game_mode() not_eq App::GameMode::tutorial and
            island()->checksum() not_eq checksum_) {
            find_construction_sites();
            category_label_.reset();
            msg(SYSTR(construction_build)->c_str());
            checksum_ = island()->checksum();
        }

        if (test_key(Key::right)) {
            stack_ = 0;
            if (selector_ < data_->construction_sites_.size() - 1) {
                ++selector_;

                sync_cursor = true;
                PLATFORM.speaker().play_sound("cursor_tick", 0);

            } else if (near_ and APP.game_mode() == App::GameMode::sandbox and
                       APP.opponent_island()) {
                auto& cursor_loc = globals().far_cursor_loc_;

                PLATFORM.speaker().play_sound("cursor_tick", 0);

                cursor_loc.x = 0;
                cursor_loc.y = globals().near_cursor_loc_.y;
                return make_scene<ConstructionScene>(false);
            }
        } else if (test_key(Key::left)) {
            stack_ = 0;
            if (selector_ > 0) {
                --selector_;

                sync_cursor = true;

                PLATFORM.speaker().play_sound("cursor_tick", 0);

            } else if (not near_ and
                       APP.game_mode() == App::GameMode::sandbox) {
                auto& cursor_loc = globals().near_cursor_loc_;

                cursor_loc.x = APP.player_island().terrain().size();
                cursor_loc.y = globals().far_cursor_loc_.y;

                PLATFORM.speaker().play_sound("cursor_tick", 0);
                return make_scene<ConstructionScene>(true);
            }
        } else if (test_key(Key::up)) {
            stack_ = 0;
            if (selector_ > 0 and
                data_->construction_sites_[selector_].x ==
                    data_->construction_sites_[selector_ - 1].x) {

                --selector_;
                sync_cursor = true;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            stack_ = 0;
            if (selector_ < data_->construction_sites_.size() - 1 and
                data_->construction_sites_[selector_].x ==
                    data_->construction_sites_[selector_ + 1].x) {

                ++selector_;
                sync_cursor = true;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (not data_->construction_sites_.empty()) {
            cursor_loc.x = data_->construction_sites_[selector_].x;
            cursor_loc.y = data_->construction_sites_[selector_].y;

            if (sync_cursor) {
                APP.player().network_sync_cursor(cursor_loc, 3, true);
            }
        }

        if (((tapclick and
              *tapclick == data_->construction_sites_[selector_]) or
             APP.player().key_down(Key::action_1)) and
            not data_->construction_sites_.empty()) {

            tapclick.reset();

            if (data_->construction_sites_[selector_].y == 15) {
                // Special case: we want to add to the terrain level, not
                // construct a building.
                state_ = State::add_terrain;
                StringBuffer<30> temp;
                temp += *SYSTR(construction_add_terrain);
                temp += stringify(APP.terrain_cost(*island()));
                temp += "@";
                msg(temp.c_str());

            } else {
                if (not collect_available_buildings()) {
                    // Do nothing...
                } else if (not data_->available_buildings_.empty()) {

                    if (data_->last_constructed_building_ and
                        (data_->available_buildings_[building_selector_] not_eq
                         *data_->last_constructed_building_)) {

                        // Ok, so if we constructed a building, and the cursor
                        // advanced into a narrower slot, we may have fewer
                        // options for stuff to build. i.e. the
                        // available_buildings array was effectively
                        // re-indexed. If our former selection still exists in
                        // the list, adjust the selector accordingly. This isn't
                        // strictly necessary, it just makes the UI feel nicer to
                        // the player.

                        for (u32 i = 0; i < data_->available_buildings_.size();
                             ++i) {
                            if (data_->available_buildings_[i] ==
                                *data_->last_constructed_building_) {
                                building_selector_ = i;
                                break;
                            }
                        }
                    }

                    flicker_on_ = false;
                    flicker_timer_ = -milliseconds(150);
                    state_ = State::choose_building;
                    sound_openbag.play(1, seconds(2));
                    sound_openbag.reset();
                    show_current_building_text();
                }
            }
        } else if (tapclick or APP.player().touch_held(milliseconds(200))) {

            if (APP.player().touch_held(milliseconds(200))) {
                // If the player presses and holds the touch screen, scroll
                // through available construction sites.
                if (auto pos = APP.player().touch_current()) {
                    if (auto t = get_local_tapclick(island(), *pos)) {
                        tapclick = t->cast<s8>();
                    }
                }
            }

            if (tapclick) {
                // First try to find an exact match. If not, try a loose match
                // based on x coordinate.
                for (u32 i = 0; i < data_->construction_sites_.size(); ++i) {
                    if (data_->construction_sites_[i] == *tapclick) {
                        tapclick.reset();
                        selector_ = i;
                        camera_update_timer_ = milliseconds(500);
                        break;
                    }
                }
                if (tapclick) {
                    for (u32 i = 0; i < data_->construction_sites_.size();
                         ++i) {
                        if (data_->construction_sites_[i].x == tapclick->x) {
                            tapclick.reset();
                            selector_ = i;
                            camera_update_timer_ = milliseconds(500);
                            break;
                        }
                    }
                }
            }
        }
        break;

    case State::choose_building: {
        if (APP.player().key_down(Key::start)) {
            auto mt = data_->available_buildings_[building_selector_];
            auto next = make_scene<GlossaryViewerModule>(mt);
            category_label_.reset();
            next->skip_categories();
            if (next) {
                const bool near = near_;
                const u8 mti_8 = mt;
                next->set_next_scene([near, mti_8]() {
                    auto s = make_scene<ConstructionScene>(near);
                    s->open_prompt_at(mti_8);
                    return s;
                });
                return next;
            }
        }

        auto scroll_right = [&] {
            flicker_on_ = false;
            flicker_timer_ = -milliseconds(150);
            PLATFORM.speaker().play_sound("click", 1);
            if (building_selector_ <
                (int)data_->available_buildings_.size() - 1) {
                ++building_selector_;
                show_current_building_text();
            } else {
                building_selector_ = 0;
                show_current_building_text();
            }
        };
        auto scroll_left = [&] {
            flicker_on_ = false;
            flicker_timer_ = -milliseconds(150);
            PLATFORM.speaker().play_sound("click", 1);
            if (building_selector_ > 0) {
                --building_selector_;
                show_current_building_text();
            } else {
                building_selector_ = data_->available_buildings_.size() - 1;
                show_current_building_text();
            }
        };
        if (APP.player().touch_held(milliseconds(200))) {
            if (auto p = APP.player().touch_current()) {
                if (last_touch_x_) {
                    touchscroll_ += p->x - last_touch_x_;
                    last_touch_x_ = p->x;
                } else {
                    last_touch_x_ = p->x;
                }
            }
            if (touchscroll_ < -16) {
                touchscroll_ = 0;
                scroll_right();
            } else if (touchscroll_ > 16) {
                touchscroll_ = 0;
                scroll_left();
            }
        } else if (APP.player().key_down(Key::action_2) or
                   (tapclick and
                    *tapclick not_eq data_->construction_sites_[selector_])) {
            find_construction_sites();
            state_ = State::select_loc;
            category_label_.reset();
            msg(SYSTR(construction_build)->c_str());
            last_touch_x_ = 0;
            break;
        } else {
            last_touch_x_ = 0;
        }

        if (APP.player().key_down(Key::down)) {
            PLATFORM.speaker().play_sound("click", 1);

            flicker_on_ = false;
            flicker_timer_ = -milliseconds(150);

            show_category_ = true;

            auto current_category =
                (*load_metaclass(
                     data_->available_buildings_[building_selector_]))
                    ->category();
            // When the player presses down, jump to the next building in the
            // list of a different category. The room metatable should be
            // organized by room category by convention.
            u32 i = 0;
            for (i = 0; i < data_->available_buildings_.size(); ++i) {
                auto other_category =
                    (*load_metaclass(
                         data_->available_buildings_[(building_selector_ + i) %
                                                     data_->available_buildings_
                                                         .size()]))
                        ->category();
                if (other_category not_eq current_category) {
                    break;
                }
            }
            building_selector_ += i;
            building_selector_ %= data_->available_buildings_.size();
            show_current_building_text();
        }

        if (APP.player().key_down(Key::up)) {
            PLATFORM.speaker().play_sound("click", 1);
            const auto current_category =
                (*load_metaclass(
                     data_->available_buildings_[building_selector_]))
                    ->category();

            show_category_ = true;

            Room::Category target_category = current_category;

            flicker_on_ = false;
            flicker_timer_ = -milliseconds(150);


            u32 i = 0;
            for (i = 0; i < data_->available_buildings_.size(); ++i) {
                int index = building_selector_ - i;
                if (index < 0) {
                    index += data_->available_buildings_.size();
                }

                auto other_category =
                    (*load_metaclass(data_->available_buildings_[index]))
                        ->category();
                if (other_category not_eq current_category) {
                    target_category = other_category;
                    break;
                }
            }

            // The above loop seeks the first category that differs from our
            // own. But we want to seek to the beginning of the category, not
            // the end, hence the next loop:
            for (i = 0; i < data_->available_buildings_.size(); ++i) {
                if ((*load_metaclass(data_->available_buildings_[i]))
                        ->category() == target_category) {
                    break;
                }
            }
            building_selector_ = i;
            show_current_building_text();
        }

        if (test_key(Key::right)) {
            scroll_right();
        }

        if (test_key(Key::left)) {
            scroll_left();
        }

        if (tapclick == data_->construction_sites_[selector_] or
            APP.player().key_down(Key::action_1)) {

            auto mti = data_->available_buildings_[building_selector_];

            const auto& target = *load_metaclass(mti);

            if (target->properties() & RoomProperties::singleton) {
                bool dup = false;
                for (auto& room : island()->rooms()) {
                    if (room->metaclass_index() == mti) {
                        dup = true;
                        break;
                    }
                }

                if (dup) {
                    category_label_.reset();
                    msg(SYSTR(construciton_one_allowed)->c_str());
                    PLATFORM.speaker().play_sound("beep_error", 2);
                    state_ = State::insufficient_funds;
                    break;
                }
            }

            if (APP.coins() < get_room_cost(island(), target)) {
                category_label_.reset();
                msg(SYSTR(construction_insufficient_funds)->c_str());
                PLATFORM.speaker().play_sound("beep_error", 2);
                state_ = State::insufficient_funds;
                break;
            }

            if (target->consumes_power() > 0 and
                island()->power_supply() - island()->power_drain() <
                    target->consumes_power()) {
                category_label_.reset();
                msg(SYSTR(construction_insufficient_power_supply)->c_str());
                PLATFORM.speaker().play_sound("beep_error", 2);
                state_ = State::insufficient_funds;
                break;
            }

            if (globals().room_pools_.empty() or island()->rooms().full()) {
                category_label_.reset();
                msg(SYSTR(construction_too_many_rooms)->c_str());
                PLATFORM.speaker().play_sound("beep_error", 2);
                state_ = State::insufficient_funds;
                break;
            }

            if (not site_has_space(mti)) {
                category_label_.reset();
                PLATFORM.speaker().play_sound("beep_error", 2);
                msg(SYSTR(construction_not_enough_space)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            const auto diff = get_room_cost(island(), target);
            APP.set_coins(APP.coins() - diff);
            APP.level_coins_spent() += diff;

            const auto sz = target->size().y;
            const u8 dest_x = data_->construction_sites_[selector_].x;
            const u8 dest_y =
                data_->construction_sites_[selector_].y - (sz - 1);

            PLATFORM.speaker().play_sound("build0", 4);

            target->create(island(), {dest_x, dest_y});
            data_->last_constructed_building_ = metaclass_index(target->name());

            stack_ += target->size().y;

            checksum_ = island()->checksum();

            if (str_eq(target->name(), "workshop")) {
                APP.persistent_data().set_flag(PersistentData::workshop_built);
            }

            APP.player().rooms_built_++;

            auto mt_index = metaclass_index(target->name());

            network::packet::RoomConstructed packet;
            packet.metaclass_index_.set(mt_index);
            packet.x_ = dest_x;
            packet.y_ = dest_y;
            network::transmit(packet);


            if (near_) {
                time_stream::event::PlayerRoomCreated p;
                p.x_ = dest_x;
                p.y_ = dest_y;
                APP.time_stream().push(APP.level_timer(), p);
            } else {
                time_stream::event::OpponentRoomCreated p;
                p.x_ = dest_x;
                p.y_ = dest_y;
                APP.time_stream().push(APP.level_timer(), p);
            }


            if (auto room = island()->get_room({dest_x, dest_y})) {
                room->init_ai_awareness();

                if (APP.game_speed() == GameSpeed::stopped) {

                    room->init_ai_awareness_upon_unpause();

                    if (str_eq("cloak", room->name())) {
                        for (auto& room : island()->rooms()) {
                            if (room->should_init_ai_awareness_upon_unpause()) {
                                room->init_ai_awareness();
                                room->init_ai_awareness_upon_unpause();
                            }
                        }
                    }
                }

                make_construction_effect(room->visual_center());

                if (auto scene = room->setup()) {
                    fixup_cursor();
                    return scene;
                }
            }


            find_construction_sites();

            category_label_.reset();
            state_ = State::select_loc;
            msg(SYSTR(construction_build)->c_str());
        }
        break;
    }

    case State::insufficient_funds:
        if (APP.player().key_down(Key::action_2) or
            APP.player().key_down(Key::action_1)) {
            find_construction_sites();
            state_ = State::select_loc;
            msg(SYSTR(construction_build)->c_str());
        }
        break;

    case State::add_terrain:
        if (APP.player().key_down(Key::action_2)) {
            find_construction_sites();
            state_ = State::select_loc;
            msg(SYSTR(construction_build)->c_str());
            break;
        }

        if (APP.player().key_down(Key::action_1)) {
            if (APP.coins() < APP.terrain_cost(*island())) {
                msg(SYSTR(construction_build)->c_str());
                state_ = State::insufficient_funds;
                break;
            }

            APP.set_coins(APP.coins() - APP.terrain_cost(*island()));

            time_stream::event::IslandTerrainChanged e;
            e.previous_terrain_size_ = island()->terrain().size();
            e.near_ = is_player_island(island());
            APP.time_stream().push(APP.level_timer(), e);


            auto& terrain = island()->terrain();
            terrain.pop_back(); // the old edge tile
            terrain.push_back(Tile::terrain_middle);
            terrain.push_back(Tile::terrain_right);

            PLATFORM.speaker().play_sound("gravel", 4);

            island()->schedule_repaint();


            if (data_->construction_sites_[selector_].x == -1) {
                shift_rooms_right(*island());
                network::packet::TerrainConstructedLeft packet;
                packet.new_terrain_size_ = island()->terrain().size();
                network::transmit(packet);
            } else {
                network::packet::TerrainConstructed packet;
                packet.new_terrain_size_ = island()->terrain().size();
                network::transmit(packet);
            }

            find_construction_sites();
            state_ = State::select_loc;

            msg(SYSTR(construction_build)->c_str());
        }
        break;
    }

    return null_scene();
}


void ConstructionScene::show_current_building_text()
{
    if (APP.game_mode() == App::GameMode::tutorial) {
        show_category_ = false;
    }

    auto current_category =
        (*load_metaclass(data_->available_buildings_[building_selector_]))
            ->category();

    auto category_str = (SystemString)(((int)SystemString::category_begin) +
                                       (int)current_category);

    StringBuffer<48> category_str_buffer;

    if (show_category_) {
        category_str_buffer = loadstr(category_str)->c_str();
    }

    if (current_category not_eq last_category_) {
        category_label_.reset();
    }

    StringBuffer<32> str = SYSTR(construction_build)->c_str();
    str += " :";

    str += (*load_metaclass(data_->available_buildings_[building_selector_]))
               ->ui_name()
               ->c_str();

    str += " ";
    str += stringify(get_room_cost(
        island(),
        (*load_metaclass(data_->available_buildings_[building_selector_]))));
    str += "@";
    str += " ";
    str += stringify(
        (*load_metaclass(data_->available_buildings_[building_selector_]))
            ->consumes_power());
    str += "`";

    msg(str.c_str());


    auto st = calc_screen_tiles();

    if (not show_construction_icons) {
        return;
    }

    for (int i = st.x - 25;
         i < int((st.x - 5) - utf8::len(category_str_buffer.c_str()));
         ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        PLATFORM.set_tile(Layer::overlay, st.x - 26, y, 130);
        PLATFORM.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);

    {
        int index = building_selector_;
        if (index - 2 < -1) {
            index = data_->available_buildings_.size() - 2;
        } else if (index - 2 < 0) {
            index = data_->available_buildings_.size() - 1;
        } else {
            index = index - 2;
        }

        auto icon =
            (*load_metaclass(data_->available_buildings_[index]))->unsel_icon();
        draw_image(258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = building_selector_;
        if (index - 1 < 0) {
            index = data_->available_buildings_.size() - 1;
        } else {
            index = index - 1;
        }

        auto icon =
            (*load_metaclass(data_->available_buildings_[index]))->unsel_icon();
        draw_image(181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon =
            (*load_metaclass(data_->available_buildings_[building_selector_]))
                ->icon();
        draw_image(197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = building_selector_;
        if (index + 1 >= (int)data_->available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon =
            (*load_metaclass(data_->available_buildings_[index]))->unsel_icon();
        draw_image(213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = building_selector_;
        if (index + 1 >= (int)data_->available_buildings_.size()) {
            index = 1;
        } else if (index + 2 >= (int)data_->available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon =
            (*load_metaclass(data_->available_buildings_[index]))->unsel_icon();
        draw_image(274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(274, icon, 16);
    }

    if (show_category_) {

        u8 x = st.x;
        x -= 5;
        x -= utf8::len(category_str_buffer.c_str());

        auto coord = OverlayCoord{x, u8(st.y - 6)};

        if (current_category not_eq last_category_ or not category_label_) {
            category_label_.emplace(coord);
            category_label_->assign(category_str_buffer.c_str(),
                                    FontColors{ColorConstant::med_blue_gray,
                                               ColorConstant::rich_black});
        }

        last_category_ = current_category;

        PLATFORM.set_tile(Layer::overlay, x - 1, st.y - 6, 419);

        for (int i = x; i < x + category_label_->len(); ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 7, 425);
        }

        PLATFORM.set_tile(Layer::overlay, st.x - 5, st.y - 6, 433);
    }
}



void draw_required_space(Island& island,
                         const Vec2<Fixnum> origin,
                         const Vec2<u8>& sz,
                         Room::WeaponOrientation o)
{
    auto ocpy = origin;

    if (sz.x == 1 and sz.y == 1) {
        Sprite sprite;
        sprite.set_tidx_16x16(13, 1);
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_position({origin.x, origin.y});
        PLATFORM.screen().draw(sprite);
    } else if (sz.x == 2 and sz.y == 1) {
        Sprite sprite;
        sprite.set_tidx_16x16(13, 1);
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_position({origin.x, origin.y});
        PLATFORM.screen().draw(sprite);
        sprite.set_position({origin.x + 16.0_fixed, origin.y});
        PLATFORM.screen().draw(sprite);
    } else {
        Sprite sprite;
        sprite.set_texture_index(13);
        sprite.set_size(Sprite::Size::w16_h32);

        for (int x = 0; x < sz.x; ++x) {
            for (int y = 0; y < sz.y / 2; ++y) {
                sprite.set_position({origin.x + Fixnum::from_integer(x * 16),
                                     origin.y + Fixnum::from_integer(y * 32)});
                PLATFORM.screen().draw(sprite);
            }
        }

        if (sz.y % 2 not_eq 0) {
            // Odd sized room in y direction. Draw bottom row:
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);
            for (int x = 0; x < sz.x; ++x) {
                sprite.set_position(
                    {origin.x + Fixnum::from_integer(x * 16),
                     origin.y + Fixnum::from_integer((sz.y - 2) * 16 + 16)});
                PLATFORM.screen().draw(sprite);
            }
        }
    }

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h16);
    switch (o) {
    case Room::WeaponOrientation::none:
        break;

    case Room::WeaponOrientation::horizontal: {
        spr.set_tidx_16x16(97, 1);
        const bool flip = &island == APP.opponent_island();
        spr.set_flip({flip, false});
        if (flip) {
            ocpy.x -= 16.0_fixed;
        } else {
            ocpy.x += 16.0_fixed;
            ocpy.x += Fixnum::from_integer((sz.x - 1) * 16);
        }
        const auto ys = sz.y;
        if (ys > 1) {
            auto offset = Fixnum::from_integer(((ys - 1)) * 16);
            offset *= 0.5_fixed;
            ocpy.y += offset;
        }
        spr.set_position(ocpy);
        PLATFORM.screen().draw(spr);
        break;
    }

    case Room::WeaponOrientation::vertical: {
        spr.set_tidx_16x16(98, 0);
        const auto xs = sz.x;
        if (xs > 1) {
            auto offset = Fixnum::from_integer(((xs - 1)) * 16);
            offset *= 0.5_fixed;
            ocpy.x += offset;
        }
        ocpy.y -= 16.0_fixed;
        spr.set_position(ocpy);
        PLATFORM.screen().draw(spr);
        break;
    }
    }
}



void ConstructionScene::display()
{
    if (not island()) {
        return;
    }

    if (stack_ > 4) {
        camera_update_timer_ = milliseconds(500);
    }

    switch (state_) {
    case State::insufficient_funds:
        break;

    case State::select_loc:
        if (not data_->construction_sites_.empty()) {
            auto origin = island()->visual_origin();

            origin.x += Fixnum::from_integer(
                data_->construction_sites_[selector_].x * 16);
            origin.y += Fixnum::from_integer(
                (data_->construction_sites_[selector_].y) * 16);

            Sprite sprite;
            sprite.set_position(origin);

            if (data_->construction_sites_[selector_].y == 15) {
                // Display a different icon when constructing terrain, as a hint
                // to the player that he/she can expand an island's terrain.
                sprite.set_tidx_16x16(73, 0);
                sprite.set_size(Sprite::Size::w16_h16);
            } else {
                sprite.set_tidx_16x16(12, 0);
                sprite.set_size(Sprite::Size::w16_h16);
            }

            if (data_->construction_sites_[selector_].x == -1) {
                sprite.set_flip({true, false});
            }


            PLATFORM.screen().draw(sprite);

            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_flip({});

            if (data_->construction_sites_[selector_].y == 15) {
                origin = island()->visual_origin();
                origin.x +=
                    Fixnum::from_integer((island()->terrain().size() - 1) * 16);
                origin.y += 15.0_fixed * 16.0_fixed;

                int tid_1 = 0;
                int tid_2 = 1;

                if (data_->construction_sites_[selector_].x == -1) {
                    sprite.set_flip({true, false});
                    std::swap(tid_1, tid_2);
                    origin.x = island()->visual_origin().x - 16.0_fixed;
                }
                sprite.set_size(Sprite::Size::w16_h16);
                sprite.set_position(origin);
                sprite.set_tidx_16x16(14, tid_1);
                sprite.set_alpha(Sprite::Alpha::translucent);
                PLATFORM.screen().draw(sprite);
                sprite.set_tidx_16x16(14, tid_2);
                origin.x += 16.0_fixed;
                sprite.set_position(origin);
                PLATFORM.screen().draw(sprite);
                sprite.set_size(Sprite::Size::w16_h32);
            } else if ((u32)data_->construction_sites_[selector_].x ==
                       island()->terrain().size() - 1) {
                origin = island()->visual_origin();
                origin.x +=
                    Fixnum::from_integer((island()->terrain().size()) * 16);
                origin.y += 15.0_fixed * 16.0_fixed;

                sprite.set_size(Sprite::Size::w16_h16);
                sprite.set_position(origin);
                sprite.set_tidx_16x16(103, 0);
                sprite.set_alpha(Sprite::Alpha::translucent);
                PLATFORM.screen().draw(sprite);
            }

            if (data_->construction_sites_[selector_].x == 0) {
                origin = island()->visual_origin();
                origin.x -= 16.0_fixed;
                origin.y += 15.0_fixed * 16.0_fixed;

                sprite.set_size(Sprite::Size::w16_h16);
                sprite.set_position(origin);
                sprite.set_flip({true, false});
                sprite.set_tidx_16x16(103, 0);
                sprite.set_alpha(Sprite::Alpha::translucent);
                PLATFORM.screen().draw(sprite);
            }
        }
        break;


    case State::choose_building:
        if (not data_->available_buildings_.empty()) {
            const auto& meta = *load_metaclass(
                data_->available_buildings_[building_selector_]);
            const auto sz = meta->size();

            auto origin = island()->visual_origin();
            origin.x += Fixnum::from_integer(
                data_->construction_sites_[selector_].x * 16);
            origin.y += Fixnum::from_integer(
                (data_->construction_sites_[selector_].y - (sz.y - 1)) * 16);

            auto rloc = data_->construction_sites_[selector_];
            rloc.y -= sz.y - 1;

            for (u8 x = rloc.x; x < rloc.x + sz.x; ++x) {
                for (u8 y = rloc.y; y < rloc.y + sz.y; ++y) {
                    if (island()->get_room({x, y}) or
                        x >= (int)island()->terrain().size()) {

                        if (flicker_on_) {
                            Sprite spr;
                            spr.set_tidx_16x16(13, 1);
                            spr.set_size(Sprite::Size::w16_h16);
                            auto origin = island()->visual_origin();
                            origin.x += Fixnum(x * 16);
                            origin.y += Fixnum(y * 16);
                            spr.set_position({origin.x, origin.y});
                            spr.set_mix({custom_color(0xf7ce9e), 250});
                            PLATFORM.screen().draw(spr);
                        }
                    }
                }
            }

            auto o = meta->weapon_orientation();
            draw_required_space(*island(), origin, sz, o);
        }
        break;


    case State::add_terrain: {
        auto& terrain = island()->terrain();
        const RoomCoord loc = {u8(terrain.size()), 15};
        auto origin = island()->visual_origin();
        origin.x += Fixnum::from_integer(loc.x * 16);
        if (data_->construction_sites_[selector_].x == -1) {
            origin.x = island()->visual_origin().x - 16.0_fixed;
        }
        origin.y -= 32.0_fixed;
        Sprite sprite;
        sprite.set_tidx_16x16(13, 1);
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_position({origin.x, origin.y + 16.0_fixed});
        PLATFORM.screen().draw(sprite);
        sprite.set_size(Sprite::Size::w16_h32);

        int tid_1 = 0;
        int tid_2 = 1;

        if (data_->construction_sites_[selector_].x == -1) {
            sprite.set_flip({true, false});
            std::swap(tid_1, tid_2);
            origin.x = island()->visual_origin().x - 16.0_fixed;
            origin.y = island()->visual_origin().y + 15.0_fixed * 16.0_fixed;
        } else {
            origin = island()->visual_origin();
            origin.x +=
                Fixnum::from_integer((island()->terrain().size() - 1) * 16);
            origin.y += 15.0_fixed * 16.0_fixed;
        }
        {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_position(origin);
            sprite.set_tidx_16x16(14, tid_1);
            sprite.set_alpha(Sprite::Alpha::translucent);
            PLATFORM.screen().draw(sprite);
            sprite.set_tidx_16x16(14, tid_2);
            origin.x += 16.0_fixed;
            sprite.set_position(origin);
            PLATFORM.screen().draw(sprite);
        }
        break;
    }
    }

    WorldScene::display();
}



void ConstructionScene::find_construction_sites()
{
    data_->construction_sites_.clear();

    bool matrix[16][16];

    checksum_ = island()->checksum();

    const auto& terrain = island()->terrain();
    if (not terrain.full() and
        APP.game_mode() not_eq App::GameMode::multiplayer) {
        // Construct terrain on lefthand side of island
        data_->construction_sites_.push_back({-1, 15});
    }


    island()->plot_construction_zones(matrix);

    for (s8 x = 0; x < 16; ++x) {
        for (s8 y = 0; y < 16; ++y) {
            if (matrix[x][y] and y >= construction_zone_min_y) {
                data_->construction_sites_.push_back({x, y});
            }
        }
    }

    if (not terrain.full() and
        APP.game_mode() not_eq App::GameMode::multiplayer) {
        data_->construction_sites_.push_back({s8(terrain.size()), 15});
    }

    if (data_->construction_sites_.empty()) {
        selector_ = 0;
    } else if (selector_ >= data_->construction_sites_.size()) {
        selector_--;
    }
}



void ConstructionScene::msg(const char* text)
{
    auto st = calc_screen_tiles();
    text_.emplace(text, OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 3, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 5, 0);

        if (not category_label_ or
            (category_label_ and
             (i < category_label_->coord().x or
              i >= category_label_->coord().x + category_label_->len()))) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 0);
        }

        if (show_category_) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 7, 0);
        }
    }
}



bool ConstructionScene::site_has_space(MetaclassIndex m)
{
    u8 matrix[16][16];
    island()->plot_rooms(matrix);

    int avail_x_space = 0;

    // Avail y space per x coordinate of the room that we're constructing.
    u8 avail_y_space[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    const auto current = data_->construction_sites_[selector_];

    for (int x = current.x; x < (int)island()->terrain().size(); ++x) {
        if (matrix[x][current.y]) {
            break;
        }
        for (int y = current.y; y > construction_zone_min_y - 1; --y) {
            if (matrix[x][y]) {
                break;
            }
            ++avail_y_space[x - current.x];
        }
        ++avail_x_space;
    }

    if (avail_x_space == 0) {
        // We should have started with a construction site known to be valid, so
        // the minimum space available should 1,1.
        //
        // P.S.: after adding mycelium, technically something can grow in the
        // available slot, so return a failure.
        return false;
    }

    auto calc_avail_y_space = [&](int room_width) {
        int min = avail_y_space[0];
        for (int x = 0; x < room_width; ++x) {
            if (avail_y_space[x] < min) {
                min = avail_y_space[x];
            }
        }
        return min;
    };

    auto& meta = *load_metaclass(m);

    return meta->size().x <= avail_x_space and
           meta->size().y <= calc_avail_y_space(meta->size().x);
}



bool ConstructionScene::collect_available_buildings()
{
    data_->available_buildings_.clear();

    const auto w_count =
        island()->workshop_count() + island()->manufactory_count();

    const auto f_count = island()->manufactory_count();

    auto metatable = room_metatable();
    for (MetaclassIndex i = 0; i < metatable.second; ++i) {
        auto& meta = metatable.first[i];

        if (APP.game_mode() == App::GameMode::multiplayer or
            APP.game_mode() == App::GameMode::tutorial or
            APP.game_mode() == App::GameMode::co_op) {
            if (i >= plugin_rooms_begin()) {
                // We disable plugin (dlc) rooms during certain game modes.
                break;
            }
        }

        if (not is_enabled(i)) {
            continue;
        }

        const bool workshop_required =
            (meta->properties() & RoomProperties::workshop_required);

        const bool manufactory_required =
            (meta->properties() & RoomProperties::manufactory_required);

        const bool sandbox_dependencies_off =
            not SandboxLoaderModule::get_setting(3);

        if (APP.game_mode() not_eq App::GameMode::sandbox) {
            if ((meta->properties() & RoomProperties::human_only) and
                APP.faction() not_eq Faction::human) {
                continue;
            }

            if ((meta->properties() & RoomProperties::sylph_only) and
                APP.faction() not_eq Faction::sylph) {
                continue;
            }

            if ((meta->properties() & RoomProperties::goblin_only) and
                APP.faction() not_eq Faction::goblin) {
                continue;
            }
        }

        const bool dependencies_satisfied =
            (not manufactory_required or
             (manufactory_required and f_count > 0) or
             (APP.game_mode() == App::GameMode::sandbox and
              sandbox_dependencies_off)) and
            (not workshop_required or (workshop_required and w_count > 0) or
             (APP.game_mode() == App::GameMode::sandbox and
              sandbox_dependencies_off));

        const bool explicitly_disabled =
            (APP.game_mode() == App::GameMode::tutorial and
             meta->properties() & RoomProperties::disabled_in_tutorials) or
            (meta->properties() & RoomProperties::not_constructible) or
            (APP.game_mode() not_eq App::GameMode::tutorial and
             room_hidden(i)) or
            (APP.game_mode() not_eq App::GameMode::adventure and
             meta->properties() & RoomProperties::adventure_mode_only) or
            (APP.game_mode() not_eq App::GameMode::sandbox and
             meta->properties() &
                 RoomProperties::only_constructible_in_sandbox) or
            (PLATFORM.network_peer().is_connected() and
             meta->properties() & RoomProperties::multiplayer_unsupported) or
            (APP.game_mode() == App::GameMode::skyland_forever and
             meta->properties() &
                 RoomProperties::skyland_forever_unsupported) or
            (state_bit_load(StateBit::multiboot) and
             not(meta->properties() & RoomProperties::multiboot_compatible));

        if (not explicitly_disabled and dependencies_satisfied and
            (not constrain_ or (constrain_ and site_has_space(i))) and
            (APP.game_mode() not_eq App::GameMode::tutorial or
             // NOTE: for backwards compatibility with tutorials: The game used
             // to only display blocks that would fit into the selected
             // construction site. But players were confused during testing, so
             // I instead decided to always show all of the blocks in the
             // construction menu. The tutorials were recorded with the
             // old-styled menu, and I don't want to re-record them, so tutorial
             // mode still only displays the blocks that fit into the selected
             // construction site.
             (APP.game_mode() == App::GameMode::tutorial and
              site_has_space(i)))) {
            // Do not show decorations in the building list in tutorial mode.
            if (not(APP.game_mode() == App::GameMode::tutorial and
                    meta->category() == Room::Category::decoration)) {

                if (not data_->available_buildings_.push_back(i)) {
                    Platform::fatal("TODO: available buildings buffer "
                                    "needs more memory");
                }
            }
        }
    }

    if (building_selector_ >= (int)data_->available_buildings_.size()) {
        building_selector_ = data_->available_buildings_.size() - 1;
    }

    return true;
}



void ConstructionScene::enter(Scene& prev)
{
    if (not near_) {
        power_fraction_opponent_island_ = true;
    }

    PLATFORM.screen().fade(0.f);

    WorldScene::enter(prev);

    if (not near_) {
        far_camera();
    }

    persist_ui();

    find_construction_sites();

    if (not data_->construction_sites_.empty()) {
        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        // Pick a construction site in the same column as our selector
        for (u32 i = 0; i < data_->construction_sites_.size(); ++i) {
            if (data_->construction_sites_[i].x == cursor_loc.x) {
                selector_ = i;
            }
        }

        // In case the cursor happens to be hovering over an empty slot, select
        // it.
        for (u32 i = 0; i < data_->construction_sites_.size(); ++i) {
            if (data_->construction_sites_[i].x == cursor_loc.x and
                data_->construction_sites_[i].y == cursor_loc.y) {
                selector_ = i;
            }
        }


        APP.player().network_sync_cursor(cursor_loc, 3, true);
    }

    if (jump_to_selection_) {
        collect_available_buildings();
        state_ = State::choose_building;
        for (u32 i = 0; i < data_->available_buildings_.size(); ++i) {
            if (data_->available_buildings_[i] == *jump_to_selection_) {
                building_selector_ = i;
                break;
            }
        }
        show_current_building_text();
    } else {
        msg(SYSTR(construction_build)->c_str());
    }
}



void ConstructionScene::exit(Scene& next)
{
    WorldScene::exit(next);

    if (next.cast_construction_scene()) {
        // We do not want the menus to flicker between scenes when we switch
        // between two construction scenes, so disable cleanup for text. Only
        // really happens in sandbox mode, where you can build on either your
        // own castle or the opponent's. dynamic_cast<> because it's such an
        // obscure edge case and I don't have time to fix it correctly right
        // now.
        text_->__detach();
    } else {
        text_.reset();
        PLATFORM.fill_overlay(0);
    }
}



} // namespace skyland
