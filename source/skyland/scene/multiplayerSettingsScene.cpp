////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "multiplayerSettingsScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/character/character.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const u8 special_parameter_slot_co_op_rng = 99;



MultiplayerSettingsScene::ParamBuffer MultiplayerSettingsScene::vs_parameters_;



const MultiplayerSettingsScene::ParameterInfo
    MultiplayerSettingsScene::param_info[decltype(vs_parameters_)::capacity()] =
        {{SystemString::mt_game_mode, 1, 0, 1},
         {SystemString::mt_prep_seconds, 5, 20, 10000},
         {SystemString::mt_unhide_prep, 1, 0, 1},
         {SystemString::mt_coins, 100, 1000, 10000000},
         {SystemString::mt_terrain_size, 1, 3, 13},
         {SystemString::mt_timeout_freq, 30, 0, 1000},
         {SystemString::mt_timeout_duration, 5, 10, 120}};



void MultiplayerSettingsScene::enter(Scene& prev)
{
    if (PLATFORM.network_peer().is_host()) {

        // Feed the utility rng into a variable, sync it with the client
        // console.
        co_op_rng_ = rng::utility_state;

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = special_parameter_slot_co_op_rng;
        p.value_.set(*co_op_rng_);
        network::transmit(p);
    }

    PLATFORM.screen().set_view({});

    PLATFORM.screen().set_shader(passthrough_shader);

    const auto title = SYSTR(mt_title);

    PLATFORM.load_overlay_texture("overlay_challenges");

    title_.emplace(

        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(utf8::len(title->c_str())), 1});


    const auto msg = SYSTR(mt_hint);
    msg_.emplace(

        msg->c_str(),
        OverlayCoord{(u8)centered_text_margins(utf8::len(msg->c_str())), 18});


    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(OverlayCoord{2, u8(4 + i * 2)});
    }

    if (not vs_parameters_.full()) {
        for (u32 i = 0; i < vs_parameters_.capacity(); ++i) {
            vs_parameters_.push_back(0);
        }

        // Defaults
        vs_parameters_[0] = 0;
        vs_parameters_[1] = 120;
        vs_parameters_[2] = 0;
        vs_parameters_[3] = 17500;
        vs_parameters_[4] = 8;
        vs_parameters_[5] = 60;
        vs_parameters_[6] = 30;
    }


    for (u32 i = 0; i < vs_parameters_.capacity(); ++i) {
        update_parameter(i);
    }

    PLATFORM.screen().fade(0.6f, ColorConstant::rich_black, {}, false, false);

    PLATFORM_EXTENSION(vertical_parallax_enable, false);
}



void MultiplayerSettingsScene::update_parameter(u8 line_num)
{
    if (line_num >= vs_parameters_.capacity()) {
        return;
    }

    if (vs_parameters_[0] and line_num > 0) {
        return;
    }

    StringBuffer<48> temp;
    temp += loadstr(param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;

    StringBuffer<32> field_name;
    if (line_num == 0) {
        if (vs_parameters_[line_num]) {
            field_name = SYSTR(mt_co_op)->c_str();
        } else {
            field_name = SYSTR(mt_vs)->c_str();
        }
    } else {
        if (vs_parameters_[line_num]) {
            field_name = SYSTR(yes)->c_str();
        } else {
            field_name = SYSTR(no)->c_str();
        }
    }


    auto int_text_len = integer_text_length(vs_parameters_[line_num]);
    if (is_boolean_field) {
        int_text_len = utf8::len(field_name.c_str());
    }

    for (u32 i = temp.length(); i < 28 - int_text_len - 2; ++i) {
        if (i % 2 == 0) {
            temp.push_back('.');
        } else {
            temp.push_back(' ');
        }
    }

    if (is_boolean_field) {
        temp += field_name;
    } else {
        temp += stringify(vs_parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());

    if (line_num == 0) {
        if (vs_parameters_[line_num]) {
            for (u32 i = 1; i < settings_text_.size(); ++i) {
                settings_text_[i].erase();
            }
            player_cursor_ = 0;
        } else {
            for (u32 i = 1; i < settings_text_.size(); ++i) {
                update_parameter(i);
            }
        }
    }
}



void set_island_positions(Island& left_island, Island& right_island);



void MultiplayerSettingsScene::exit(Scene& next)
{
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    title_.reset();
    settings_text_.clear();

    PLATFORM.load_overlay_texture("overlay");

    PLATFORM_EXTENSION(vertical_parallax_enable, true);

    if (vs_parameters_[0]) {
        setup_co_op_game();
    } else {
        setup_vs_game();
    }
}



void prep_level();



void MultiplayerSettingsScene::setup_co_op_game()
{
    if (co_op_rng_) {
        rng::critical_state = *co_op_rng_;
    } else {
        Platform::fatal("Session error code 9001");
    }

    Character::__reset_ids();

    globals().unhide_multiplayer_prep_ = true;
    globals().co_op_cursor_icon_ = 15;

    // NOTE: A co-op game is basically just SKYLAND Forever where both players
    // share control of a castle.
    SkylandForever::init(1, *co_op_rng_);

    APP.persistent_data().score_.set(0);

    APP.set_coins(std::max(0, APP.coins() - 1000));

    APP.swap_player<CoOpTeam>();

    APP.game_mode() = App::GameMode::co_op;
}



Time MultiplayerSettingsScene::timeout_frequency()
{
    return seconds(vs_parameters_[5]);
}



Time MultiplayerSettingsScene::timeout_duration()
{
    return seconds(vs_parameters_[6]);
}



void MultiplayerSettingsScene::setup_vs_game()
{
    const bool is_host = PLATFORM.network_peer().is_host();
    const auto non_host_start_id =
        Character::multiplayer_vs_client_chr_start_id;

    if (is_host) {
        Character::__reset_ids();
    } else {
        const auto start = non_host_start_id;
        Character::__reset_ids(start);
    }


    globals().multiplayer_timeout_remaining_ = seconds(vs_parameters_[6]);
    globals().multiplayer_timeout_countdown_ = seconds(vs_parameters_[5]);

    globals().multiplayer_prep_seconds_ = vs_parameters_[1];

    globals().unhide_multiplayer_prep_ = vs_parameters_[2];

    APP.set_coins(vs_parameters_[3]);


    APP.player_island().init_terrain(vs_parameters_[4]);

    // Now that we know the size of the terrain for the multiplayer match, we
    // can create and position the two islands.
    APP.create_opponent_island(vs_parameters_[4]);

    APP.opponent_island()->set_float_timer(std::numeric_limits<Time>::max() /
                                           2);

    set_island_positions(APP.player_island(), *APP.opponent_island());


    APP.player_island().clear_rooms();


    // Unless the island configured size is really tiny, leave a one-tile gap to
    // the left of the starting position of the power core.
    const u8 player_start_x = vs_parameters_[4] > 3 ? 1 : 0;
    APP.player_island().add_room<Core>({player_start_x, 13}, true);

    auto add_player_chr = [](u8 x, u8 y, int id) {
        auto chr = alloc_entity<Character>(
            &APP.player_island(), &APP.player(), RoomCoord{x, y}, false);

        chr->__assign_id(id);

        APP.player_island().add_character(std::move(chr));
    };

    add_player_chr(player_start_x, 14, is_host ? 1 : non_host_start_id + 1);
    add_player_chr(player_start_x + 1, 14, is_host ? 2 : non_host_start_id);


    const u8 opponent_start_x =
        vs_parameters_[4] > 3 ? vs_parameters_[4] - 3 : vs_parameters_[4] - 2;
    APP.opponent_island()->add_room<Core>({opponent_start_x, 13}, true);

    auto add_opponent_chr = [](u8 x, u8 y, int id) {
        auto chr = alloc_entity<Character>(
            APP.opponent_island(), &APP.opponent(), RoomCoord{x, y}, false);
        chr->__assign_id(id);
        APP.opponent_island()->add_character(std::move(chr));
    };

    add_opponent_chr(opponent_start_x, 14, is_host ? non_host_start_id : 2);

    add_opponent_chr(
        opponent_start_x + 1, 14, is_host ? non_host_start_id + 1 : 1);

    APP.player_island().repaint();
    APP.opponent_island()->repaint();
}



void MultiplayerSettingsScene::display()
{
    Scene::display();

    if (state_ not_eq State::edit_settings) {
        return;
    }

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position(
        {2.0_fixed, 31.0_fixed + Fixnum::from_integer(player_cursor_ * 16)});

    PLATFORM.screen().draw(spr);
}



void MultiplayerSettingsScene::receive(


    const network::packet::GameMatchParameterUpdate& packet)
{
    if (packet.parameter_id_ == special_parameter_slot_co_op_rng) {
        co_op_rng_ = packet.value_.get();
        return;
    }

    vs_parameters_[packet.parameter_id_] = packet.value_.get();
    update_parameter(packet.parameter_id_);
}



void MultiplayerSettingsScene::receive(


    const network::packet::GameMatchSettingsCursor& packet)
{
    opponent_cursor_ = packet.cursor_line_;
}



void MultiplayerSettingsScene::receive(


    const network::packet::GameMatchReady& packet)
{
    opponent_ready_ = true;
}



void MultiplayerSettingsScene::sync_parameters()
{
    for (u32 i = 0; i < vs_parameters_.size(); ++i) {
        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = i;
        p.value_.set(vs_parameters_[i]);
        network::transmit(p);
    }
}



ScenePtr MultiplayerSettingsScene::update(Time delta)
{
    APP.update_parallax(delta);

    if (PLATFORM.network_peer().is_connected()) {
        network::poll_messages(*this);
    }

    if (APP.player().key_pressed(Key::up)) {
        key_held_timers_[0] += delta;
    } else {
        key_held_timers_[0] = 0;
    }

    if (APP.player().key_pressed(Key::down)) {
        key_held_timers_[1] += delta;
    } else {
        key_held_timers_[1] = 0;
    }

    if (APP.player().key_pressed(Key::left)) {
        key_held_timers_[2] += delta;
    } else {
        key_held_timers_[2] = 0;
    }

    if (APP.player().key_pressed(Key::right)) {
        key_held_timers_[3] += delta;
    } else {
        key_held_timers_[3] = 0;
    }

    if (state_ == State::ready) {
        if (opponent_ready_) {
            return make_scene<FadeInScene>();
        }
        return null_scene();
    }

    if (APP.player().key_down(Key::start)) {
        if (not PLATFORM.network_peer().is_host() and
            parameter_sync_timer_ > 0) {
            return null_scene();
        }
        state_ = State::ready;

        sync_parameters();

        network::packet::GameMatchReady r;
        network::transmit(r);

        const auto msg = SYSTR(mt_waiting);
        msg_.emplace(

            msg->c_str(),
            OverlayCoord{(u8)centered_text_margins(utf8::len(msg->c_str())),
                         18});


        return null_scene();
    }

    if (PLATFORM.network_peer().is_host()) {
        parameter_sync_timer_ -= delta;
        if (parameter_sync_timer_ < 0) {
            parameter_sync_timer_ = seconds(2);

            // Every so often, the host device broadcasts its own parameters, in
            // case anything got out of sync.
            sync_parameters();
        }
    } else {
        if (parameter_sync_timer_ > 0) {
            // We want to get at least one sync from the host device, add a
            // smaller delta.
            parameter_sync_timer_ -= delta / 2;
        }
    }


    const bool is_boolean_field =
        param_info[player_cursor_].lower_limit_ == 0 and
        param_info[player_cursor_].upper_limit_ == 1;


    if (APP.player().key_down(Key::right) or
        key_held_timers_[3] > milliseconds(500)) {
        if (vs_parameters_[player_cursor_] <
            param_info[player_cursor_].upper_limit_) {
            vs_parameters_[player_cursor_] +=
                param_info[player_cursor_].increment_;
        } else if (is_boolean_field) {
            vs_parameters_[player_cursor_] = not vs_parameters_[player_cursor_];
        }
        update_parameter(player_cursor_);
        key_held_timers_[3] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(vs_parameters_[player_cursor_]);
        network::transmit(p);

    } else if (APP.player().key_down(Key::left) or
               key_held_timers_[2] > milliseconds(500)) {
        vs_parameters_[player_cursor_] -= param_info[player_cursor_].increment_;
        if (vs_parameters_[player_cursor_] <
            param_info[player_cursor_].lower_limit_) {
            vs_parameters_[player_cursor_] =
                param_info[player_cursor_].lower_limit_;
        } else if (is_boolean_field) {
            vs_parameters_[player_cursor_] = not vs_parameters_[player_cursor_];
        }
        update_parameter(player_cursor_);
        key_held_timers_[2] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(vs_parameters_[player_cursor_]);
        network::transmit(p);

    } else if (APP.player().key_down(Key::down) and
               player_cursor_ < settings_text_.size() - 1 and
               vs_parameters_[0] == 0) {
        ++player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(c);

    } else if (APP.player().key_down(Key::up) and player_cursor_ > 0) {
        --player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(c);
    }

    return null_scene();
}



} // namespace skyland
