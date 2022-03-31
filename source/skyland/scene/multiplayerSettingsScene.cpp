#include "multiplayerSettingsScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/character/basicCharacter.hpp"
#include "skyland/player/coopTeam.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



MultiplayerSettingsScene::ParamBuffer MultiplayerSettingsScene::vs_parameters_;



const MultiplayerSettingsScene::ParameterInfo
    MultiplayerSettingsScene::param_info[decltype(vs_parameters_)::capacity()] =
        {
            {SystemString::mt_game_mode, 1, 0, 1},
            {SystemString::mt_prep_seconds, 5, 20, 10000},
            {SystemString::mt_unhide_prep, 1, 0, 1},
            {SystemString::mt_coins, 100, 1000, 10000000},
            {SystemString::mt_terrain_size, 1, 3, 13},
};



void MultiplayerSettingsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    const auto title = SYSTR(mt_title);

    pfrm.load_overlay_texture("overlay_challenges");

    title_.emplace(
        pfrm,
        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(title->c_str())),
                     1});


    const auto msg = SYSTR(mt_hint);
    msg_.emplace(
        pfrm,
        msg->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(msg->c_str())),
                     18});


    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(pfrm, OverlayCoord{2, u8(4 + i * 2)});
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
    }


    for (u32 i = 0; i < vs_parameters_.capacity(); ++i) {
        update_parameter(pfrm, i);
    }

    pfrm.screen().fade(0.6f, ColorConstant::rich_black, {}, false, false);

    pfrm.system_call("v-parallax", (void*)false);
}



void MultiplayerSettingsScene::update_parameter(Platform& pfrm, u8 line_num)
{
    if (line_num >= vs_parameters_.capacity()) {
        return;
    }

    if (vs_parameters_[0] and line_num > 0) {
        return;
    }

    StringBuffer<48> temp;
    temp += loadstr(pfrm, param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;

    StringBuffer<32> field_name;
    if (line_num == 0) {
        if (vs_parameters_[line_num]) {
            field_name = SYSTR(mt_coop)->c_str();
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
                update_parameter(pfrm, i);
            }
        }
    }
}



void set_island_positions(Island& left_island, Island& right_island);



void MultiplayerSettingsScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    title_.reset();
    settings_text_.clear();

    pfrm.load_overlay_texture("overlay");

    pfrm.system_call("v-parallax", (void*)true);

    if (vs_parameters_[0]) {
        setup_coop_game(pfrm, app);
    } else {
        setup_vs_game(pfrm, app);
    }
}



void prep_level(Platform& pfrm, App& app);



void MultiplayerSettingsScene::setup_coop_game(Platform& pfrm, App& app)
{
    std::get<SkylandGlobalData>(globals()).unhide_multiplayer_prep_ = true;
    std::get<SkylandGlobalData>(globals()).coop_cursor_icon_ = 15;

    // NOTE: A co-op game is basically just SKYLAND Forever where both players
    // share control of a castle.
    SkylandForever::init(pfrm, app, 1);


    app.set_coins(pfrm, std::max(0, app.coins() - 1000));

    app.swap_player<CoopTeam>();

    app.game_mode() = App::GameMode::co_op;
}



void MultiplayerSettingsScene::setup_vs_game(Platform& pfrm, App& app)
{
    std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_ =
        vs_parameters_[1];

    std::get<SkylandGlobalData>(globals()).unhide_multiplayer_prep_ =
        vs_parameters_[2];

    app.set_coins(pfrm, vs_parameters_[3]);


    app.player_island().init_terrain(pfrm, vs_parameters_[4]);

    // Now that we know the size of the terrain for the multiplayer match, we
    // can create and position the two islands.
    app.create_opponent_island(pfrm, vs_parameters_[4]);

    app.opponent_island()->set_float_timer(
        std::numeric_limits<Microseconds>::max() / 2);

    set_island_positions(app.player_island(), *app.opponent_island());


    app.player_island().clear_rooms(pfrm, app);


    // Unless the island configured size is really tiny, leave a one-tile gap to
    // the left of the starting position of the power core.
    const u8 player_start_x = vs_parameters_[4] > 3 ? 1 : 0;
    app.player_island().add_room<Core>(pfrm, app, {player_start_x, 13}, true);

    auto add_player_chr = [&app](u8 x, u8 y) {
        app.player_island().add_character(alloc_entity<BasicCharacter>(
            &app.player_island(), &app.player(), Vec2<u8>{x, y}, false));
    };

    add_player_chr(player_start_x, 14);
    add_player_chr(player_start_x + 1, 14);


    const u8 opponent_start_x =
        vs_parameters_[4] > 3 ? vs_parameters_[4] - 3 : vs_parameters_[4] - 2;
    app.opponent_island()->add_room<Core>(
        pfrm, app, {opponent_start_x, 13}, true);

    auto add_opponent_chr = [&app](u8 x, u8 y) {
        app.opponent_island()->add_character(alloc_entity<BasicCharacter>(
            app.opponent_island(), &app.opponent(), Vec2<u8>{x, y}, false));
    };

    add_opponent_chr(opponent_start_x, 14);
    add_opponent_chr(opponent_start_x + 1, 14);

    app.player_island().repaint(pfrm, app);
    app.opponent_island()->repaint(pfrm, app);
}



void MultiplayerSettingsScene::display(Platform& pfrm, App& app)
{
    Scene::display(pfrm, app);

    if (state_ not_eq State::edit_settings) {
        return;
    }

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position({2, 31.f + player_cursor_ * 16});

    pfrm.screen().draw(spr);
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchParameterUpdate& packet)
{
    vs_parameters_[packet.parameter_id_] = packet.value_.get();
    update_parameter(pfrm, packet.parameter_id_);
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchSettingsCursor& packet)
{
    opponent_cursor_ = packet.cursor_line_;
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchReady& packet)
{
    opponent_ready_ = true;
}



void MultiplayerSettingsScene::sync_parameters(Platform& pfrm)
{
    for (u32 i = 0; i < vs_parameters_.size(); ++i) {
        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = i;
        p.value_.set(vs_parameters_[i]);
        network::transmit(pfrm, p);
    }
}



ScenePtr<Scene>
MultiplayerSettingsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    }

    if (app.player().key_pressed(pfrm, Key::up)) {
        key_held_timers_[0] += delta;
    } else {
        key_held_timers_[0] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::down)) {
        key_held_timers_[1] += delta;
    } else {
        key_held_timers_[1] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::left)) {
        key_held_timers_[2] += delta;
    } else {
        key_held_timers_[2] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::right)) {
        key_held_timers_[3] += delta;
    } else {
        key_held_timers_[3] = 0;
    }

    if (state_ == State::ready) {
        if (opponent_ready_) {
            return scene_pool::alloc<FadeInScene>();
        }
        return null_scene();
    }

    if (app.player().key_down(pfrm, Key::start)) {
        if (not pfrm.network_peer().is_host() and parameter_sync_timer_ > 0) {
            return null_scene();
        }
        state_ = State::ready;

        sync_parameters(pfrm);

        network::packet::GameMatchReady r;
        network::transmit(pfrm, r);

        const auto msg = SYSTR(mt_waiting);
        msg_.emplace(
            pfrm,
            msg->c_str(),
            OverlayCoord{
                (u8)centered_text_margins(pfrm, utf8::len(msg->c_str())), 18});


        return null_scene();
    }

    if (pfrm.network_peer().is_host()) {
        parameter_sync_timer_ -= delta;
        if (parameter_sync_timer_ < 0) {
            parameter_sync_timer_ = seconds(2);

            // Every so often, the host device broadcasts its own parameters, in
            // case anything got out of sync.
            sync_parameters(pfrm);
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


    if (app.player().key_down(pfrm, Key::right) or
        key_held_timers_[3] > milliseconds(500)) {
        if (vs_parameters_[player_cursor_] <
            param_info[player_cursor_].upper_limit_) {
            vs_parameters_[player_cursor_] +=
                param_info[player_cursor_].increment_;
        } else if (is_boolean_field) {
            vs_parameters_[player_cursor_] = not vs_parameters_[player_cursor_];
        }
        update_parameter(pfrm, player_cursor_);
        key_held_timers_[3] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(vs_parameters_[player_cursor_]);
        network::transmit(pfrm, p);

    } else if (app.player().key_down(pfrm, Key::left) or
               key_held_timers_[2] > milliseconds(500)) {
        vs_parameters_[player_cursor_] -= param_info[player_cursor_].increment_;
        if (vs_parameters_[player_cursor_] <
            param_info[player_cursor_].lower_limit_) {
            vs_parameters_[player_cursor_] =
                param_info[player_cursor_].lower_limit_;
        } else if (is_boolean_field) {
            vs_parameters_[player_cursor_] = not vs_parameters_[player_cursor_];
        }
        update_parameter(pfrm, player_cursor_);
        key_held_timers_[2] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(vs_parameters_[player_cursor_]);
        network::transmit(pfrm, p);

    } else if (app.player().key_down(pfrm, Key::down) and
               player_cursor_ < settings_text_.size() - 1 and
               vs_parameters_[0] == 0) {
        ++player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(pfrm, c);

    } else if (app.player().key_down(pfrm, Key::up) and player_cursor_ > 0) {
        --player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(pfrm, c);
    }

    return null_scene();
}



} // namespace skyland
