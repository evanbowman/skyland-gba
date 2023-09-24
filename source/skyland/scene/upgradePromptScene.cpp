#include "upgradePromptScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



UpgradePromptScene::UpgradePromptScene(const Vec2<u8>& coord,
                                       MetaclassIndex upgrade_from,
                                       MetaclassIndex upgrade_to)
    : upgrade_from_(upgrade_from), upgrade_to_(upgrade_to), target_coord_(coord)
{
}



Coins get_room_cost(Island* island, const RoomMeta& meta);



void UpgradePromptScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    pfrm.speaker().play_sound("openbag", 8);

    persist_ui();

    auto st = calc_screen_tiles(pfrm);

    const auto& from = load_metaclass(upgrade_from_);
    const auto& to = load_metaclass(upgrade_to_);

    StringBuffer<30> text(
        format(SYS_CSTR(upgrade_prompt), (*to)->ui_name(pfrm)->c_str())
            .c_str());

    text += stringify(get_room_cost(&app.player_island(), *to) -
                      get_room_cost(&app.player_island(), *from));

    text += "@";

    text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

    persist_ui();

    pfrm.set_tile(Layer::overlay, 0, st.y - 3, 245);
    pfrm.set_tile(Layer::overlay, 1, st.y - 3, 246);
    pfrm.set_tile(Layer::overlay, 0, st.y - 2, 247);
    pfrm.set_tile(Layer::overlay, 1, st.y - 2, 248);

    pfrm.set_tile(Layer::overlay, 2, st.y - 2, 418);
    pfrm.set_tile(Layer::overlay, 2, st.y - 3, 433);
    pfrm.set_tile(Layer::overlay, 0, st.y - 4, 425);
    pfrm.set_tile(Layer::overlay, 1, st.y - 4, 425);
}



void UpgradePromptScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    const auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        pfrm.set_tile(Layer::overlay, x, st.y - 1, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 2, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 4, 0);
    }
}



ScenePtr<Scene>
UpgradePromptScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    flicker_timer_ += delta;
    if (flicker_timer_ > milliseconds(300)) {
        flicker_timer_ -= milliseconds(300);
        flicker_on_ = not flicker_on_;
    }

    if (pfrm.network_peer().is_connected()) {
        // Upgrades unsupported in networked multiplayer.
        return scene_pool::alloc<ReadyScene>();
    }

    if (player(app).key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (player(app).key_down(pfrm, Key::action_1)) {
        if (auto room = app.player_island().get_room(target_coord_)) {

            auto next = scene_pool::make_deferred_scene<ReadyScene>();

            StringBuffer<80> err;

            auto notify_err = [&]() {
                pfrm.speaker().play_sound("beep_error", 3);
                return scene_pool::alloc<NotificationScene>(err, next);
            };

            if (room->metaclass_index() == upgrade_from_) {
                const auto& from = load_metaclass(upgrade_from_);
                const auto& to = load_metaclass(upgrade_to_);
                int size_diff_y = (*to)->size().y - (*from)->size().y;
                int size_diff_x = (*to)->size().x - (*from)->size().x;

                const auto props = (*to)->properties();

                if (props & RoomProperties::manufactory_required and
                    app.player_island().manufactory_count() == 0) {
                    err = SYS_CSTR(upgrade_denied_manufactory);
                    return notify_err();
                }

                if (props & RoomProperties::workshop_required and
                    app.player_island().workshop_count() == 0) {
                    err = SYS_CSTR(upgrade_denied_workshop);
                    return notify_err();
                }

                if (size_diff_x) {
                    auto sx = (target_coord_.x + (*to)->size().x) - size_diff_x;
                    auto ex = target_coord_.x + (*to)->size().x;
                    auto sy = target_coord_.y;
                    auto ey = target_coord_.y + (*to)->size().y;

                    if (ex > (int)app.player_island().terrain().size()) {
                        err = SYS_CSTR(construction_not_enough_space);
                        return notify_err();
                    }

                    for (u8 x = sx; x < ex; ++x) {
                        for (u8 y = sy; y < ey; ++y) {

                            if (app.player_island().get_room({x, y})) {
                                err = SYS_CSTR(construction_not_enough_space);
                                return notify_err();
                            }
                        }
                    }
                }

                if (size_diff_y) {
                    auto sx = target_coord_.x;
                    auto ex = target_coord_.x + (*to)->size().x;
                    auto sy = target_coord_.y - size_diff_y;

                    for (u8 x = sx; x < ex; ++x) {
                        for (u8 y = sy; y < target_coord_.y; ++y) {

                            if (app.player_island().get_room({x, y})) {
                                pfrm.speaker().play_sound("beep_error", 3);
                                err = SYS_CSTR(construction_not_enough_space);
                                return scene_pool::alloc<NotificationScene>(
                                    err, next);
                            }
                        }
                    }
                }

                auto cost = get_room_cost(&app.player_island(), *to) -
                            get_room_cost(&app.player_island(), *from);

                if (app.coins() < cost) {
                    pfrm.speaker().play_sound("beep_error", 3);
                    err = SYS_CSTR(construction_insufficient_funds);
                    return scene_pool::alloc<NotificationScene>(err, next);
                }

                room->__unsafe__transmute(pfrm, app, upgrade_to_);

                app.set_coins(pfrm, app.coins() - cost);
                app.level_coins_spent() += cost;
                pfrm.speaker().play_sound("build0", 4);
            }
        } else {
            pfrm.speaker().play_sound("beep_error", 3);
        }
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



void UpgradePromptScene::display(Platform& pfrm, App& app)
{
    ActiveWorldScene::display(pfrm, app);

    const auto& from = load_metaclass(upgrade_from_);
    const auto& to = load_metaclass(upgrade_to_);
    int size_diff_y = (*to)->size().y - (*from)->size().y;
    int size_diff_x = (*to)->size().x - (*from)->size().x;

    if (size_diff_x) {
        for (u8 x = (target_coord_.x + (*to)->size().x) - size_diff_x;
             x < target_coord_.x + (*to)->size().x;
             ++x) {
            for (u8 y = target_coord_.y; y < target_coord_.y + (*to)->size().y;
                 ++y) {
                Sprite spr;
                spr.set_tidx_16x16(13, 1);
                spr.set_size(Sprite::Size::w16_h16);
                auto origin = app.player_island().visual_origin();
                origin.x += Fixnum(x * 16);
                origin.y += Fixnum(y * 16);
                spr.set_position({origin.x, origin.y});

                if (flicker_on_) {
                    if (app.player_island().get_room({x, y}) or
                        x >= app.player_island().terrain().size()) {
                        spr.set_mix({custom_color(0xf7ce9e), 250});
                    }
                }

                pfrm.screen().draw(spr);
            }
        }
    }
    if (size_diff_y) {
        for (u8 x = target_coord_.x; x < target_coord_.x + (*to)->size().x;
             ++x) {
            for (u8 y = target_coord_.y - size_diff_y; y < target_coord_.y;
                 ++y) {
                Sprite spr;
                spr.set_tidx_16x16(13, 1);
                spr.set_size(Sprite::Size::w16_h16);
                auto origin = app.player_island().visual_origin();
                origin.x += Fixnum(x * 16);
                origin.y += Fixnum(y * 16);
                spr.set_position({origin.x, origin.y});

                if (flicker_on_) {
                    if (app.player_island().get_room({x, y})) {
                        spr.set_mix({custom_color(0xf7ce9e), 250});
                    }
                }

                pfrm.screen().draw(spr);
            }
        }
    }

    if (auto room = app.player_island().get_room(target_coord_)) {
        auto origin = app.player_island().visual_origin();
        origin.x += Fixnum::from_integer(target_coord_.x * 16);
        origin.y += Fixnum::from_integer(target_coord_.y * 16);

        origin.x -= 4.0_fixed;
        origin.y -= 4.0_fixed;

        Sprite spr;
        spr.set_size(Sprite::Size::w16_h16);
        spr.set_texture_index(52 * 2);
        spr.set_position(origin);
        pfrm.screen().draw(spr);

        spr.set_flip({true, false});
        origin.x += Fixnum::from_integer(room->size().x * 16);
        origin.x -= 8.0_fixed;
        spr.set_position(origin);
        pfrm.screen().draw(spr);

        spr.set_flip({true, true});
        origin.y += Fixnum::from_integer(room->size().y * 16);
        origin.y -= 8.0_fixed;
        spr.set_position(origin);
        pfrm.screen().draw(spr);

        spr.set_flip({false, true});
        origin.x -= Fixnum::from_integer(room->size().x * 16);
        origin.x += 8.0_fixed;
        spr.set_position(origin);
        pfrm.screen().draw(spr);
    }
}



} // namespace skyland
