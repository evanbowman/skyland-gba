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
                                       const Room::UpgradeList& upgrade_to)
    : upgrade_from_(upgrade_from), upgrade_to_(upgrade_to), target_coord_(coord)
{
}



Coins get_room_cost(Island* island, const RoomMeta& meta);



void UpgradePromptScene::repaint()
{
    auto st = calc_screen_tiles();

    const auto& from = load_metaclass(upgrade_from_);
    const auto& to = load_metaclass(upgrade_to_[upgrade_index_]);

    StringBuffer<30> text(
        format(SYS_CSTR(upgrade_prompt), (*to)->ui_name()->c_str()).c_str());

    text += stringify(get_room_cost(&APP.player_island(), *to) -
                      get_room_cost(&APP.player_island(), *from));

    text += "@";

    text_.emplace(text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int x = 0; x < st.x; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 425);
    }

    int x_margin = (st.x - (4 * upgrade_to_.size())) / 2 - 2;

    static const int vram_locs[] = {258, 181, 197, 213, 274};
    for (u32 i = 0; i < upgrade_to_.size(); ++i) {
        int x_start = x_margin + i * 4;
        draw_image(vram_locs[i], x_start, st.y - 5, 4, 4, Layer::overlay);
        auto mt = load_metaclass(upgrade_to_[i]);
        auto icon = i == upgrade_index_ ? (*mt)->icon() : (*mt)->unsel_icon();
        PLATFORM.load_overlay_chunk(vram_locs[i], icon, 16);
        for (int x = 0; x < 4; ++x) {
            PLATFORM.set_tile(Layer::overlay, x_start + x, st.y - 6, 425);
        }
    }

    PLATFORM.set_tile(Layer::overlay, 0, st.y - 3, 245);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 3, 246);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - 2, 247);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 2, 248);
    PLATFORM.set_tile(Layer::overlay, 2, st.y - 2, 418);
    PLATFORM.set_tile(Layer::overlay, 2, st.y - 3, 433);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - 4, 425);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 4, 425);

    PLATFORM.set_tile(Layer::overlay, x_margin - 1, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, x_margin - 1, st.y - 3, 130);
    PLATFORM.set_tile(Layer::overlay, x_margin - 1, st.y - 4, 130);
    PLATFORM.set_tile(Layer::overlay, x_margin - 1, st.y - 5, 130);
    auto sel_end = x_margin + (upgrade_to_.size() * 4);
    PLATFORM.set_tile(Layer::overlay, sel_end, st.y - 2, 418);
    PLATFORM.set_tile(Layer::overlay, sel_end, st.y - 3, 433);
    PLATFORM.set_tile(Layer::overlay, sel_end, st.y - 4, 433);
    PLATFORM.set_tile(Layer::overlay, sel_end, st.y - 5, 433);

    yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

}



void UpgradePromptScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    PLATFORM.speaker().play_sound("openbag", 8);

    persist_ui();

    repaint();

    persist_ui();
}



void UpgradePromptScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    text_.reset();

    PLATFORM.fill_overlay(0);
}



ScenePtr UpgradePromptScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(150));
    };


    flicker_timer_ += delta;
    if (flicker_timer_ > milliseconds(300)) {
        flicker_timer_ -= milliseconds(300);
        flicker_on_ = not flicker_on_;
    }

    if (PLATFORM.network_peer().is_connected()) {
        // Upgrades unsupported in networked multiplayer.
        return make_scene<ReadyScene>();
    }

    if (player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    if (upgrade_to_.size() > 1) {
        if (test_key(Key::right)) {
            ++upgrade_index_;
            upgrade_index_ %= upgrade_to_.size();
            repaint();
        }
        if (test_key(Key::left)) {
            if (upgrade_index_ == 0) {
                upgrade_index_ = upgrade_to_.size() - 1;
            } else {
                --upgrade_index_;
            }
            repaint();
        }
    }

    if (player().key_down(Key::action_1)) {
        if (auto room = APP.player_island().get_room(target_coord_)) {

            auto next = make_deferred_scene<ReadyScene>();

            StringBuffer<80> err;

            auto notify_err = [&]() {
                PLATFORM.speaker().play_sound("beep_error", 3);
                return make_scene<NotificationScene>(err, next);
            };

            if (room->metaclass_index() == upgrade_from_) {
                const auto& from = load_metaclass(upgrade_from_);
                const auto& to = load_metaclass(upgrade_to_[upgrade_index_]);
                auto to_sz = (*to)->constructed_size();
                auto from_sz = (*from)->constructed_size();
                int size_diff_y = to_sz.y - from_sz.y;
                int size_diff_x = to_sz.x - from_sz.x;

                const auto props = (*to)->properties();

                if (props & RoomProperties::manufactory_required and
                    APP.player_island().manufactory_count() == 0) {
                    err = SYS_CSTR(upgrade_denied_manufactory);
                    return notify_err();
                }

                if (props & RoomProperties::workshop_required and
                    APP.player_island().workshop_count() == 0 and
                    APP.player_island().manufactory_count() == 0) {
                    err = SYS_CSTR(upgrade_denied_workshop);
                    return notify_err();
                }

                if (size_diff_x) {
                    auto sx = (target_coord_.x + to_sz.x) - size_diff_x;
                    auto ex = target_coord_.x + to_sz.x;
                    auto sy = target_coord_.y;
                    auto ey = target_coord_.y + to_sz.y;

                    if (ex > (int)APP.player_island().terrain().size()) {
                        err = SYS_CSTR(construction_not_enough_space);
                        return notify_err();
                    }

                    for (u8 x = sx; x < ex; ++x) {
                        for (u8 y = sy; y < ey; ++y) {

                            if (APP.player_island().get_room({x, y})) {
                                err = SYS_CSTR(construction_not_enough_space);
                                return notify_err();
                            }
                        }
                    }
                }

                if (size_diff_y) {
                    auto sx = target_coord_.x;
                    auto ex = target_coord_.x + to_sz.x;
                    auto sy = target_coord_.y - size_diff_y;

                    for (u8 x = sx; x < ex; ++x) {
                        for (u8 y = sy; y < target_coord_.y; ++y) {

                            if (APP.player_island().get_room({x, y})) {
                                PLATFORM.speaker().play_sound("beep_error", 3);
                                err = SYS_CSTR(construction_not_enough_space);
                                return make_scene<NotificationScene>(err, next);
                            }
                        }
                    }
                }

                auto cost = get_room_cost(&APP.player_island(), *to) -
                            get_room_cost(&APP.player_island(), *from);

                if (APP.coins() < cost) {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    err = SYS_CSTR(construction_insufficient_funds);
                    return make_scene<NotificationScene>(err, next);
                }

                room->__unsafe__transmute(upgrade_to_[upgrade_index_]);
                room->parent()->rooms().reindex(true);

                APP.set_coins(APP.coins() - cost);
                APP.level_coins_spent() += cost;
                PLATFORM.speaker().play_sound("build0", 4);
            }
        } else {
            PLATFORM.speaker().play_sound("beep_error", 3);
        }
        return make_scene<ReadyScene>();
    }

    return null_scene();
}



void UpgradePromptScene::display()
{
    ActiveWorldScene::display();

    const auto& from = load_metaclass(upgrade_from_);
    const auto& to = load_metaclass(upgrade_to_[upgrade_index_]);
    auto to_sz = (*to)->constructed_size();
    auto from_sz = (*from)->constructed_size();
    int size_diff_y = to_sz.y - from_sz.y;
    int size_diff_x = to_sz.x - from_sz.x;

    if (size_diff_x) {
        for (u8 x = (target_coord_.x + to_sz.x) - size_diff_x;
             x < target_coord_.x + to_sz.x;
             ++x) {
            for (u8 y = target_coord_.y; y < target_coord_.y + to_sz.y; ++y) {
                Sprite spr;
                spr.set_tidx_16x16(13, 1);
                spr.set_size(Sprite::Size::w16_h16);
                auto origin = APP.player_island().visual_origin();
                origin.x += Fixnum(x * 16);
                origin.y += Fixnum(y * 16);
                spr.set_position({origin.x, origin.y});

                if (flicker_on_) {
                    if (APP.player_island().get_room({x, y}) or
                        x >= APP.player_island().terrain().size()) {
                        spr.set_mix({custom_color(0xf7ce9e), 250});
                    }
                }

                PLATFORM.screen().draw(spr);
            }
        }
    }
    if (size_diff_y) {
        for (u8 x = target_coord_.x; x < target_coord_.x + to_sz.x; ++x) {
            for (u8 y = target_coord_.y - size_diff_y; y < target_coord_.y;
                 ++y) {
                Sprite spr;
                spr.set_tidx_16x16(13, 1);
                spr.set_size(Sprite::Size::w16_h16);
                auto origin = APP.player_island().visual_origin();
                origin.x += Fixnum(x * 16);
                origin.y += Fixnum(y * 16);
                spr.set_position({origin.x, origin.y});

                if (flicker_on_) {
                    if (APP.player_island().get_room({x, y})) {
                        spr.set_mix({custom_color(0xf7ce9e), 250});
                    }
                }

                PLATFORM.screen().draw(spr);
            }
        }
    }

    if (auto room = APP.player_island().get_room(target_coord_)) {
        auto origin = APP.player_island().visual_origin();
        origin.x += Fixnum::from_integer(target_coord_.x * 16);
        origin.y += Fixnum::from_integer(target_coord_.y * 16);

        origin.x -= 4.0_fixed;
        origin.y -= 4.0_fixed;

        Sprite spr;
        spr.set_size(Sprite::Size::w16_h16);
        spr.set_texture_index(52 * 2);
        spr.set_position(origin);
        PLATFORM.screen().draw(spr);

        spr.set_flip({true, false});
        origin.x += Fixnum::from_integer(room->size().x * 16);
        origin.x -= 8.0_fixed;
        spr.set_position(origin);
        PLATFORM.screen().draw(spr);

        spr.set_flip({true, true});
        origin.y += Fixnum::from_integer(room->size().y * 16);
        origin.y -= 8.0_fixed;
        spr.set_position(origin);
        PLATFORM.screen().draw(spr);

        spr.set_flip({false, true});
        origin.x -= Fixnum::from_integer(room->size().x * 16);
        origin.x += 8.0_fixed;
        spr.set_position(origin);
        PLATFORM.screen().draw(spr);
    }
}



} // namespace skyland
