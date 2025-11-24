#include "upgradePromptScene.hpp"
#include "constructionScene.hpp"
#include "modules/glossaryViewerModule.hpp"
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

    StringBuffer<64> text(
        format(SYS_CSTR(upgrade_prompt), (*to)->ui_name()->c_str()).c_str());

    if (utf8::len(text.c_str()) >= 27 and text[text.length() - 1] == ' ') {
        // The text + the cost likely overflows the screen width. Pop off a
        // space character to squeeze everything in.
        text.pop_back();
    }

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

    persist_ui();

    repaint();

    persist_ui();
}



void UpgradePromptScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    PLATFORM.fill_overlay(0);
}



bool has_gap_left(Room& room)
{
    if (room.position().x == 0) {
        return false;
    }
    bool gap_left = true;
    auto target_coord = room.position();
    for (int y = 0; y < room.size().y; ++y) {
        if (room.parent()->get_room(
                {u8(target_coord.x - 1), u8(target_coord.y + y)})) {
            gap_left = false;
        }
    }
    return gap_left;
}



int seek_max_displaced_x(Island& island,
                         const RoomCoord& current_coord,
                         const Vec2<u8>& current_room_size)
{
    Optional<u8> max_x;

    for (int y = 0; y < current_room_size.y; ++y) {
        if (auto room =
                island.get_room({u8(current_coord.x + current_room_size.x),
                                 u8(current_coord.y + y)})) {
            auto max =
                seek_max_displaced_x(island, room->position(), room->size());
            if (max_x and max > *max_x) {
                max_x = max;
            } else if (not max_x) {
                max_x = max;
            }
        }
    }

    if (max_x) {
        return *max_x;
    }

    return current_coord.x + (current_room_size.x - 1);
}



int seek_min_displaced_y(Island& island,
                         const RoomCoord& current_coord,
                         const Vec2<u8>& current_room_size)
{
    Optional<u8> min_y;

    for (int x = 0; x < current_room_size.x; ++x) {
        if (auto room = island.get_room(
                {u8(current_coord.x + x), u8(current_coord.y - 1)})) {
            auto min =
                seek_min_displaced_y(island, room->position(), room->size());
            if (min_y and min < *min_y) {
                min_y = min;
            } else if (not min_y) {
                min_y = min;
            }
        }
    }

    if (min_y) {
        return *min_y;
    }

    return current_coord.y;
}



bool can_displace_upwards(Island& island, const RoomCoord& locus, int amount)
{
    if (auto room = island.get_room(locus)) {
        return seek_min_displaced_y(island, room->position(), room->size()) -
                   amount >=
               construction_zone_min_y;
    }
    return false;
}



bool can_displace_rightwards(Island& island, const RoomCoord& locus, int amount)
{
    if (auto room = island.get_room(locus)) {
        return seek_max_displaced_x(island, room->position(), room->size()) +
                   amount <
               (int)island.terrain().size();
    }
    return false;
}



void displace_rooms_upwards(Island& island,
                            bool move,
                            const RoomCoord& current_coord,
                            const Vec2<u8>& current_room_size,
                            int displacement_amount = 1)
{
    for (int i = 0; i < displacement_amount; ++i) {
        for (int x = 0; x < current_room_size.x; ++x) {
            if (auto room = island.get_room(
                    {u8(current_coord.x + x), u8(current_coord.y - 1 - i)})) {
                displace_rooms_upwards(
                    island, true, room->position(), room->size(), 1);
            }
        }
        if (move) {
            island.move_room(current_coord,
                             {current_coord.x, u8(current_coord.y - 1)});
        }
    }
}



void displace_rooms_rightwards(Island& island,
                               bool move,
                               const RoomCoord& current_coord,
                               const Vec2<u8> current_room_size,
                               int displacement_amount = 1)
{
    for (int i = 0; i < displacement_amount; ++i) {
        for (int y = 0; y < current_room_size.y; ++y) {
            if (auto room = island.get_room(
                    {u8(current_coord.x + current_room_size.x + i),
                     u8(current_coord.y + y)})) {
                displace_rooms_rightwards(
                    island, true, room->position(), room->size(), 1);
            }
        }
    }
    if (move) {
        island.move_room(current_coord,
                         {u8(current_coord.x + 1), current_coord.y});
    }
}



class UpgradeDisplaceScene : public ActiveWorldScene
{
public:
    UpgradeDisplaceScene(MetaclassIndex upgrade_from,
                         MetaclassIndex upgrade_to,
                         const RoomCoord& coord,
                         int size_diff,
                         Coins cost)
        : upgrade_from_(upgrade_from), upgrade_to_(upgrade_to),
          target_coord_(coord), size_diff_(size_diff), cost_(cost)
    {
    }


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);
        repaint_menu_opts();

        StringBuffer<84> text = SYS_CSTR(construction_not_enough_space);
        int fill = 30 - utf8::len(text.c_str());
        for (int i = 0; i < fill; ++i) {
            text.push_back(' ');
        }
        Text::print(text.c_str(), {0, 17});
        for (int i = 0; i < 30; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 16, 425);
        }
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);
        PLATFORM.fill_overlay(0);
    }


    void repaint_menu_opts()
    {
        static const auto highlight_colors =
            FontColors{custom_color(0x000010), ColorConstant::aerospace_orange};

        auto println = [&](const char* txt, u8 y, bool highlight) {
            OptColors clr;
            if (highlight) {
                clr = highlight_colors;
                PLATFORM.set_tile(Layer::overlay, 0, y, 475);
            } else {
                PLATFORM.set_tile(Layer::overlay, 0, y, 112);
            }
            StringBuffer<84> line;
            line = txt;
            while (utf8::len(line.c_str()) < 29) {
                line.push_back(' ');
            }
            Text::print(line.c_str(), {1, y}, clr);
        };

        println(SYS_CSTR(upgrade_displace_cancel), 18, menu_opt_ == 0);
        println(SYS_CSTR(upgrade_displace_affirm), 19, menu_opt_ == 1);
    }


    ScenePtr update(Time delta) override
    {
        if (auto scene = ActiveWorldScene::update(delta)) {
            return scene;
        }

        flicker_timer_ += delta;
        if (flicker_timer_ > milliseconds(300)) {
            flicker_timer_ -= milliseconds(300);
            flicker_on_ = not flicker_on_;
        }

        if (APP.player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
        }

        if (APP.player().key_down(Key::down) or
            APP.player().key_down(Key::up)) {
            menu_opt_ = not menu_opt_;
            repaint_menu_opts();
        }

        if (APP.player().key_down(Key::action_1)) {
            if (menu_opt_ == 0) {
                // cancel
                return make_scene<ReadyScene>();
            }
            if (auto room = player_island().get_room(target_coord_)) {
                const auto& from = load_metaclass(upgrade_from_);
                const auto& to = load_metaclass(upgrade_to_);
                auto to_sz = (*to)->constructed_size();
                auto from_sz = (*from)->constructed_size();
                int size_diff_y = to_sz.y - from_sz.y;

                if (size_diff_y == size_diff_) {
                    displace_rooms_upwards(player_island(),
                                           false,
                                           target_coord_,
                                           room->size(),
                                           size_diff_);
                } else {
                    if (has_gap_left(*room)) {
                        player_island().move_room(
                            target_coord_,
                            {u8(target_coord_.x - 1), target_coord_.y});
                    } else {
                        displace_rooms_rightwards(player_island(),
                                                  false,
                                                  target_coord_,
                                                  room->size(),
                                                  size_diff_);
                    }
                }

                room->__unsafe__transmute(upgrade_to_);
                room->parent()->rooms().reindex(true);
                room->parent()->recalculate_power_usage();

                APP.set_coins(APP.coins() - cost_);
                APP.level_coins_spent() += cost_;
                PLATFORM.speaker().play_sound("build0", 4);
            }

            return make_scene<ReadyScene>();
        }

        return null_scene();
    }


    void display() override
    {
        ActiveWorldScene::display();

        const auto& from = load_metaclass(upgrade_from_);
        const auto& to = load_metaclass(upgrade_to_);
        auto to_sz = (*to)->constructed_size();
        auto from_sz = (*from)->constructed_size();
        int size_diff_y = to_sz.y - from_sz.y;
        int size_diff_x = to_sz.x - from_sz.x;

        if (size_diff_x) {
            for (u8 x = (target_coord_.x + to_sz.x) - size_diff_x;
                 x < target_coord_.x + to_sz.x;
                 ++x) {
                for (u8 y = target_coord_.y; y < target_coord_.y + to_sz.y;
                     ++y) {
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


private:
    MetaclassIndex upgrade_from_;
    MetaclassIndex upgrade_to_;
    RoomCoord target_coord_;
    int size_diff_;
    Coins cost_;
    Time flicker_timer_ = 0;
    bool flicker_on_ = false;
    u8 menu_opt_ = 0;
};



void UpgradePromptScene::jump_selection(u8 opt)
{
    upgrade_index_ = opt;
}



ScenePtr UpgradePromptScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(150));
    };

    if (APP.player().key_down(Key::start)) {
        auto mt = upgrade_to_[upgrade_index_];
        auto next = make_scene<GlossaryViewerModule>(mt);
        next->skip_categories();
        if (next) {
            auto coord = target_coord_;
            auto from = upgrade_from_;
            auto to = upgrade_to_;
            u8 index = upgrade_index_;
            next->set_next_scene([coord, from, to, index] {
                                     PLATFORM.screen().schedule_fade(0);
                                     auto next =
                                         make_scene<UpgradePromptScene>(coord,
                                                                        from,
                                                                        to);
                                     next->jump_selection(index);
                                     return next;
                                 });
            return next;
        }
    }

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

                auto cost = get_room_cost(&APP.player_island(), *to) -
                            get_room_cost(&APP.player_island(), *from);

                if (APP.coins() < cost) {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    err = SYS_CSTR(construction_insufficient_funds);
                    return make_scene<NotificationScene>(err, next);
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
                                if (not size_diff_y and
                                    (has_gap_left(*room) or
                                     can_displace_rightwards(player_island(),
                                                             target_coord_,
                                                             size_diff_x))) {
                                    PLATFORM.speaker().play_sound("beep_error",
                                                                  3);
                                    return make_scene<UpgradeDisplaceScene>(
                                        upgrade_from_,
                                        upgrade_to_[upgrade_index_],
                                        target_coord_,
                                        size_diff_x,
                                        cost);
                                } else {
                                    err =
                                        SYS_CSTR(construction_not_enough_space);
                                    return notify_err();
                                }
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
                                if (can_displace_upwards(player_island(),
                                                         target_coord_,
                                                         size_diff_y)) {
                                    PLATFORM.speaker().play_sound("beep_error",
                                                                  3);
                                    return make_scene<UpgradeDisplaceScene>(
                                        upgrade_from_,
                                        upgrade_to_[upgrade_index_],
                                        target_coord_,
                                        size_diff_y,
                                        cost);
                                } else {
                                    err =
                                        SYS_CSTR(construction_not_enough_space);
                                    return notify_err();
                                }
                            }
                        }
                    }
                }

                room->__unsafe__transmute(upgrade_to_[upgrade_index_]);
                room->parent()->rooms().reindex(true);
                room->parent()->recalculate_power_usage();

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
