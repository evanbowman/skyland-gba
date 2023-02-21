#include "itemShopScene.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



ItemShopScene::ItemShopScene()
    : items_(allocate_dynamic<ItemsBuffer>("shop-items-buffer"))
{
}



void ItemShopScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    persist_ui();

    WorldScene::enter(pfrm, app, prev);

    pfrm.speaker().play_sound("openbag", 3);

    auto items = lisp::get_var("shop-items");
    lisp::foreach (items, [this](lisp::Value* item) {
        auto mt = metaclass_index(lisp::get_list(item, 0)->symbol().name());
        u16 price = lisp::get_list(item, 1)->integer().value_;
        u16 qty = lisp::get_list(item, 2)->integer().value_;
        if (items_->full()) {
            Platform::fatal("shop item list cannot exceed 4 elems");
        }
        items_->push_back(ShopItem{mt, price, qty});
    });

    if (items_->empty()) {
        Platform::fatal("no shop items defined!?");
    }
}



void ItemShopScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);
    pfrm.screen().schedule_fade(0);
    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
ItemShopScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = WorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    int tile_mem[4] = {258, 181, 197, 213};
    Vec2<u8> slots[] = {{4, 5}, {15, 5}, {4, 11}, {15, 11}};

    auto item_slot = [&](int x, int y) -> u32 { return y * 2 + x; };

    constexpr auto fade_duration = milliseconds(300);

    switch (state_) {
    case State::fade_in:
        timer_ += delta;
        if (timer_ > fade_duration) {
            pfrm.screen().schedule_fade(0.5f);
            timer_ = 0;
            state_ = State::animate_box;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(0.5f * amount);
        }
        break;

    case State::animate_box: {
        timer_ += delta;
        static const auto sweep_duration = milliseconds(100);
        const auto st = calc_screen_tiles(pfrm);

        if (timer_ > sweep_duration) {
            timer_ = 0;
            state_ = State::ready;
            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y + 1, 112);
                }
            }

            int i = 0;
            for (auto& item : *items_) {
                auto icon = (*load_metaclass(item.mt_))->unsel_icon();
                if (i == 0) {
                    icon = (*load_metaclass(item.mt_))->icon();
                }
                draw_image(pfrm,
                           tile_mem[i],
                           slots[i].x,
                           slots[i].y,
                           4,
                           4,
                           Layer::overlay);
                pfrm.load_overlay_chunk(tile_mem[i], icon, 16);
                u8 x = slots[i].x + 5;
                u8 y = slots[i].y + 1;
                Text::print(pfrm, format("%@", item.price_).c_str(), {x, y});
                y += 1;
                Text::print(pfrm, format("x%", item.qty_).c_str(), {x, y});
                ++i;
            }

        } else {
            const int total = st.y - 7;
            const int progress =
                total * smoothstep(0.f, sweep_duration, timer_);

            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    if (y <= progress + 3) {
                        pfrm.set_tile(Layer::overlay, x, y + 1, 112);
                    }
                }
            }
        }

        break;
    }

    case State::ready:
        if (player(app).key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }
        if (player(app).key_down(pfrm, Key::action_1)) {

            auto fn = lisp::get_var("on-shop-item-sel");
            if (fn->type() == lisp::Value::Type::function) {
                auto mt_i = (*items_)[item_slot(cursor_.x, cursor_.y)].mt_;
                auto mt = load_metaclass(mt_i);
                lisp::push_op(lisp::make_string((*mt)->ui_name(pfrm)->c_str()));
                lisp::push_op(L_INT(item_slot(cursor_.x, cursor_.y)));
                lisp::safecall(fn, 2);
                lisp::pop_op(); // funcall result

                app.time_stream().clear();
                time_stream::event::Initial e;
                app.time_stream().push(app.level_timer(), e);
                return null_scene();
            }
        }
        auto move_cursor = [&](int x2, int y2) {
            int i = item_slot(cursor_.x, cursor_.y);
            auto icon = (*load_metaclass((*items_)[i].mt_))->unsel_icon();
            pfrm.load_overlay_chunk(tile_mem[i], icon, 16);
            cursor_.y = y2;
            cursor_.x = x2;
            pfrm.speaker().play_sound("cursor_tick", 0);
            i = item_slot(cursor_.x, cursor_.y);
            icon = (*load_metaclass((*items_)[i].mt_))->icon();
            pfrm.load_overlay_chunk(tile_mem[i], icon, 16);
        };
        if (player(app).key_down(pfrm, Key::down) and cursor_.y == 0) {
            if (item_slot(cursor_.x, cursor_.y + 1) < items_->size()) {
                move_cursor(cursor_.x, cursor_.y + 1);
            }
        } else if (player(app).key_down(pfrm, Key::up) and cursor_.y == 1) {
            move_cursor(cursor_.x, cursor_.y - 1);
        }
        if (player(app).key_down(pfrm, Key::left) and cursor_.x == 1) {
            move_cursor(cursor_.x - 1, cursor_.y);
        } else if (player(app).key_down(pfrm, Key::right) and cursor_.x == 0) {
            if (item_slot(cursor_.x + 1, cursor_.y) < items_->size()) {
                move_cursor(cursor_.x + 1, cursor_.y);
            }
        }
        break;
    }

    return null_scene();
}



} // namespace skyland
