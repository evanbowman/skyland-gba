#include "shopModule.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



static SHARED_VARIABLE(shop_items_locked);



struct ShopItem {
    enum class Type {
        room,
        drone,
    } type_;

    int class_index_;
};



static const int shop_item_count = 2;



static const ShopItem shop_items[shop_item_count] = {
    {
        ShopItem::Type::drone,
        DroneMeta::index("repair-drone"),
    },
    {
        ShopItem::Type::room,
        metaclass_index("decimator"),
    }};



static const Coins shop_item_costs[shop_item_count] = {10000, 1000000};



void ShopModule::exit(Platform& pfrm, App& app, Scene& next)
{
    text_.reset();
    pfrm.fill_overlay(0);
}



void ShopModule::repaint(Platform& pfrm, App& app)
{
    auto st = calc_screen_tiles(pfrm);

    auto name = [&](int index) {
        switch (shop_items[index].type_) {
        case ShopItem::Type::drone:
            return drone_metatable()
                .first[shop_items[index].class_index_]
                ->name();

        case ShopItem::Type::room:
            return (*load_metaclass(shop_items[index].class_index_))->name();
        }
        return "ERROR";
    };

    StringBuffer<30> message = "unlock: ";
    message += name(selector_);
    message += " ";
    message += stringify(shop_item_costs[selector_]);
    message += "@";

    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, u8(st.y - 1)});
    }
    text_->assign(message.c_str());

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        pfrm.set_tile(Layer::overlay, i, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 5, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 0);
    }

    for (int i = st.x - 25; i < st.x - 5; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        pfrm.set_tile(Layer::overlay, st.x - 26, y, 128);
        pfrm.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    pfrm.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);


    auto sel_icon = [&](int index) {
        switch (shop_items[index].type_) {
        case ShopItem::Type::drone:
            return drone_metatable()
                .first[shop_items[index].class_index_]
                ->icon();

        case ShopItem::Type::room:
            return (*load_metaclass(shop_items[index].class_index_))->icon();
        }
        return u16(0);
    };

    auto unsel_icon = [&](int index) {
        switch (shop_items[index].type_) {
        case ShopItem::Type::drone:
            return drone_metatable()
                .first[shop_items[index].class_index_]
                ->unsel_icon();

        case ShopItem::Type::room:
            return (*load_metaclass(shop_items[index].class_index_))
                ->unsel_icon();
        }
        return u16(0);
    };


    {
        int index = selector_;
        if (index - 2 < -1) {
            index = shop_item_count - 2;
        } else if (index - 2 < 0) {
            index = shop_item_count - 1;
        } else {
            index = index - 2;
        }

        auto icon = unsel_icon(index);
        draw_image(pfrm, 258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = shop_item_count - 1;
        } else {
            index = index - 1;
        }

        auto icon = unsel_icon(index);
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = sel_icon(selector_);
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)shop_item_count) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = unsel_icon(index);
        draw_image(pfrm, 213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)shop_item_count) {
            index = 1;
        } else if (index + 2 >= (int)shop_item_count) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon = unsel_icon(index);
        draw_image(pfrm, 274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(274, icon, 16);
    }
}



void ShopModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    repaint(pfrm, app);
    pfrm.screen().fade(0.f);
}



ScenePtr<Scene> ShopModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    Module::update(pfrm, app, delta);

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    if (app.player().key_down(pfrm, Key::right)) {
        if (selector_ == shop_item_count - 1) {
            selector_ = 0;
        } else {
            ++selector_;
        }
        repaint(pfrm, app);
    }

    if (app.player().key_down(pfrm, Key::left)) {
        if (selector_ == 0) {
            selector_ = shop_item_count - 1;
        } else {
            --selector_;
        }
        repaint(pfrm, app);
    }

    return null_scene();
}



// ShopModule::Factory ShopModule::factory_;



} // namespace skyland
