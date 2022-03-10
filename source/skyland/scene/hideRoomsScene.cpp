#include "hideRoomsScene.hpp"
#include "skyland/player/player.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



void HideRoomsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    data_ = allocate_dynamic<Data>(pfrm, "hide-rooms-context");


    Text::platform_retain_alphabet(pfrm);


    auto [mt, ms] = room_metatable();
    for (int i = 0; i < ms; ++i) {
        if (is_enabled(i)) {
            (*data_)->room_classes_.push_back(i);
        }
    }

    repaint(pfrm);
}



void HideRoomsScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    names_.clear();
}



void HideRoomsScene::repaint(Platform& pfrm)
{
    auto [mt, ms] = room_metatable();

    if (index_ >= ms or index_ >= (int)(*data_)->room_classes_.size()) {
        Platform::fatal("glossary: invalid index");
    }

    names_.clear();
    hidden_.clear();

    auto put = [&](int index, int vram, int y, bool shade) {
        if (index >= (int)(*data_)->room_classes_.size()) {
            return;
        }
        auto& m = mt[(*data_)->room_classes_[index]];
        auto icon = shade ? m->icon() : m->unsel_icon();
        draw_image(pfrm, vram, 1, y, 4, 4, Layer::overlay);
        pfrm.load_overlay_chunk(vram, icon, 16);

        StringBuffer<48> description;
        description += m->ui_name(pfrm)->c_str();

        names_.emplace_back(
            pfrm, description.c_str(), OverlayCoord{6, u8(y + 1)});

        Text::OptColors opts;
        if (shade) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        if (is_hidden((*data_)->room_classes_[index])) {
            auto str = SYSTR(yes);
            hidden_.emplace_back(
                pfrm,
                OverlayCoord{(u8)((calc_screen_tiles(pfrm).x - 1) -
                                  utf8::len(str->c_str())),
                             u8(y + 2)});
            hidden_.back().assign(str->c_str());
        } else {
            auto str = SYSTR(no);
            hidden_.emplace_back(
                pfrm,
                OverlayCoord{(u8)((calc_screen_tiles(pfrm).x - 1) -
                                  utf8::len(str->c_str())),
                             u8(y + 2)});
            hidden_.back().assign(str->c_str());
        }
    };

    put(index_, 181, 3, true);
    put(index_ + 1, 197, 10, false);
    put(index_ + 2, 213, 15, false);

    for (int x = 2; x < calc_screen_tiles(pfrm).x - 2; ++x) {
        pfrm.set_tile(Layer::overlay, x, 8, 377);
    }
}



ScenePtr<Scene>
HideRoomsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    if (player(app).key_down(pfrm, Key::action_2)) {
        return next_();
    }

    if (player(app).key_down(pfrm, Key::action_1)) {
        auto mti = (*data_)->room_classes_[index_];
        set_hidden(mti, not is_hidden(mti));
        repaint(pfrm);
    }

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    int limit = (int)(*data_)->room_classes_.size();
    if (test_key(Key::down) and index_ < limit - 1) {
        ++index_;
        repaint(pfrm);
    }

    if (test_key(Key::up) and index_ > 0) {
        --index_;
        repaint(pfrm);
    }

    return null_scene();
}



} // namespace skyland
