#pragma once

#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectWeaponGroupScene : public ActiveWorldScene
{
public:
    SelectWeaponGroupScene(DeferredScene cancel) : cancel_(cancel)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        ActiveWorldScene::enter(pfrm, app, prev);

        text_.emplace(pfrm,
                      SYSTR(modifier_keys_opt_5)->c_str(),
                      OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});

        text_->append(" ");

        pfrm.set_tile(
            Layer::overlay, text_->len(), calc_screen_tiles(pfrm).y - 1, 395);

        pfrm.set_tile(Layer::overlay,
                      text_->len() + 1,
                      calc_screen_tiles(pfrm).y - 1,
                      393);

        pfrm.set_tile(Layer::overlay,
                      text_->len() + 2,
                      calc_screen_tiles(pfrm).y - 1,
                      394);

        for (int i = 0; i < text_->len() + 3; ++i) {
            pfrm.set_tile(Layer::overlay, i, 18, 425);
        }
    }



    void exit(Platform& pfrm, App& app, Scene& prev) override
    {
        ActiveWorldScene::exit(pfrm, app, prev);

        text_.reset();

        pfrm.fill_overlay(0);
    }



    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
            return next;
        }

        if (app.player().key_down(pfrm, Key::up)) {
            for (auto& room : app.player_island().rooms()) {
                if (room->group() == Room::Group::one) {
                    if (auto scene =
                            room->select(pfrm, app, room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (app.player().key_down(pfrm, Key::right)) {
            for (auto& room : app.player_island().rooms()) {
                if (room->group() == Room::Group::two) {
                    if (auto scene =
                            room->select(pfrm, app, room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (app.player().key_down(pfrm, Key::left)) {
            for (auto& room : app.player_island().rooms()) {
                if (room->group() == Room::Group::three) {
                    if (auto scene =
                            room->select(pfrm, app, room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            return cancel_();
        }

        return null_scene();
    }

private:
    DeferredScene cancel_;
    std::optional<Text> text_;
};



} // namespace skyland
