#include "keyComboScene.hpp"
#include "skyland/keyCallbackProcessor.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"



namespace skyland {



void KeyComboScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    text_data_ = "shortcut: ";

    text_.emplace(pfrm, text_data_.c_str(), OverlayCoord{0, 19});
    for (int i = text_->len(); i < 30; ++i) {
        text_->append(" ");
    }

    for (int i = 0; i < 30; ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }

    key_callback_processor.reset();
}



void KeyComboScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    pfrm.fill_overlay(0);
    text_.reset();
}



ScenePtr<Scene> KeyComboScene::update(Platform& pfrm,
                                      App& app,
                                      Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (app.player().key_down(pfrm, Key::start) or
        key_callback_processor.seek_state() == KeyCallbackProcessor::seq_max - 1) {

        if (auto binding = key_callback_processor.match()) {
            binding->callback_(pfrm, app);
        }
        key_callback_processor.reset();
        return scene_pool::alloc<ReadyScene>();
    }

    key_callback_processor.update(pfrm);

    Key found = Key::count;
    for (int i = 0; i < (int)Key::count; ++i) {
        auto k = (Key)i;
        if (pfrm.keyboard().down_transition(k)) {
            found = k;
            break;
        }
    }

    if (found not_eq Key::count and
        found not_eq Key::alt_1) {

        text_data_ += [found] {
            switch (found) {
            case Key::left:
                return "l-";

            case Key::right:
                return "r-";

            case Key::up:
                return "u-";

            case Key::down:
                return "d-";

            case Key::action_1:
                return "a-";

            case Key::action_2:
                return "b-";

            default:
                return "err";
            }
        }();

        text_->assign(text_data_.c_str());

        for (int i = text_->len(); i < 30; ++i) {
            text_->append(" ");
        }
    }

    return null_scene();
}



}
