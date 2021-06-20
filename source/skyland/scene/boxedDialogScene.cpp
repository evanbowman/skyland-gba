#include "boxedDialogScene.hpp"
#include "graphics/overlay.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "scriptHookScene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



const int y_start = 1;



bool BoxedDialogScene::advance_text(Platform& pfrm,
                                         App& app,
                                         Microseconds delta,
                                         bool sfx)
{
    const auto delay = milliseconds(80);

    text_state_.timer_ += delta;

    const auto st = calc_screen_tiles(pfrm);

    if (text_state_.timer_ > delay) {
        text_state_.timer_ = 0;

        if (sfx) {
            // pfrm.speaker().play_sound("msg", 5);
        }

        if (text_state_.current_word_remaining_ == 0) {
            while (*text_state_.current_word_ == ' ') {
                text_state_.current_word_++;
                if (text_state_.pos_ < st.x - 2) {
                    text_state_.pos_ += 1;
                }
            }
            bool done = false;
            utf8::scan(
                [&](const utf8::Codepoint& cp, const char*, int) {
                    if (done) {
                        return;
                    }
                    if (cp == ' ') {
                        done = true;
                    } else {
                        text_state_.current_word_remaining_++;
                    }
                },
                text_state_.current_word_,
                str_len(text_state_.current_word_));
        }

        // At this point, we know the length of the next space-delimited word in
        // the string. Now we can print stuff...

        const auto st = calc_screen_tiles(pfrm);
        static const auto margin_sum = 4;
        const auto text_box_width = st.x - margin_sum;
        const auto remaining = (text_box_width - text_state_.pos_) -
                               (text_state_.line_ == 0 ? 0 : 2);

        if (remaining < text_state_.current_word_remaining_) {
            if (text_state_.line_ == 0) {
                text_state_.line_++;
                text_state_.pos_ = 0;
                return true;
            } else {
                return false;
            }
        }

        int bytes_consumed = 0;
        const auto cp = utf8::getc(text_state_.current_word_, &bytes_consumed);

        const auto mapping_info = locale_texture_map()(cp);

        u16 t = 495; // bad glyph, FIXME: add a constant

        if (mapping_info) {
            t = pfrm.map_glyph(cp, *mapping_info);
        }

        const int y_offset = text_state_.line_ == 0 ? 4 + y_start : 2 + y_start;
        const int x_offset = text_state_.pos_ + 2;

        pfrm.set_tile(Layer::overlay, x_offset, st.y - (y_offset), t);

        text_state_.current_word_remaining_--;
        text_state_.current_word_ += bytes_consumed;
        text_state_.pos_++;

        if (*text_state_.current_word_ == '\0') {
            display_mode_ = DisplayMode::key_released_check2;
        }
    }

    return true;
}



void BoxedDialogScene::clear_textbox(Platform& pfrm)
{
    const auto st = calc_screen_tiles(pfrm);


    // Corners

    // pfrm.set_tile(Layer::overlay, 1, 1, 93);
    // pfrm.set_tile(Layer::overlay, 1, 2, 95);
    // pfrm.set_tile(Layer::overlay, 2, 1, 94);

    // pfrm.set_tile(Layer::overlay, st.x - 3, 1, 97);
    // pfrm.set_tile(Layer::overlay, st.x - 2, 1, 98);
    // pfrm.set_tile(Layer::overlay, st.x - 2, 2, 100);

    // pfrm.set_tile(Layer::overlay, 1, st.y - 2, 103);
    // pfrm.set_tile(Layer::overlay, 2, st.y - 2, 104);
    // pfrm.set_tile(Layer::overlay, 1, st.y - 3, 101);

    // pfrm.set_tile(Layer::overlay, st.x - 3, st.y - 2, 107);
    // pfrm.set_tile(Layer::overlay, st.x - 2, st.y - 2, 108);
    // pfrm.set_tile(Layer::overlay, st.x - 2, st.y - 3, 106);

    // End Corners


    for (int x = 1; x < st.x - 1; ++x) {
        pfrm.set_tile(Layer::overlay, x, st.y - (5 + y_start), 84);
        pfrm.set_tile(Layer::overlay, x, st.y - (4 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (3 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (2 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (1 + y_start), 85);
    }

    pfrm.set_tile(Layer::overlay, 0, st.y - (4 + y_start), 89);
    pfrm.set_tile(Layer::overlay, 0, st.y - (3 + y_start), 89);
    pfrm.set_tile(Layer::overlay, 0, st.y - (2 + y_start), 89);

    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (4 + y_start), 88);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (3 + y_start), 88);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (2 + y_start), 88);

    pfrm.set_tile(Layer::overlay, 0, st.y - (5 + y_start), 83);
    pfrm.set_tile(Layer::overlay, 0, st.y - (1 + y_start), 90);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (5 + y_start), 87);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (1 + y_start), 86);

    text_state_.line_ = 0;
    text_state_.pos_ = 0;
}



void BoxedDialogScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_dialog");

    clear_textbox(pfrm);

    text_state_.current_word_remaining_ = 0;
    text_state_.current_word_ = buffer_->c_str();
    text_state_.timer_ = 0;
    text_state_.line_ = 0;
    text_state_.pos_ = 0;
}



void BoxedDialogScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    pfrm.load_overlay_texture("overlay");

}



ScenePtr<Scene> BoxedDialogScene::update(Platform& pfrm,
                                              App& app,
                                              Microseconds delta)
{

    auto animate_moretext_icon = [&] {
        static const auto duration = milliseconds(500);
        text_state_.timer_ += delta;
        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            const auto st = calc_screen_tiles(pfrm);
            if (pfrm.get_tile(Layer::overlay, st.x - 3, st.y - (2 + y_start)) == 91) {
                pfrm.set_tile(Layer::overlay, st.x - 3, st.y - (2 + y_start), 92);
            } else {
                pfrm.set_tile(Layer::overlay, st.x - 3, st.y - (2 + y_start), 91);
            }
        }
    };

    switch (display_mode_) {
    case DisplayMode::animate_in:
        display_mode_ = DisplayMode::busy;
        break;

    case DisplayMode::busy: {

        const bool text_busy = advance_text(pfrm, app, delta, true);

        if (not text_busy) {
            display_mode_ = DisplayMode::key_released_check1;
        } else {
            if (pfrm.keyboard().down_transition<Key::action_2>() or
                pfrm.keyboard().down_transition<Key::action_1>()) {

                while (advance_text(pfrm, app, delta, false)) {
                    if (display_mode_ not_eq DisplayMode::busy) {
                        break;
                    }
                }

                if (display_mode_ == DisplayMode::busy) {
                    display_mode_ = DisplayMode::key_released_check1;
                }
            }
        }
    } break;

    case DisplayMode::wait: {
        animate_moretext_icon();

        if (pfrm.keyboard().down_transition<Key::action_2>() or
            pfrm.keyboard().down_transition<Key::action_1>()) {

            text_state_.timer_ = 0;

            clear_textbox(pfrm);
            display_mode_ = DisplayMode::busy;
        }
        break;
    }

    case DisplayMode::key_released_check1:
        // if (pfrm.keyboard().down_transition<Key::action_2>() or
        //     pfrm.keyboard().down_transition<Key::action_1>()) {

            text_state_.timer_ = seconds(1);
            display_mode_ = DisplayMode::wait;
        // }
        break;

    case DisplayMode::key_released_check2:
        text_state_.timer_ = seconds(1);
        display_mode_ = DisplayMode::done;

        if (expects_answer_y_n_) {
            // TODO...
            invoke_hook(pfrm, "after-dialog-accepted");
        }

        break;

    case DisplayMode::done:
        animate_moretext_icon();
        if (pfrm.keyboard().down_transition<Key::action_2>() or
            pfrm.keyboard().down_transition<Key::action_1>()) {

            display_mode_ = DisplayMode::animate_out;
        }
        break;

    case DisplayMode::animate_out:
        display_mode_ = DisplayMode::clear;
        pfrm.fill_overlay(0);
        break;

    case DisplayMode::clear:
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



}
