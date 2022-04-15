////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "highscoresScene.hpp"
#include "base32.hpp"
#include "qrViewerScene.hpp"
#include "skyland/loginToken.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



HighscoresScene::HighscoresScene()
    : show_current_score_(false), disable_writeback_(true),
      title_screen_page_(3)
{
}



HighscoresScene::HighscoresScene(bool show_current_score, int title_screen_page)
    : show_current_score_(show_current_score),
      disable_writeback_(show_current_score == false),
      title_screen_page_(title_screen_page)
{
}



extern SharedVariable score_multiplier;



void HighscoresScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().schedule_fade(0.95f);
    pfrm.screen().schedule_fade(1.f);

    const auto screen_tiles = calc_screen_tiles(pfrm);

    int metrics_y_offset_ = -5;

    if (show_current_score_) {
        --metrics_y_offset_;
    }


    const auto dot = ".";

    auto print_metric_impl = [&](const char* str,
                                 const StringBuffer<32>& text,
                                 const char* suffix = "",
                                 bool highlight = false) {
        if (lines_.full()) {
            return;
        }

        lines_.emplace_back(
            pfrm, Vec2<u8>{3, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

        const auto colors =
            highlight
                ? Text::OptColors{FontColors{ColorConstant::rich_black,
                                             ColorConstant::aerospace_orange}}
                : Text::OptColors{};


        lines_.back().append(str, colors);

        const auto iters = screen_tiles.x - (utf8::len(str) + 6 +
                                             text.length() + utf8::len(suffix));


        for (u32 i = 0; i < iters; ++i) {
            lines_.back().append(dot, colors);
        }

        lines_.back().append(text.c_str(), colors);
        lines_.back().append(suffix, colors);
    };


    int score = show_current_score_ ? app.persistent_data().score_.get() : 0;
    if (score < 0) {
        score = 0;
    }

    auto print_metric = [&](const char* str,
                            int num,
                            const char* suffix = "",
                            bool highlight = false) {
        print_metric_impl(str, stringify(num), suffix, highlight);
    };

    auto& highscores = app.gp_.highscores_;

    for (auto& highscore : reversed(highscores.values_)) {
        if (highscore.get() < (u32)score) {
            highscore.set(score);
            break;
        }
    }

    std::sort(
        std::begin(highscores.values_),
        std::end(highscores.values_),
        [](const auto& lhs, const auto& rhs) { return lhs.get() > rhs.get(); });

    if (show_current_score_) {
        print_metric(SYSTR(highscores_score)->c_str(), score);
    }

    lines_.emplace_back(
        pfrm, Vec2<u8>{7, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

    lines_.back().append(SYSTR(highscores_title)->c_str());


    bool highlighted = false;
    for (int i = 0; i < Highscores::count; ++i) {
        StringBuffer<24> str;
        str += stringify(i + 1);
        str += " ";
        bool highlight = show_current_score_ and not highlighted and
                         highscores.values_[i].get() == (u32)score;
        print_metric(str.c_str(), highscores.values_[i].get(), "", highlight);

        if (highlight) {
            highlighted = true;
        }
    }

    if (not disable_writeback_) {
        if (show_current_score_ and highscores.values_[0].get() == (u32)score) {
            highscores.highest_score_play_seconds_.set(
                app.persistent_data().total_seconds_.get());
            highscores.highest_score_multiplier_used_ = score_multiplier;
        }

        save::store_global_data(pfrm, app.gp_);
    }

    auto upload = SYSTR(highscores_upload);

    u8 margin = centered_text_margins(pfrm, utf8::len(upload->c_str()));

    upload_hint_.emplace(pfrm, OverlayCoord{margin, u8(screen_tiles.y - 1)});
    upload_hint_->assign(
        upload->c_str(),
        OptColors{{ColorConstant::rich_black, ColorConstant::silver_white}});
}



void HighscoresScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    lines_.clear();
    upload_hint_.reset();
}



ScenePtr<Scene> HighscoresScene::update(Platform& pfrm, App& app, Microseconds)
{
    if (app.player().key_pressed(pfrm, Key::alt_1) and
        app.player().key_pressed(pfrm, Key::alt_2)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        auto p = title_screen_page_;
        auto next = [p, &app, &pfrm]() {
            StringBuffer<LoginToken::size> token_str;
            for (int i = 0; i < 8; ++i) {
                token_str.push_back(__login_token.text_[i]);
            }

            struct Payload
            {
                host_u32 score_;
                u8 trick_1_;
                u8 login_token_[LoginToken::size];
                u8 trick_2_;
                u8 score_multiplier_;
                u8 trick_3_;
                host_u32 time_seconds_;
            } payload;

            payload.score_.set(app.gp_.highscores_.values_[0].get());
            memcpy(payload.login_token_, __login_token.text_, LoginToken::size);

            payload.time_seconds_.set(
                app.gp_.highscores_.highest_score_play_seconds_.get());
            payload.score_multiplier_ =
                app.gp_.highscores_.highest_score_multiplier_used_;

            // Just to confuse people who try to decode the format. We padded
            // the structure with three bytes to make it a multiple of five, to
            // avoid base32 delimiters on urls, might as well use the bytes for
            // something. We want to discourage cheaters, who might try to send
            // false data to the highscore server.
            payload.trick_1_ = ~(payload.score_.get());
            payload.trick_2_ = 0xaa ^ (payload.score_.get());
            payload.trick_3_ =
                (payload.trick_1_ | payload.trick_2_) ^ payload.login_token_[3];

            static_assert(
                sizeof(Payload) % 5 == 0,
                "Base32 string not multiple of five, i.e. will contain "
                "invalid '=' delimiters when url-encoded.");

            static_assert(sizeof(Payload) == 20);

            Vector<char> data;
            // Unrolled rail-fence cipher.
            data.push_back(((u8*)&payload)[0]); // row 1
            data.push_back(((u8*)&payload)[4]);
            data.push_back(((u8*)&payload)[8]);
            data.push_back(((u8*)&payload)[12]);
            data.push_back(((u8*)&payload)[16]);

            data.push_back(((u8*)&payload)[1]); // row 2
            data.push_back(((u8*)&payload)[3]);
            data.push_back(((u8*)&payload)[5]);
            data.push_back(((u8*)&payload)[7]);
            data.push_back(((u8*)&payload)[9]);
            data.push_back(((u8*)&payload)[11]);
            data.push_back(((u8*)&payload)[13]);
            data.push_back(((u8*)&payload)[15]);
            data.push_back(((u8*)&payload)[17]);
            data.push_back(((u8*)&payload)[19]);

            data.push_back(((u8*)&payload)[2]); // row 3
            data.push_back(((u8*)&payload)[6]);
            data.push_back(((u8*)&payload)[10]);
            data.push_back(((u8*)&payload)[14]);
            data.push_back(((u8*)&payload)[18]);

            auto encoded = base32::encode(data);

            StringBuffer<64> temp;
            for (auto& c : encoded) {
                temp.push_back(c);
            }

            return scene_pool::alloc<ConfiguredURLQRViewerScene>(
                "/scripts/config/uploadscore.lisp",
                format("?d=%", temp.c_str()).c_str(),
                SYSTR(score_upload_prompt_3)->c_str(),
                scene_pool::make_deferred_scene<HighscoresScene>());
        };

        if (__login_token.valid_) {
            return next();
        }

        __login_token.valid_ = true;

        return scene_pool::alloc<ConfiguredURLQRViewerScene>(
            "/scripts/config/login.lisp",
            "",
            SYSTR(score_upload_prompt_1)->c_str(),
            next);
    }

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>(title_screen_page_);
    }
    return null_scene();
}



HighscoresScene::Factory HighscoresScene::factory_;



} // namespace skyland
