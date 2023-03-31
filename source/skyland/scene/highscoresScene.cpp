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
#include "achievementNotificationScene.hpp"
#include "base32.hpp"
#include "qrViewerScene.hpp"
#include "skyland/loginToken.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "textEntryScene.hpp"
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

    int metrics_y_offset_ = -6;

    if (not show_current_score_) {
        auto str = SYSTR(highscores_leaderboard);
        auto len = utf8::len(str->c_str());
        u8 margin = centered_text_margins(pfrm, len);
        leaderboard_text_.emplace(pfrm, OverlayCoord{margin, 17});
        leaderboard_text_->assign(str->c_str());
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

    bool changed = false;

    if (not app.is_developer_mode() and
        not(app.persistent_data().state_flags_.get() &
            PersistentData::StateFlag::dev_mode_active)) {
        for (auto& highscore : reversed(highscores.values_)) {
            if (highscore.get() < (u32)score) {
                highscore.set(score);
                changed = true;
                break;
            }
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

    if (not disable_writeback_ and not app.is_developer_mode()) {
        if (show_current_score_ and highscores.values_[0].get() == (u32)score) {
            highscores.highest_score_play_seconds_.set(
                app.persistent_data().total_seconds_.get());
            highscores.highest_score_multiplier_used_ = score_multiplier;
        }

        if (changed) {
            save::store_global_data(pfrm, app.gp_);
        }
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
    leaderboard_text_.reset();
}



static Vector<char> encode_highscore_data(Platform& pfrm, App& app)
{
    StringBuffer<LoginToken::size> token_str;
    for (int i = 0; i < 8; ++i) {
        token_str.push_back(__login_token.text_[i]);
    }

    struct Payload
    {
        host_u32 score_;
        u8 trick_1_;
        u8 login_token_[LoginToken::size];
        // Split for backwards compatibility with the server.
        u8 fs_checksum_1_;
        u8 score_multiplier_;
        u8 fs_checksum_2_;
        host_u32 time_seconds_;
        u8 flag_data_[72];
        u8 pad_[8];
    } payload;

    payload.score_.set(app.gp_.highscores_.values_[0].get());
    memcpy(payload.login_token_, __login_token.text_, LoginToken::size);

    payload.time_seconds_.set(
        app.gp_.highscores_.highest_score_play_seconds_.get());
    payload.score_multiplier_ =
        app.gp_.highscores_.highest_score_multiplier_used_;

    // Encode the flag data, one 4 bit pixel at a time.
    auto& flag = app.custom_flag_image_;
    // Sanity check.
    static_assert((FlagPixels::width * FlagPixels::height) / 2 <=
                  sizeof payload.flag_data_);
    u8* flag_data_out = payload.flag_data_;
    int parity = 0; // even/odd nibble
    for (int x = 0; x < FlagPixels::width; ++x) {
        for (int y = 0; y < FlagPixels::height; ++y) {
            if (parity == 0) {
                *flag_data_out = flag.pixels[x][y] & 0x0f;
                parity = 1;
            } else {
                *flag_data_out |= (flag.pixels[x][y] & 0x0f) << 4;
                ++flag_data_out;
                parity = 0;
            }
        }
    }

    // Just to confuse people who try to decode the format. We padded
    // the structure with three bytes to make it a multiple of five, to
    // avoid base32 delimiters on urls, might as well use the bytes for
    // something. We want to discourage cheaters, who might try to send
    // false data to the highscore server.
    payload.trick_1_ = ~(payload.score_.get());

    u16 fs_checksum = 0;

    pfrm.walk_filesystem([&pfrm, &fs_checksum](const char* path) {
        if (auto f = pfrm.load_file_contents("", path)) {
            StringBuffer<86> str_path(path);
            if (ends_with(StringBuffer<4>(".lisp"), str_path)) {
                while (*f not_eq '\0') {
                    fs_checksum += *f;
                    ++f;
                }
            }
        }
    });

    payload.fs_checksum_1_ = fs_checksum & 0xff;
    payload.fs_checksum_2_ = (fs_checksum >> 8) & 0xff;

    static_assert(sizeof(Payload) % 5 == 0,
                  "Base32 string not multiple of five, i.e. will contain "
                  "invalid '=' delimiters when url-encoded.");

    // static_assert(sizeof(Payload) == 20);

    Vector<char> data;

    static_assert((sizeof payload) % 2 == 0);

    // Pack in the upper nibbles, followed by the lower nibbles.
    for (u32 i = 0; i < sizeof payload; i += 2) {
        data.push_back((((u8*)&payload)[i] & 0x0f) |
                       ((((u8*)&payload)[i + 1] & 0x0f) << 4));
    }
    for (u32 i = 0; i < sizeof payload; i += 2) {
        data.push_back((((u8*)&payload)[i] & 0xf0) |
                       ((((u8*)&payload)[i + 1] & 0xf0) >> 4));
    }

    const char* cipher_alphabet = "5yet2s4waiobjmdx3hzg6nv7cfqprklu";

    return base32::encode(data, cipher_alphabet);
}



ScenePtr<Scene> HighscoresScene::update(Platform& pfrm, App& app, Microseconds)
{
    if (app.player().key_pressed(pfrm, Key::select)) {
        return scene_pool::alloc<ConfiguredURLQRViewerScene>(
            "/scripts/config/leaderboard.lisp",
            "",
            SYSTR(highscores_scan_qr_leaderboard)->c_str(),
            scene_pool::make_deferred_scene<HighscoresScene>());
    }

    if (app.player().key_pressed(pfrm, Key::alt_1) and
        app.player().key_pressed(pfrm, Key::alt_2)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        auto p = title_screen_page_;

        auto next = [p, &app, &pfrm]() {
            auto encoded = encode_highscore_data(pfrm, app);

            StringBuffer<200> temp;
            for (auto& c : encoded) {
                temp.push_back(c);
            }

            return scene_pool::alloc<ConfiguredURLQRViewerScene>(
                "/scripts/config/uploadscore.lisp",
                format<300>("?d=%", temp.c_str()).c_str(),
                SYSTR(score_upload_prompt_3)->c_str(),
                scene_pool::make_deferred_scene<HighscoresScene>(),
                ColorConstant::rich_black);
        };



        if (__login_token.valid_) {
            return next();
        }

        auto gettext = [next, &pfrm]() {
            auto receive = [next](const char* text) {
                __login_token.valid_ = true;
                if (str_len(text) not_eq 8) {
                    // sanity check
                    Platform::fatal("qr text logic error");
                }
                memcpy(__login_token.text_, text, 8);
                return next();
            };
            auto prompt = SYSTR(score_upload_enter_token);
            return scene_pool::alloc<TextEntryScene>(
                prompt->c_str(), receive, 8, 8);
        };

        return scene_pool::alloc<ConfiguredURLQRViewerScene>(
            "/scripts/config/login.lisp",
            "",
            SYSTR(score_upload_prompt_1)->c_str(),
            gettext);
    }

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().key_down(pfrm, Key::action_2)) {

        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update(pfrm, app);
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(pfrm, app, achievement);

                pfrm.screen().fade(1.f);

                const auto pg = title_screen_page_;

                auto next =
                    scene_pool::make_deferred_scene<TitleScreenScene>(pg);
                return scene_pool::alloc<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }

        return scene_pool::alloc<TitleScreenScene>(title_screen_page_);
    }
    return null_scene();
}



HighscoresScene::Factory HighscoresScene::factory_;



} // namespace skyland
