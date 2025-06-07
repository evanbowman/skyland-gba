////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "highscoresScene.hpp"
#include "achievementNotificationScene.hpp"
#include "base32.hpp"
#include "platform/flash_filesystem.hpp"
#include "qrViewerScene.hpp"
#include "script/lisp.hpp"
#include "skyland/loginToken.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "textEntryScene.hpp"
#include "titleScreenScene.hpp"
#include "version.hpp"



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



// People wanted to see island images uploaded to the game's highscore site. So
// I started needing to save island data when a player beats his/her previous
// highscore, in order to upload the data to the webserver along with highscore
// info.
struct HighscoreIslandInfo
{
    struct BlockData
    {
        u8 type_;
        u8 pos_;

        void set_xpos(u8 xpos)
        {
            pos_ &= 0xf0;
            pos_ |= 0x0f & xpos;
        }

        void set_ypos(u8 ypos)
        {
            pos_ &= 0x0f;
            pos_ |= (0x0f & ypos) << 4;
        }
    };

    using CharacterInfo = u8;
    CharacterInfo chrs_[16];

    static constexpr int max_blocks = 62;
    BlockData blocks_[max_blocks];
};
static_assert(alignof(HighscoreIslandInfo) == 1);



const char* highscore_island_file = "/save/hs_isle.dat";



Optional<HighscoreIslandInfo> highscore_island_info_load()
{
    HighscoreIslandInfo result;

    if (not flash_filesystem::file_exists(highscore_island_file)) {
        return std::nullopt;
    }

    Vector<char> data;
    flash_filesystem::read_file_data_binary(highscore_island_file, data);

    if (data.size() not_eq sizeof result) {
        return std::nullopt;
    }

    auto it = (u8*)&result;
    for (char c : data) {
        *(it++) = c;
    }

    return result;
}



void highscore_island_info_store()
{
    HighscoreIslandInfo info;
    int blockdata_iter = 0;
    int chr_iter = 0;

    memset(&info, 0, sizeof info);

    flash_filesystem::unlink_file(highscore_island_file);

    if (not APP.has_backup()) {
        return;
    }

    if (APP.game_mode() not_eq App::GameMode::adventure) {
        return;
    }

    // Use the last backup created before the end of the level where the player
    // finished the game (win or loss).
    auto backup = APP.get_backup();
    if (backup->lisp_data_) {

        using namespace lisp;

        VectorCharSequence seq(*backup->lisp_data_);
        read(seq); // result at stack top

        // NOTE: see scripts/save.lisp for format. It's an association list.
        l_foreach(get_op0(), [&](Value* val) {
            if (str_eq(val->cons().car()->symbol().name(), "rooms")) {
                auto lat = val->cons().cdr();
                l_foreach(lat, [&](Value* val) {
                    if (blockdata_iter == HighscoreIslandInfo::max_blocks) {
                        return;
                    }
                    auto& rname = get_list(val, 0)->symbol();
                    auto& rx = get_list(val, 1)->integer();
                    auto& ry = get_list(val, 2)->integer();

                    auto mt = metaclass_index(rname.name());
                    HighscoreIslandInfo::BlockData bd;
                    bd.type_ = mt + 1; // 0 used as null room
                    bd.set_xpos(rx.value_);
                    bd.set_ypos(ry.value_);
                    info.blocks_[blockdata_iter++] = bd;
                });
            }
            if (str_eq(val->cons().car()->symbol().name(), "chrs")) {
                auto lat = val->cons().cdr();
                l_foreach(lat, [&](Value* val) {
                    if (chr_iter == 16) {
                        return;
                    }

                    auto& rx = get_list(val, 0)->integer();
                    auto& ry = get_list(val, 1)->integer();

                    u8 chr_pos = 0;
                    chr_pos |= rx.value_ & 0x0f;
                    chr_pos |= (ry.value_ & 0x0f) << 4;

                    info.chrs_[chr_iter++] = chr_pos;
                });
            }
        });

        pop_op(); // result of read(seq)

        Vector<char> result;
        for (u32 i = 0; i < sizeof info; ++i) {
            result.push_back(((u8*)(&info))[i]);
        }

        flash_filesystem::store_file_data_binary(highscore_island_file, result);
    }
}



void HighscoresScene::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(0.95f);
    PLATFORM.screen().schedule_fade(1.f);

    const auto screen_tiles = calc_screen_tiles();

    int metrics_y_offset_ = -6;

    if (not show_current_score_) {
        auto str = SYSTR(highscores_leaderboard);
        auto len = utf8::len(str->c_str());
        u8 margin = centered_text_margins(len);
        leaderboard_text_.emplace(OverlayCoord{margin, 17});
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
            Vec2<u8>{3, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

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


    int score = show_current_score_ ? APP.persistent_data().score_.get() : 0;
    if (score < 0) {
        score = 0;
    }

    auto print_metric = [&](const char* str,
                            int num,
                            const char* suffix = "",
                            bool highlight = false) {
        print_metric_impl(str, stringify(num), suffix, highlight);
    };

    auto& highscores = APP.gp_.highscores_;

    bool changed = false;

    if (not APP.is_developer_mode() and
        not(APP.persistent_data().state_flags_.get() &
            PersistentData::StateFlag::dev_mode_active)) {

        if (score > (int)highscores.values_[0].get()) {
            highscore_island_info_store();
        }

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
        Vec2<u8>{7, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

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

    if (not disable_writeback_ and not APP.is_developer_mode()) {
        if (show_current_score_ and highscores.values_[0].get() == (u32)score) {
            highscores.highest_score_play_seconds_.set(
                APP.persistent_data().total_seconds_.get());
            highscores.highest_score_multiplier_used_ = score_multiplier;
        }

        if (changed) {
            save::store_global_data(APP.gp_);
        }
    }

    auto upload = SYSTR(highscores_upload);

    u8 margin = centered_text_margins(utf8::len(upload->c_str()));

    upload_hint_.emplace(OverlayCoord{margin, u8(screen_tiles.y - 1)});
    upload_hint_->assign(
        upload->c_str(),
        OptColors{{ColorConstant::rich_black, ColorConstant::silver_white}});
}



void HighscoresScene::exit(Scene& prev)
{
    lines_.clear();
    upload_hint_.reset();
    leaderboard_text_.reset();
}



static Vector<char> encode_highscore_data()
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

        HighscoreIslandInfo island_;

        u8 version_major_;
        u8 version_minor_;
        u8 version_subminor_;
        u8 version_revision_;

        u8 pad_[4]; // Adjust this until the static asserts (below) pass.
    } payload;
    static_assert(alignof(decltype(payload)) == 1);

    payload.version_major_ = PROGRAM_MAJOR_VERSION - 2000;
    payload.version_minor_ = PROGRAM_MINOR_VERSION;
    payload.version_subminor_ = PROGRAM_SUBMINOR_VERSION;
    payload.version_revision_ = PROGRAM_VERSION_REVISION;

    if (auto info = highscore_island_info_load()) {
        payload.island_ = *info;
    } else {
        memset(&payload.island_, 0, sizeof payload.island_);
    }

    payload.score_.set(APP.gp_.highscores_.values_[0].get());
    memcpy(payload.login_token_, __login_token.text_, LoginToken::size);

    payload.time_seconds_.set(
        APP.gp_.highscores_.highest_score_play_seconds_.get());
    payload.score_multiplier_ =
        APP.gp_.highscores_.highest_score_multiplier_used_;

    // Encode the flag data, one 4 bit pixel at a time.
    auto& flag = APP.custom_flag_image_;
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

    PLATFORM.walk_filesystem([&fs_checksum](const char* path) {
        if (auto f = PLATFORM.load_file_contents("", path)) {
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



ScenePtr HighscoresScene::update(Time)
{
    if (APP.player().key_pressed(Key::select)) {
        return make_scene<ConfiguredURLQRViewerScene>(
            "/scripts/config/leaderboard.lisp",
            "",
            SYSTR(highscores_scan_qr_leaderboard)->c_str(),
            make_deferred_scene<HighscoresScene>());
    }

    if (APP.player().key_pressed(Key::alt_1) and
        APP.player().key_pressed(Key::alt_2)) {
        PLATFORM.speaker().play_sound("button_wooden", 3);

        auto next = []() {
            auto encoded = encode_highscore_data();

            auto temp = allocate_dynamic<StringBuffer<700>>("temp-buf");
            auto fmt_buf = allocate_dynamic<StringBuffer<700>>("fmt-buf");
            for (auto& c : encoded) {
                temp->push_back(c);
            }

            make_format(*fmt_buf, "?d=%", temp->c_str());

            return make_scene<ConfiguredURLQRViewerScene>(
                "/scripts/config/uploadscore.lisp",
                fmt_buf->c_str(),
                SYSTR(score_upload_prompt_3)->c_str(),
                make_deferred_scene<HighscoresScene>(),
                ColorConstant::rich_black);
        };



        if (__login_token.valid_) {
            return next();
        }

        auto gettext = [next]() {
            auto receive = [next](const char* text) {
                __login_token.valid_ = true;
                if (strlen(text) not_eq 8) {
                    // sanity check
                    Platform::fatal("qr text logic error");
                }
                memcpy(__login_token.text_, text, 8);
                return next();
            };
            auto prompt = SYSTR(score_upload_enter_token);
            return make_scene<TextEntryScene>(prompt->c_str(), receive, 8, 8);
        };

        return make_scene<ConfiguredURLQRViewerScene>(
            "/scripts/config/login.lisp",
            "",
            SYSTR(score_upload_prompt_1)->c_str(),
            gettext);
    }

    if (APP.player().key_down(Key::action_1) or
        APP.player().key_down(Key::action_2)) {

        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update();
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(achievement);

                PLATFORM.screen().fade(1.f);

                const auto pg = title_screen_page_;

                auto next = make_deferred_scene<TitleScreenScene>(pg);
                return make_scene<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }

        return make_scene<TitleScreenScene>(title_screen_page_);
    }
    return null_scene();
}



HighscoresScene::Factory HighscoresScene::factory_;



} // namespace skyland
