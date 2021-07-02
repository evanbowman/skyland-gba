#include "highscoresScene.hpp"
#include "skyland/skyland.hpp"
#include "localization.hpp"
#include "titleScreenScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



void HighscoresScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    const auto screen_tiles = calc_screen_tiles(pfrm);

    int metrics_y_offset_ = -5;

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


    // Score calculation
    int score = 0;
    score =
        1000 +
        app.persistent_data().coins_ +
        app.persistent_data().zone_ * 12000 +
        (app.persistent_data().current_map_location_.x + 1) * 800 +
        app.persistent_data().zone_ * app.persistent_data().zone_ * 1000;

    score -= 2 * app.persistent_data().total_seconds_.get() / 2;
    score -= 15 * app.persistent_data().total_pauses_.get();

    if (score < 0) {
        score = 0;
    }

    auto print_metric = [&](const char* str,
                            int num,
                            const char* suffix = "",
                            bool highlight = false) {
        print_metric_impl(str, to_string<20>(num), suffix, highlight);
    };

    auto& highscores = app.highscores_;

    for (auto& highscore : reversed(highscores.values_)) {
        if (highscore.get() < (u32)score) {
            highscore.set(score);
            break;
        }
    }

    std::sort(std::begin(highscores.values_),
              std::end(highscores.values_),
              [](const auto& lhs, const auto& rhs) {
                  return lhs.get() > rhs.get();
              });

    print_metric("score ", score);

    lines_.emplace_back(
            pfrm, Vec2<u8>{7, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

    lines_.back().append("-- highscores --");


    bool highlighted = false;
    for (int i = 0; i < Highscores::count; ++i) {
        StringBuffer<24> str;
        str += to_string<12>(i + 1);
        str += " ";
        bool highlight = not highlighted and highscores.values_[i].get() == (u32)score;
        print_metric(str.c_str(), highscores.values_[i].get(), "",
                     highlight);

        if (highlight) {
            highlighted = true;
        }
    }

    pfrm.write_save_data(&highscores, sizeof highscores, 0);
}



void HighscoresScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    lines_.clear();
}


ScenePtr<Scene> HighscoresScene::update(Platform& pfrm, App&, Microseconds)
{
    if (key_down<Key::action_1>(pfrm) or key_down<Key::action_2>(pfrm)) {
        return scene_pool::alloc<TitleScreenScene>();
    }
    return null_scene();
}



}
