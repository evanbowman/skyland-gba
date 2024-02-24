#pragma once


#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



class TextviewScene : public Scene
{
public:
    TextviewScene(const char* str)
        : str_(allocate_dynamic<StringBuffer<2000>>("textview-text")), page_(0)
    {
        *str_ = str;
    }


    Optional<DeferredScene> next_;


    void enter(Scene& prev) override
    {
        PLATFORM.screen().schedule_fade(
            1.f, ColorConstant::rich_black, {}, true, true);

        PLATFORM.load_overlay_texture("overlay_textview");

        auto screen_tiles = calc_screen_tiles();
        text_.emplace();

        page_number_.emplace(OverlayCoord{0, u8(screen_tiles.y - 1)});


        text_->assign(
            str_->c_str(),
            {1, 2},
            OverlayCoord{u8(screen_tiles.x - 2), u8(screen_tiles.y - 4)});

        PLATFORM.speaker().set_music_volume(15);
        PLATFORM.speaker().set_sounds_volume(13);
    }


    void exit(Scene& next) override
    {
        PLATFORM.screen().schedule_fade(1);
        PLATFORM.fill_overlay(0);
        text_.reset();
        page_number_.reset();
        PLATFORM.speaker().set_music_volume(
            Platform::Speaker::music_volume_max);
        PLATFORM.speaker().set_sounds_volume(
            Platform::Speaker::music_volume_max);
    }


    ScenePtr<Scene> update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition(Key::action_2)) {
            PLATFORM.screen().clear();
            PLATFORM.screen().schedule_fade(
                1, ColorConstant::rich_black, {}, true, true);
            PLATFORM.fill_overlay(0);
            PLATFORM.screen().display();
            PLATFORM.load_overlay_texture("overlay");
            if (next_) {
                return (*next_)();
            }
        }

        if (not PLATFORM.speaker().is_sound_playing("archivist")) {
            PLATFORM.speaker().play_sound("archivist", 9);
        }

        auto fc = custom_color(0xf7f7ef);

        switch (display_mode_) {
        case DisplayMode::after_transition:
            // NOTE: Loading a textview page for the first time results in a
            // large amount of lag on some systems, which effectively skips the
            // screen fade. The first load of a new page causes the platform to
            // load a bunch of glyphs into memory for the first time, which
            // means copying over texture memory, possibly adjusting a glyph
            // mapping table, etc. So we're just going to sit for one update
            // cycle, and let the huge delta pass over.
            display_mode_ = DisplayMode::fade_in;
            break;

        case DisplayMode::fade_in: {
            static const auto fade_duration = milliseconds(200);
            timer_ += delta;
            if (timer_ < fade_duration) {
                auto sstep = 1.f - smoothstep(0.f, fade_duration, timer_);
                PLATFORM.set_overlay_origin(0, -20 * sstep);
                PLATFORM.screen().schedule_fade(sstep, fc, {}, true, true);
            } else {
                timer_ = 0;
                PLATFORM.screen().schedule_fade(0.f);
                display_mode_ = DisplayMode::show;
            }
            break;
        }

        case DisplayMode::show:
            if (PLATFORM.keyboard().pressed<Key::right>()) {
                auto has_more_pages = [&]() -> bool {
                    return text_->parsed() not_eq utf8::len(str_->c_str());
                };

                if (has_more_pages()) {
                    PLATFORM.speaker().play_sound("page_flip", 3);
                    dir_ = false;
                    page_ += 1;
                    timer_ = 0;
                    display_mode_ = DisplayMode::fade_out;
                }

            } else if (PLATFORM.keyboard().pressed<Key::left>()) {
                if (page_ > 0) {
                    PLATFORM.speaker().play_sound("page_flip", 3);
                    dir_ = true;
                    page_ -= 1;
                    timer_ = 0;
                    display_mode_ = DisplayMode::fade_out;
                }
            }
            break;

        case DisplayMode::fade_out: {
            timer_ += delta;
            static const auto fade_duration = milliseconds(400);
            if (timer_ < fade_duration) {
                auto sstep = smoothstep(0.f, fade_duration, timer_);
                PLATFORM.set_overlay_origin((dir_ ? -32 : 32) * sstep, 0);
                PLATFORM.screen().schedule_fade(sstep, fc, {}, true, true);
            } else {
                timer_ = 0;
                PLATFORM.set_overlay_origin(0, 0);
                PLATFORM.screen().schedule_fade(1.f, fc, {}, true, true);
                display_mode_ = DisplayMode::transition;
            }
            break;
        }

        case DisplayMode::transition:
            repaint_page();
            PLATFORM.speaker().play_sound("open_book", 0);
            display_mode_ = DisplayMode::after_transition;
            break;
        }

        return null_scene();
    }

private:
    void repaint_page()
    {
        const auto size = text_->size();

        page_number_->erase();
        repaint_margin();
        page_number_->assign(page_ + 1);

        text_->assign(str_->c_str(), {1, 2}, size, page_ * (size.y / 2));
    }

    void repaint_margin()
    {
        u16 textview_margin_tile = get_whitespace_tile();

        PLATFORM.set_overlay_origin(0, 1);

        for (int x = 0; x < 32; ++x) {
            for (int y = 0; y < 32; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, textview_margin_tile);
            }
        }
    }


    static u16 get_whitespace_tile()
    {
        const auto mapping_info = locale_texture_map()(' ');
        if (mapping_info) {
            return PLATFORM.map_glyph(' ', *mapping_info);
        }
        return 0;
    }


    Microseconds timer_ = 0;

    enum class DisplayMode {
        fade_in,
        show,
        fade_out,
        transition,
        after_transition,
    } display_mode_ = DisplayMode::transition;

    Optional<TextView> text_;
    Optional<Text> page_number_;
    DynamicMemory<StringBuffer<2000>> str_;
    int page_;
    bool dir_ = false;
};



} // namespace skyland
