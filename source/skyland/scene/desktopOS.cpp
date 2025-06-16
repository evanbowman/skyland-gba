///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "desktopOS.hpp"



#include "platform/flash_filesystem.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene/lispReplScene.hpp"
#include "skyland/scene/modules/fileBrowserModule.hpp"
#include "skyland/scene/modules/textEditorModule.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);


class DesktopOS;


DesktopOS* g_os_;



class DesktopOS : public Scene
{
public:
    class Clickable;


    using OptionCallback = void (*)();


    void register_clickable(Clickable* clk)
    {
        mem_->clickables_.push_back(clk);
    }


    void unregister_clickable(Clickable* clk)
    {
        auto it = mem_->clickables_.begin();
        for (; it not_eq mem_->clickables_.end();) {
            if (*it == clk) {
                it = mem_->clickables_.erase(it);
            } else {
                ++it;
            }
        }
    }


    class Clickable
    {
    public:
        Clickable(const HitBox::Dimension& dimension)
        {
            hitbox_.position_ = &position_;
            hitbox_.dimension_ = dimension;

            g_os_->register_clickable(this);
        }


        Vec2<Fixnum>& pos()
        {
            return position_;
        }


        Clickable(const Clickable&) = delete;


        ~Clickable()
        {
            g_os_->unregister_clickable(this);
        }


        virtual void on_hover()
        {
        }


        virtual void on_click()
        {
        }


        const HitBox& hitbox() const
        {
            return hitbox_;
        }


        void set_enabled(bool enabled)
        {
            enabled_ = enabled;
        }


        bool enabled() const
        {
            return enabled_;
        }


        bool shows_pointer()
        {
            return show_pointer_;
        }


    private:
        HitBox hitbox_;
        Vec2<Fixnum> position_;

    protected:
        bool enabled_ = true;
        bool show_pointer_ = false;
    };


    class DropdownMenu : public Clickable
    {
    public:
        DropdownMenu(const char* name, u8 x, u8 y)
            : Clickable({u8(strlen(name) * 8), 8, 0, 0}), name_(name), x_(x),
              y_(y)
        {
            pos().x = Fixnum::from_integer(x * 8);
            pos().y = Fixnum::from_integer(y * 8) - 7.0_fixed;
        }


        bool is_open() const
        {
            return open_;
        }


        void close()
        {
            open_ = false;
        }


        u8 name_len() const
        {
            return strlen(name_);
        }


        u8 x() const
        {
            return x_;
        }


        void repaint()
        {
            static constexpr const Text::OptColors sel_colors{
                {custom_color(0xffffff), custom_color(0x1276c5)}};

            Text::print(name_, {x_, y_}, open_ ? sel_colors : nullopt());

            if (open_) {

                u32 longest_opt = 0;
                for (auto& opt : options_) {
                    auto len = strlen(opt.name());
                    if (len > longest_opt) {
                        longest_opt = len;
                    }
                }

                u8 y = y_ + 1;
                for (auto& opt : options_) {
                    opt.set_enabled(true);
                    StringBuffer<32> tmp;
                    for (u32 x = 0; x < longest_opt; ++x) {
                        tmp.push_back('-');
                    }
                    Text::print(tmp.c_str(), {x_, y});
                    tmp = opt.name();
                    while (tmp.length() not_eq longest_opt) {
                        tmp += " ";
                    }
                    Text::print(tmp.c_str(), {x_, u8(y + 1)});
                    y += 2;
                }
            } else {
                for (auto& opt : options_) {
                    opt.set_enabled(false);
                }
            }
        }


        void on_hover() override;


        void on_click() override
        {
            open_ = not open_;
            g_os_->repaint_windows();
        }


        class Option : public Clickable
        {
        public:
            Option(const char* name, OptionCallback cb, u8 x, u8 y)
                : Clickable({u8(strlen(name) * 8), 8, 0, 0}), name_(name),
                  cb_(cb)
            {
                pos().x = Fixnum::from_integer(x * 8);
                pos().y = Fixnum::from_integer(y * 8);
                show_pointer_ = true;
            }

            void on_click() override
            {
                cb_();
            }

            const char* name() const
            {
                return name_;
            }

        private:
            const char* name_;
            OptionCallback cb_;
        };


        void add_option(const char* name, OptionCallback cb)
        {
            options_.emplace_back(
                name, cb, x_, u8(y_ + 1 + options_.size() * 2));
        }


        Buffer<Option, 5>& opts()
        {
            return options_;
        }


    private:
        const char* name_;
        Buffer<Option, 5> options_;
        u8 x_;
        u8 y_;
        bool open_ = false;
    };


    Buffer<DropdownMenu, 4>& dropdown_menus()
    {
        return mem_->menu_bar_opts_;
    }


    class Window;
    using OnOpenCallback = void (*)(Window* win, int param);


    class DockIcon : public Clickable
    {
    public:
        DockIcon(const char* name, u8 icon_gfx, Vec2<Fixnum> pos)
            : Clickable({16, 16, 0, 0}), name_(name), icon_gfx_(icon_gfx)
        {
            this->pos() = pos;
            y_pos_ = this->pos().y.as_integer();
        }


        void set_minimized()
        {
            g_os_->minimize_window(name());
            g_os_->repaint_windows();
        }


        void set_open()
        {
            state_ = State::open;
            if (icon_gfx_ < 21) {
                icon_gfx_ += 8;
            }
            g_os_->make_window(this);
        }


        OnOpenCallback on_open_callback_ = [](Window*, int) {};
        int on_open_param_;


        void set_on_open(OnOpenCallback cb, int param)
        {
            on_open_callback_ = cb;
            on_open_param_ = param;
        }


        void set_closed()
        {
            state_ = State::closed;
            if (icon_gfx_ >= 21) {
                icon_gfx_ -= 8;
            }
            g_os_->close_window(name_);
        }


        void update()
        {
            const uint8_t dock_animation[32] = {
                0,   32,  60,  84,  104, 120, 133, 144, 153, 160, 166,
                171, 175, 178, 180, 182, 183, 182, 180, 177, 173, 168,
                162, 155, 147, 138, 128, 117, 105, 92,  78,  63};

            switch (state_) {
            case State::closed:
                break;

            case State::opening:
                pos().y = Fixnum::from_integer(
                    y_pos_ - dock_animation[anim_cyc_ % 32] / 24);
                anim_cyc_ += 1;
                if (anim_cyc_ == 128 - 32) {
                    anim_cyc_ = 0;
                    pos().y = y_pos_;
                    set_open();
                }
                break;

            case State::open:
                break;
            }
        }


        void on_click() override
        {
            if (icon_gfx_ == 29) {
                g_os_->boot_rom();
                return;
            }

            switch (state_) {
            case State::closed:
                state_ = State::opening;
                break;

            case State::opening:
                break;

            case State::open:
                g_os_->focus_window(name());
                break;
            }
        }


        void on_hover() override
        {
            if (state_ == State::opening) {
                bool had_hint = (bool)g_os_->hint_label_;
                g_os_->hint_label_.reset();
                if (had_hint) {
                    g_os_->repaint_windows();
                }
                return;
            }
            u8 x = pos().x.as_integer() / 8;
            int offset = strlen(name_) / 2;
            x -= (offset - 2);
            OverlayCoord dest{x, 17};
            if (g_os_->hint_label_ and g_os_->hint_label_->coord() == dest) {
                return;
            }
            g_os_->hint_label_.emplace(name_, dest);
        }


        u8 icon_gfx() const
        {
            return icon_gfx_;
        }


        const char* name() const
        {
            return name_;
        }


    private:
        const char* name_;
        u8 icon_gfx_;
        u8 anim_cyc_ = 0;
        u8 y_pos_ = 0;

        enum class State : u8 {
            closed,
            opening,
            open,
        } state_ = State::closed;
    };


    void open_application(const char* app_name, OnOpenCallback cb, int param)
    {
        if (auto win = get_window(app_name)) {
            cb(win, param);
            g_os_->focus_window(app_name);
            return;
        }
        for (auto& ico : mem_->dock_icons_) {
            if (str_eq(ico.name(), app_name)) {
                ico.set_closed();
                ico.on_click();
                ico.set_on_open(cb, param);
            }
        }
    }


    class Window
    {
    public:
        bool minimized_ = false;


        virtual StringBuffer<26> heading()
        {
            return name();
        }


        void minimize()
        {
            minimized_ = true;
            set_focus(false);
            pkg_->set_minimized();
        }


        class CloseButton : public Clickable
        {
        public:
            CloseButton(Window* window)
                : Clickable({7, 7, -1, -1}), window_(window)
            {
                this->pos().x = 3.0_fixed;
                this->pos().y = 12.0_fixed;
            }

            void on_click() override
            {
                window_->close();
            }


        private:
            Window* window_;
        };


        class MinimizeButton : public Clickable
        {
        public:
            MinimizeButton(Window* window)
                : Clickable({7, 7, -1, -1}), window_(window)
            {
                this->pos().x = 14.0_fixed;
                this->pos().y = 12.0_fixed;
            }

            void on_click() override
            {
                window_->minimize();
            }

        private:
            Window* window_;
        };


        Window(DockIcon* application) : pkg_(application), close_btn_(this)
        // minimize_btn_(this)
        {
        }


        virtual void destroy()
        {
        }


        virtual void update()
        {
        }


        void close()
        {
            pkg_->set_closed();
        }


        const char* name() const
        {
            return pkg_->name();
        }


        virtual void repaint()
        {
            auto head = heading();
            auto name_len = head.length();
            u8 mg = centered_text_margins(name_len);
            for (int x = 0; x < 30; ++x) {
                for (int y = 2; y < 17; ++y) {
                    if (y == 2) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 89);
                    } else if (y == 3) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 91);
                    } else {
                        PLATFORM.set_tile(Layer::overlay, x, y, 82);
                    }
                }
            }

            static constexpr const Text::OptColors heading_colors{
                {custom_color(0x717199), custom_color(0xeaeef3)}};

            Text::print(head.c_str(), {mg, 3}, heading_colors);
            PLATFORM.set_tile(Layer::overlay, 0, 2, 87);
            PLATFORM.set_tile(Layer::overlay, 0, 3, 92);
            PLATFORM.set_tile(Layer::overlay, 1, 2, 94);
            // PLATFORM.set_tile(Layer::overlay, 2, 2, 95);
            // PLATFORM.set_tile(Layer::overlay, 2, 3, 96);
            PLATFORM.set_tile(Layer::overlay, 29, 2, 88);
        }


        virtual void set_focus(bool focused)
        {
            close_btn_.set_enabled(focused);
            // minimize_btn_.set_enabled(focused);

            if (focused) {
                g_os_->clear_dropdown_menus();
                build_menu_bar_opts();
            }
        }


        virtual void build_menu_bar_opts()
        {
        }


    private:
        DockIcon* pkg_;
        CloseButton close_btn_;
        // MinimizeButton minimize_btn_;
    };



    class SystemMonitorWindow : public Window
    {
    public:
        SystemMonitorWindow(DockIcon* application) : Window(application)
        {
        }


        enum class View : u8 {
            main_page,
        } view_ = View::main_page;


        void repaint() override
        {
            Window::repaint();
            int sbr_used = scratch_buffers_in_use();
            int sbr_total =
                scratch_buffers_in_use() + scratch_buffers_remaining();
            Text::print(format("SBR:[%/%]", sbr_used, sbr_total).c_str(),
                        {1, 5});
            auto lisp_mem = lisp::value_pool_info();
            Text::print(format("lisp:[%/%]",
                               lisp_mem.first,
                               lisp_mem.first + lisp_mem.second)
                            .c_str(),
                        {1, 7});
            int ent_used = 0;
            int ent_total = 0;
            for (auto& pl : globals().entity_pools_.pools()) {
                ent_used += pl->pooled_element_count() - pl->pooled_element_remaining();
                ent_total += pl->pooled_element_count();
            }
            Text::print(format("entity:[%/%]", ent_used, ent_total).c_str(),
                        {13, 5});

            auto stat = flash_filesystem::statistics();
            Text::print(format("disk:[%/%]", stat.bytes_used_ / 1024,
                       (stat.bytes_used_ + stat.bytes_available_) / 1024).c_str(), {1, 9});

            u32 mstack = 0;
            if (auto s = PLATFORM.get_extensions().get_stack_usage) {
                mstack = s();
            }

            Text::print(format("stk: [%]", mstack).c_str(), {14, 9});

        }


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("System Monitor")) {
                        win->close();
                    }
                });
            }

            if (auto view_menu = g_os_->insert_dropdown_menu("View")) {
                (void)view_menu;
            }
        }
    };



    class SeekerWindow : public Window
    {
    public:
        SeekerWindow(DockIcon* application) : Window(application)
        {
            impl_.emplace();
            impl_->gui_mode_ = true;
            impl_->enter(*impl_);
        }


        void set_focus(bool focused) override
        {
            Window::set_focus(focused);
            active_ = focused;

            for (auto& cl : clickables_) {
                cl.set_enabled(focused);
            }
        }


        void update() override
        {
            auto delta = milliseconds(16); // fixme someday...

            if (active_) {
                impl_->update(delta);
            }

            if (double_click_timer_ > 0) {
                double_click_timer_ -= delta;
            }
        }


        StringBuffer<26> heading() override
        {
            auto base = Window::heading();
            base += ": ";
            if (impl_->selected_filesystem_ ==
                FileBrowserModule::SelectedFilesystem::none) {
                base += "mount drive";
            } else {
                base += impl_->cwd().c_str();
            }
            return base;
        }


        void repaint() override
        {
            Window::repaint();
            impl_->repaint();

            bool alternate = true;

            static constexpr const Text::OptColors alt_colors{
                {custom_color(0x212194), custom_color(0xeaeef3)}};

            clickables_.clear();


            auto& names = impl_->get_cwd_names();
            u32 i = 0;
            for (; i < names.size(); ++i) {
                u8 y = u8(4 + i);
                if (y > 16) {
                    break;
                }
                clickables_.emplace_back(i);
                StringBuffer<64> nm = "  ";
                nm += names[i].c_str();
                while (nm.length() < 30) {
                    nm.push_back(' ');
                }
                if ((s8)i == selected_file_) {
                    static constexpr const Text::OptColors sel_colors{
                        {custom_color(0xffffff), custom_color(0x1276c5)}};
                    Text::print(nm.c_str(), {0, y}, sel_colors);
                } else if (alternate) {
                    Text::print(nm.c_str(), {0, y});
                } else {
                    Text::print(nm.c_str(), {0, y}, alt_colors);
                }
                alternate = not alternate;
            }
            for (; i < 12; ++i) {
                StringBuffer<31> nm(' ', 31);
                u8 y = u8(4 + i);
                if (alternate) {
                    Text::print(nm.c_str(), {0, y});
                } else {
                    Text::print(nm.c_str(), {0, y}, alt_colors);
                }
                alternate = not alternate;
            }
        }


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                if (selected_file_ > -1) {
                    auto ent = impl_->select_entry(selected_file_, false);
                    if (ent[ent.length() - 1] not_eq '/') {
                        file_menu->add_option("delete", [] {
                            if (auto win = (SeekerWindow*)g_os_->get_window(
                                    "Seeker")) {
                                auto index = win->selected_file_;
                                auto ent =
                                    win->impl_->select_entry(index, false);
                                flash_filesystem::unlink_file(ent.c_str());
                            }
                        });
                    }
                }

                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("Seeker")) {
                        win->close();
                    }
                });
            }

            if (auto go_menu = g_os_->insert_dropdown_menu("Go")) {
                go_menu->add_option("back", [] {
                    if (auto win = g_os_->get_window("Seeker")) {
                        ((SeekerWindow*)win)->impl_->backout();
                        ((SeekerWindow*)win)->selected_file_ = -1;
                        g_os_->repaint_windows();
                    }
                });
            }
        }


        void select_option(u8 index)
        {
            auto ent = impl_->select_entry(index, double_click_timer_ > 0);

            bool opened_app = false;

            if (not ent.empty()) {
                if (double_click_timer_ > 0 and click_index_ == index) {
                    using MatchStr = StringBuffer<6>;
                    if (ends_with(ent, MatchStr(".dat")) or
                        ends_with(ent, MatchStr(".bin")) or
                        ends_with(ent, MatchStr(".raw"))) {
                        // Don't open with TextEdit
                    } else {
                        g_os_->open_application(
                            "TextEdit",
                            [](Window* w, int index) {
                                if (auto sw = (SeekerWindow*)g_os_->get_window(
                                        "Seeker")) {
                                    auto ent =
                                        sw->impl_->select_entry(index, false);
                                    bool is_rom =
                                        sw->impl_->selected_filesystem_ ==
                                        FileBrowserModule::rom;
                                    ((TextEditWindow*)w)
                                        ->open_file(ent.c_str(), is_rom);
                                }
                            },
                            index);
                        opened_app = true;
                    }
                }
            }

            if (double_click_timer_ > 0) {
                selected_file_ = -1;
                click_index_ = -1;
                double_click_timer_ = 0;
            } else {
                selected_file_ = index;
                click_index_ = index;
                double_click_timer_ = milliseconds(500);
            }

            if (not opened_app) {
                g_os_->clear_dropdown_menus();
                build_menu_bar_opts();
            }
            g_os_->repaint_windows();
        }


        class FileExplorerOption : public Clickable
        {
        public:
            FileExplorerOption(int index)
                : Clickable({u8(8 * 28), 7}), index_(index)
            {
                Vec2<Fixnum> pos{16.0_fixed, 25.0_fixed};
                pos.y += Fixnum::from_integer(8 * index);
                this->pos() = pos;
                show_pointer_ = true;
            }


            void on_click() override
            {
                if (auto sw = (SeekerWindow*)g_os_->get_window("Seeker")) {
                    sw->select_option(index_);
                }
            }

            u8 index_ = 0;
        };


        Optional<FileBrowserModule> impl_;
        Buffer<FileExplorerOption, 13> clickables_;
        Time double_click_timer_ = 0;
        bool active_ = true;
        s8 selected_file_ = -1;
        s8 click_index_ = -1;
    };



    class SkyTunesWindow : public Window
    {
    public:
        SkyTunesWindow(DockIcon* application) : Window(application)
        {
            u8 i = 0;
            PLATFORM.walk_filesystem([this, &i](const char* path) {
                if (auto rest = starts_with("/scripts/data/music/",
                                            StringBuffer<96>(path))) {
                    StringBuffer<48> temp;
                    while (*rest not_eq '.') {
                        temp.push_back(*rest);
                        ++rest;
                    }
                    Vec2<Fixnum> pos;
                    pos.x = Fixnum::from_integer(16);
                    pos.y = Fixnum::from_integer((33 - 8) + i * 8);
                    music_opts_.emplace_back(temp.c_str(),
                                             HitBox::Dimension{20 * 8, 7, 0, 0},
                                             pos,
                                             this);
                    music_names_.push_back(temp.c_str());
                    ++i;
                }
            });
        }


        SkyTunesWindow(const SkyTunesWindow&) = delete;


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("SkyTunes")) {
                        win->close();
                    }
                });
            }

            if (auto ctrl_menu = g_os_->insert_dropdown_menu("Control")) {
                ctrl_menu->add_option("stop music", [] {
                    PLATFORM.speaker().stop_music();
                    if (auto win = g_os_->get_window("SkyTunes")) {
                        ((SkyTunesWindow*)win)->selected_track_ = 255;
                    }
                });
            }
        }


        void destroy() override
        {
            this->~SkyTunesWindow();
        }


        ~SkyTunesWindow()
        {
            PLATFORM.speaker().stop_music();
        }


        void repaint() override
        {
            Window::repaint();

            bool alternate = true;

            static constexpr const Text::OptColors alt_colors{
                {custom_color(0x212194), custom_color(0xeaeef3)}};

            u32 i = 0;
            for (; i < music_names_.size(); ++i) {
                StringBuffer<48> nm = "  ";
                nm += music_names_[i].c_str();
                while (nm.length() < 30) {
                    nm.push_back(' ');
                }
                u8 y = u8(4 + i);
                if (alternate) {
                    Text::print(nm.c_str(), {0, y});
                    if (selected_track_ == i) {
                        PLATFORM.set_tile(Layer::overlay, 0, y, 98);
                    }
                } else {
                    Text::print(nm.c_str(), {0, y}, alt_colors);
                    if (selected_track_ == i) {
                        PLATFORM.set_tile(Layer::overlay, 0, y, 99);
                    }
                }
                alternate = not alternate;
            }
            for (; i < 12; ++i) {
                StringBuffer<31> nm(' ', 31);
                u8 y = u8(4 + i);
                if (alternate) {
                    Text::print(nm.c_str(), {0, y});
                } else {
                    Text::print(nm.c_str(), {0, y}, alt_colors);
                }
                alternate = not alternate;
            }
        }

        void set_focus(bool focused) override
        {
            Window::set_focus(focused);
            for (auto& opt : music_opts_) {
                opt.set_enabled(focused);
            }
        }

    private:
        class MusicTrack : public Clickable
        {
        public:
            MusicTrack(const char* name,
                       const HitBox::Dimension& dimension,
                       const Vec2<Fixnum>& pos,
                       SkyTunesWindow* window)
                : Clickable(dimension), name_(name), window_(window)
            {
                this->pos() = pos;
                show_pointer_ = true;
            }

            void on_click() override
            {
                auto str = name_;
                str += ".raw";
                PLATFORM.speaker().stream_music(str.c_str(), 0);
                window_->select(name_.c_str());
                g_os_->repaint_windows();
            }

        private:
            StringBuffer<48> name_;
            SkyTunesWindow* window_;
        };

        void select(const char* name)
        {
            for (u32 i = 0; i < music_names_.size(); ++i) {
                if (music_names_[i] == name) {
                    selected_track_ = i;
                    break;
                }
            }
        }

        Buffer<MusicTrack, 10> music_opts_;
        Buffer<StringBuffer<48>, 10> music_names_;

    public:
        u8 selected_track_ = 255;
    };


    class ExplorerWindow : public Window
    {
    public:
        using Window::Window;


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("Compass")) {
                        win->close();
                    }
                });
            }
        }


        void repaint() override
        {
            Window::repaint();

            Text::print("You are not connected", {4, 8});
            Text::print("to the internet.", {4, 10});
        }
    };


    class TextEditWindow : public Window
    {
    public:
        class WindowFocusCapture : public Clickable
        {
        public:
            WindowFocusCapture(TextEditWindow* window)
                : Clickable({240, 104, 0, 0}), window_(window)
            {
                this->pos().x = 0.0_fixed;
                this->pos().y = 25.0_fixed;
                show_pointer_ = true;
            }

            void on_click() override
            {
                g_os_->capture_focus(true);
                window_->interactive_ = true;
                window_->ignore_click();
                g_os_->repaint_windows();
            }

        private:
            TextEditWindow* window_;
        };


        void push_recent()
        {
            if (impl_) {
                if (recents_.full()) {
                    recents_.erase(recents_.begin());
                }
                auto new_entry = impl_->file_path();
                if (new_entry == "/") {
                    new_entry = "*syslog*";
                }
                for (auto& r : recents_) {
                    if (r.first == new_entry.c_str()) {
                        // Already exists, move to end
                        std::swap(recents_.back(), r);
                        return;
                    }
                }
                auto rom_fs =
                    impl_->which_fs() == TextEditorModule::FileSystem::rom;
                recents_.push_back({new_entry.c_str(), rom_fs});
                g_os_->clear_dropdown_menus();
                build_menu_bar_opts();
            }
        }


        StringBuffer<26> heading() override
        {
            auto base = Window::heading();
            base += ": ";
            if (impl_) {
                base += impl_->extract_filename(impl_->file_path().c_str());
            }
            return base;
        }


        void load_file(const char* path, bool rom)
        {
            if (str_eq(path, "*syslog*")) {
                UserContext ctx;
                impl_.emplace(std::move(ctx));
            } else {
                UserContext ctx;
                auto syntax = TextEditorModule::SyntaxMode::plain_text;
                impl_.emplace(std::move(ctx),
                              path,
                              syntax,
                              TextEditorModule::FileMode::update,
                              rom ? TextEditorModule::FileSystem::rom
                                  : TextEditorModule::FileSystem::sram);
            }

            impl_->gui_mode_ = true;
            impl_->enter(*impl_);
            g_os_->clear_dropdown_menus();
            build_menu_bar_opts();
        }


        void open_file(const char* path, bool rom)
        {
            push_recent();
            if (impl_) {
                impl_->save();
            }
            load_file(path, rom);
        }


        TextEditWindow(DockIcon* application)
            : Window(application), capture_(this)
        {
            open_file("/scratch.txt", false);
        }


        static void load_recent_file(int f)
        {
            if (auto win = (TextEditWindow*)g_os_->get_window("TextEdit")) {
                auto rec = win->recents_[f];
                win->open_file(rec.first.c_str(), rec.second);
            }
        }


        void build_menu_bar_opts() override
        {
            if (auto recent_menu = g_os_->insert_dropdown_menu("Recents")) {
                OptionCallback cbs[] = {
                    []() { load_recent_file(0); },
                    []() { load_recent_file(1); },
                    []() { load_recent_file(2); },
                    []() { load_recent_file(3); },
                    []() { load_recent_file(4); },
                };
                for (int i = recents_.size() - 1; i > -1; --i) {
                    recent_menu->add_option(recents_[i].first.c_str(), cbs[i]);
                }
            }

            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("save", [] {
                    if (auto win =
                            (TextEditWindow*)g_os_->get_window("TextEdit")) {
                        win->impl_->save();
                    }
                });
                file_menu->add_option("lisp eval",
                                      eval_current_textedit_script);

                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("TextEdit")) {
                        win->close();
                    }
                });
            }

            if (auto file_menu = g_os_->insert_dropdown_menu("Edit")) {
                file_menu->add_option("copy", [] {
                    g_os_->clipboard_.emplace("clipboard");
                    if (auto win = g_os_->get_window("TextEdit")) {
                        ((TextEditWindow*)win)
                            ->impl_->copy_selected(*g_os_->clipboard_);
                    }
                });
                file_menu->add_option("paste", [] {
                    if (not g_os_->clipboard_) {
                        return;
                    }
                    if (auto win = g_os_->get_window("TextEdit")) {
                        ((TextEditWindow*)win)
                            ->impl_->paste(*g_os_->clipboard_);
                    }
                });
            }
        }


        void set_focus(bool focused) override
        {
            Window::set_focus(focused);
            capture_.set_enabled(focused);
        }


        void update() override
        {
            if (APP.player().key_down(Key::action_2)) {
                if (interactive_ and impl_ and
                    impl_->mode_ == TextEditorModule::Mode::nav and
                    not APP.player().key_pressed(Key::alt_1)) {
                    interactive_ = false;
                    g_os_->capture_focus(false);
                    g_os_->repaint_windows();
                }
            }

            if (interactive_) {
                if (ignore_click_ and APP.player().key_down(Key::action_1)) {
                    ignore_click_--;
                } else {
                    if (impl_) {
                        impl_->update(milliseconds(16));
                    }
                }
            }
        }


        void ignore_click()
        {
            ignore_click_++;
        }


        void repaint() override
        {
            Window::repaint();

            for (int x = 0; x < 30; ++x) {
                for (int y = 4; y < 17; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 97);
                }
            }
            if (impl_) {
                impl_->repaint();
            }
        }

        Optional<TextEditorModule> impl_;
        WindowFocusCapture capture_;
        int ignore_click_ = 0;
        bool has_init_ = false;
        bool interactive_ = false;
        bool syslog_mode_ = false;
        Buffer<std::pair<StringBuffer<64>, bool>, 5> recents_;
    };


    Optional<Vector<char>> clipboard_;


    class LispWindow : public Window
    {
    public:
        class WindowFocusCapture : public Clickable
        {
        public:
            WindowFocusCapture(LispWindow* window)
                : Clickable({240, 104, 0, 0}), window_(window)
            {
                this->pos().x = 0.0_fixed;
                this->pos().y = 25.0_fixed;
                show_pointer_ = true;
            }

            void on_click() override
            {
                g_os_->capture_focus(true);
                window_->interactive_ = true;
                window_->ignore_click();
                g_os_->repaint_windows();
            }

        private:
            LispWindow* window_;
        };

        LispWindow(DockIcon* application) : Window(application), capture_(this)
        {
            impl_.emplace();
            impl_->gui_mode_ = true;
        }


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("Lisp")) {
                        win->close();
                    }
                });
            }

            if (auto help_menu = g_os_->insert_dropdown_menu("Help")) {
                help_menu->add_option("view syslog", [] {
                    g_os_->open_application(
                        "TextEdit",
                        [](Window* window, int param) {
                            ((TextEditWindow*)window)
                                ->open_file("*syslog*", true);
                        },
                        0);
                });

                help_menu->add_option("api docs", [] {
                    g_os_->open_application(
                        "TextEdit",
                        [](Window* window, int param) {
                            ((TextEditWindow*)window)
                                ->open_file("/help/api.txt", true);
                        },
                        0);
                });

                help_menu->add_option("builtin docs", [] {
                    g_os_->open_application(
                        "TextEdit",
                        [](Window* window, int param) {
                            ((TextEditWindow*)window)
                                ->open_file("/help/lisp_builtins.txt", true);
                        },
                        0);
                });

                help_menu->add_option("skyland lisp?", [] {
                    g_os_->open_application(
                        "TextEdit",
                        [](Window* window, int param) {
                            ((TextEditWindow*)window)
                                ->open_file("/help/skyland_lisp.txt", true);
                        },
                        0);
                });
            }
        }


        void set_focus(bool focused) override
        {
            Window::set_focus(focused);
            capture_.set_enabled(focused);
        }


        void update() override
        {
            if (APP.player().key_down(Key::action_2)) {
                if (interactive_ and impl_->entry_empty()) {
                    interactive_ = false;
                    g_os_->capture_focus(false);
                    g_os_->repaint_windows();
                }
            }

            if (interactive_) {
                if (ignore_click_ and APP.player().key_down(Key::action_1)) {
                    ignore_click_--;
                } else {
                    impl_->update(milliseconds(16));
                }

                if (impl_->clobbered_tiles_) {
                    impl_->clobbered_tiles_ = false;
                    g_os_->repaint_windows();
                }
            }
        }


        void ignore_click()
        {
            ignore_click_++;
        }


        void repaint() override
        {
            Window::repaint();

            for (int x = 0; x < 30; ++x) {
                for (int y = 4; y < 17; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 97);
                }
            }
            if (not has_init_) {
                impl_->enter(*impl_);
                has_init_ = true;
            }
            impl_->repaint(interactive_);
        }

        Optional<LispReplScene> impl_;
        WindowFocusCapture capture_;
        int ignore_click_ = 0;
        bool has_init_ = false;
        bool interactive_ = false;
    };


    static void eval_current_textedit_script()
    {
        if (auto win = (TextEditWindow*)g_os_->get_window("TextEdit")) {
            win->impl_->save();
            auto path = win->impl_->file_path();

            bool stored = false;
            auto store_result = [&stored](lisp::Value& val) {
                lisp::_Printer<Vector<char>> p;
                lisp::format(&val, p);

                flash_filesystem::store_file_data(
                    "/eval_output.txt", p.data_, {.use_compression_ = true});
                stored = true;
            };

            auto lv = APP.invoke_script(path.c_str(), false, store_result);
            if (not stored) {
                store_result(*lv);
            }

            win->open_file("/eval_output.txt", false);
        }
    }



    class mGBAWindow : public Window
    {
    public:
        mGBAWindow(DockIcon* application)
            : Window(application), launch_btn_(this)
        {
        }


        void build_menu_bar_opts() override
        {
            if (auto file_menu = g_os_->insert_dropdown_menu("File")) {
                file_menu->add_option("close", [] {
                    if (auto win = g_os_->get_window("mGBA")) {
                        win->close();
                    }
                });
            }
        }


        void repaint() override
        {
            Window::repaint();

            Text::print("Recent Games:", {2, 5});

            Text::print("- Skyland.gba", {3, 7});

            static constexpr const Text::OptColors btn_colors{
                {custom_color(0xeaeef3), custom_color(0x2d9773)}};

            Text::print("launch", {21, 7}, btn_colors);
        }


        class LaunchButton : public Clickable
        {
        public:
            LaunchButton(Window* window)
                : Clickable({8 * 6, 8, 0, 0}), window_(window)
            {
                this->pos().x = 168.0_fixed;
                this->pos().y = 49.0_fixed;
                show_pointer_ = true;
            }

            void on_click() override
            {
                g_os_->boot_rom();
            }


        private:
            Window* window_;
        };

    private:
        LaunchButton launch_btn_;
    };



    void boot_rom()
    {
        if (mem_->resume_) {
            mem_->resume_flag_ = true;
        } else {
            PLATFORM_EXTENSION(restart);
        }
    }



    void draw_menu_bar()
    {
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 0, 82);
            PLATFORM.set_tile(Layer::overlay, x, 1, 82);
        }
        PLATFORM.set_overlay_origin(0, 7);

        // TODO: menu bar opts...
    }


    void draw_dock()
    {
        for (int x = 0; x < 24; ++x) {
            for (int y = 0; y < 3; ++y) {
                PLATFORM.set_tile(Layer::overlay, x + 3, y + 18, 83);
            }
        }
        PLATFORM.set_tile(Layer::overlay, 3, 18, 85); // corners
        PLATFORM.set_tile(Layer::overlay, 26, 18, 86);
    }


    DesktopOS(Optional<DeferredScene> resume)
        : mem_(allocate_dynamic<Mem>("desktop-gui"))
    {
        mem_->resume_ = resume;
        g_os_ = this;
    }


    void exit(Scene&) override
    {
        mem_->clickables_.clear();
        mem_->dock_icons_.clear();
        mem_->windows_.clear();
        mem_->menu_bar_opts_.clear();
        PLATFORM.screen().set_shader(APP.environment().shader());
    }


    void enter(Scene&) override
    {
        PLATFORM.screen().set_shader(passthrough_shader);

        PLATFORM.screen().schedule_fade(
            1.f, ColorConstant::rich_black, true, true);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.load_sprite_texture("spritesheet_os");

        PLATFORM.speaker().stop_music();
        PLATFORM.load_tile0_texture("wallpaper_flattened");
        for (int x = 0; x < 32; ++x) {
            for (int y = 0; y < 32; ++y) {
                PLATFORM.set_raw_tile(Layer::map_0, x, y, 0);
                PLATFORM.set_raw_tile(Layer::map_1, x, y, 0);
            }
        }

        for (int x = 0; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_raw_tile(Layer::map_0, x, y, 32);
                PLATFORM.set_raw_tile(Layer::map_1, x, y, 32);
            }
        }
        __draw_image(1, 0, 1, 30, 16, Layer::map_0);
        PLATFORM.screen().schedule_fade(0);

        PLATFORM.fill_overlay(0);
        PLATFORM.load_overlay_texture("overlay_os");

        draw_menu_bar();
        draw_dock();

        PLATFORM.set_scroll(Layer::map_0_ext, 0, 0);

        PLATFORM.screen().set_view({});
        PLATFORM.screen().schedule_fade(0.5f);
        PLATFORM.screen().schedule_fade(0);

        cursor_ = {116.0_fixed, 76.0_fixed};

        auto spacing = 23.0_fixed;

        Vec2<Fixnum> icon_pos;
        icon_pos.x = 30.0_fixed;
        icon_pos.y = 139.0_fixed;
        mem_->dock_icons_.emplace_back("Seeker", 13, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back("Compass", 16, icon_pos);
        icon_pos.x += spacing + 1.0_fixed;
        mem_->dock_icons_.emplace_back("SkyTunes", 15, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back("TextEdit", 18, icon_pos);
        icon_pos.x += spacing + 1.0_fixed;
        mem_->dock_icons_.emplace_back("Lisp", 17, icon_pos);
        icon_pos.x += spacing;
        if (mem_->resume_) {
            mem_->dock_icons_.emplace_back("Skyland", 29, icon_pos);
        } else {
            mem_->dock_icons_.emplace_back("mGBA", 19, icon_pos);
        }
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back("System Monitor", 14, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back("...", 14, icon_pos);
    }


    Window* get_window(const char* name)
    {
        for (auto& window : mem_->windows_) {
            if (str_eq(name, window->name())) {
                return &*window;
            }
        }
        return nullptr;
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        constexpr auto fade_duration = milliseconds(1200);
        if (mem_->fade_timer_ < fade_duration) {
            mem_->fade_timer_ += delta;
            const auto amount =
                1.f - smoothstep(0.f, fade_duration, mem_->fade_timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        } else {
            PLATFORM.screen().schedule_fade(0);
        }

        if (mem_->resume_flag_ and mem_->resume_) {
            return (*mem_->resume_)();
        }

        if (not cursor_captured_) {
            if (player().key_pressed(Key::down)) {
                if (cursor_.y < 159.0_fixed) {
                    cursor_.y += 1.3_fixed;
                }
            }
            if (player().key_pressed(Key::up)) {
                if (cursor_.y > 0.0_fixed) {
                    cursor_.y -= 1.3_fixed;
                }
            }

            if (player().key_pressed(Key::right)) {
                if (cursor_.x < 239.0_fixed) {
                    cursor_.x += 1.3_fixed;
                }
            }
            if (player().key_pressed(Key::left)) {
                if (cursor_.x > 0.0_fixed) {
                    cursor_.x -= 1.3_fixed;
                }
            }

            HitBox cursor_hb;
            cursor_hb.dimension_ = {1, 1, 0, 0};
            cursor_hb.position_ = &cursor_;

            DropdownMenu* dropdown_open = nullptr;
            for (auto& opt : mem_->menu_bar_opts_) {
                if (opt.is_open()) {
                    dropdown_open = &opt;
                }
            }

            bool has_hover = false;
            pointer_ = false;

            auto update_hover = [&](auto& clickables) {
                for (auto& cl : reversed(clickables)) {
                    if (cl->enabled() and cl->hitbox().overlapping(cursor_hb)) {
                        if (cl->shows_pointer()) {
                            pointer_ = true;
                        }
                        cl->on_hover();
                        has_hover = true;
                    }
                }
            };

            if (dropdown_open) {
                Buffer<Clickable*, 10> opts;
                for (auto& dropdown : mem_->menu_bar_opts_) {
                    opts.push_back(&dropdown);
                    for (auto& opt : dropdown.opts()) {
                        opts.push_back(&opt);
                    }
                }
                update_hover(opts);
            } else {
                update_hover(mem_->clickables_);
            }

            if (not has_hover) {
                if (hint_label_) {
                    hint_label_.reset();
                }
            }

            if (player().key_down(Key::action_1)) {
                click();
            }
        }

        for (auto& ico : mem_->dock_icons_) {
            ico.update();
        }

        for (auto& win : mem_->windows_) {
            win->update();
        }

        return null_scene();
    }


    void click()
    {
        HitBox cursor_hb;
        cursor_hb.dimension_ = {1, 1, 0, 0};
        cursor_hb.position_ = &cursor_;

        DropdownMenu* dropdown_open = nullptr;
        for (auto& opt : mem_->menu_bar_opts_) {
            if (opt.is_open() and not cursor_hb.overlapping(opt.hitbox())) {
                opt.close();
                dropdown_open = &opt;
            }
        }

        if (dropdown_open) {
            for (auto& clickable : dropdown_open->opts()) {
                if (clickable.hitbox().overlapping(cursor_hb)) {
                    clickable.on_click();
                    break;
                }
            }
        } else {
            for (auto& clickable : reversed(mem_->clickables_)) {
                if (clickable->enabled() and
                    clickable->hitbox().overlapping(cursor_hb)) {
                    clickable->on_click();
                    break;
                }
            }
        }


        if (dropdown_open) {
            repaint_windows();
        }
    }


    void display() override
    {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);
        spr.set_texture_index(12);
        auto pos = cursor_;
        if (pointer_) {
            spr.set_texture_index(30);
            pos.x -= 4.0_fixed;
        }
        spr.set_position(pos);
        spr.set_priority(0);
        if (not cursor_captured_) {
            PLATFORM.screen().draw(spr);
        }

        for (auto& ico : mem_->dock_icons_) {
            spr.set_texture_index(ico.icon_gfx());
            spr.set_position(ico.pos());
            PLATFORM.screen().draw(spr);
        }
    }


    Optional<Text> hint_label_;


    void focus_window(const char* name)
    {
        if (mem_->windows_.empty()) {
            return;
        }
        mem_->menu_bar_opts_.clear();
        draw_menu_bar();
        if (str_eq(mem_->windows_.back()->name(), name)) {
            mem_->windows_.back()->minimized_ = false;
            update_focus();
            repaint_windows();
            return;
        }

        for (auto& win : mem_->windows_) {
            if (str_eq(win->name(), name)) {
                std::swap(win, mem_->windows_.back());
                mem_->windows_.back()->minimized_ = false;
                update_focus();
                repaint_windows();
                return;
            }
        }
    }


    void capture_focus(bool captured)
    {
        cursor_captured_ = captured;
    }


    void minimize_window(const char* name)
    {
        for (auto& win : mem_->windows_) {
            if (str_eq(win->name(), name)) {
                win->minimized_ = true;
                std::swap(mem_->windows_[0], win);
                for (auto& win2 : reversed(mem_->windows_)) {
                    if (not win2->minimized_) {
                        focus_window(win2->name());
                        break;
                    }
                }
            }
        }
    }


    void repaint_windows()
    {
        for (int x = 0; x < 30; ++x) {
            for (int y = 2; y < 17; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
        if (not mem_->windows_.empty()) {
            int i = mem_->windows_.size() - 1;
            while (i > -1) {
                if (not mem_->windows_[i]->minimized_) {
                    mem_->windows_[i]->repaint();
                    break;
                }
                --i;
            }
        }

        for (auto& opt : mem_->menu_bar_opts_) {
            opt.repaint();
        }
    }


    void make_window(DockIcon* application)
    {
        if (str_eq(application->name(), "Compass")) {
            mem_->windows_.push_back(
                allocate_dynamic<ExplorerWindow>("os-window", application));
        } else if (str_eq(application->name(), "SkyTunes")) {
            mem_->windows_.push_back(
                allocate_dynamic<SkyTunesWindow>("os-window", application));
        } else if (str_eq(application->name(), "mGBA")) {
            mem_->windows_.push_back(
                allocate_dynamic<mGBAWindow>("os-window", application));
        } else if (str_eq(application->name(), "TextEdit")) {
            mem_->windows_.push_back(
                allocate_dynamic<TextEditWindow>("os-window", application));
        } else if (str_eq(application->name(), "Seeker")) {
            mem_->windows_.push_back(
                allocate_dynamic<SeekerWindow>("os-window", application));
        } else if (str_eq(application->name(), "Lisp")) {
            mem_->windows_.push_back(
                allocate_dynamic<LispWindow>("os-window", application));
        } else if (str_eq(application->name(), "System Monitor")) {
            mem_->windows_.push_back(allocate_dynamic<SystemMonitorWindow>(
                "os-window", application));
        } else {
            mem_->windows_.push_back(
                allocate_dynamic<Window>("os-window", application));
        }
        application->on_open_callback_(&*mem_->windows_.back(),
                                       application->on_open_param_);
        update_focus();
        application->on_open_callback_ = [](Window*, int) {};
        repaint_windows();
    }


    void close_window(const char* name)
    {
        for (auto it = mem_->windows_.begin();
             it not_eq mem_->windows_.end();) {
            if (str_eq(name, (*it)->name())) {
                it = mem_->windows_.erase(it);
            } else {
                ++it;
            }
        }
        mem_->menu_bar_opts_.clear();
        draw_menu_bar();
        update_focus();
        repaint_windows();
    }


    void update_focus()
    {
        mem_->menu_bar_opts_.clear();
        draw_menu_bar();
        if (mem_->windows_.empty()) {
            return;
        }
        for (auto& win : mem_->windows_) {
            win->set_focus(false);
        }
        mem_->windows_.back()->set_focus(true);
    }


    DropdownMenu* insert_dropdown_menu(const char* name)
    {
        if (not mem_->menu_bar_opts_.full()) {
            u8 x = 1;
            if (not mem_->menu_bar_opts_.empty()) {
                auto& last = mem_->menu_bar_opts_.back();
                x += last.x() + last.name_len() + 1;
            }
            mem_->menu_bar_opts_.emplace_back(name, x, 1);
            mem_->menu_bar_opts_.back().repaint();
            return &mem_->menu_bar_opts_.back();
        }
        return nullptr;
    }


    void clear_dropdown_menus()
    {
        mem_->menu_bar_opts_.clear();
        draw_menu_bar();
    }


private:
    Vec2<Fixnum> cursor_;

    struct Mem
    {
        Buffer<DockIcon, 8> dock_icons_;
        Buffer<DynamicMemory<Window>, 8> windows_;
        Buffer<Clickable*, 40> clickables_;
        Buffer<DropdownMenu, 4> menu_bar_opts_;
        Optional<DeferredScene> resume_;
        Time fade_timer_ = 0;
        bool resume_flag_ = false;

        Mem()
        {
        }
    };

    DynamicMemory<Mem> mem_;
    bool pointer_ = false;
    bool cursor_captured_ = false;
};



void DesktopOS::DropdownMenu::on_hover()
{
    bool menu_open = false;
    for (auto& menu : g_os_->dropdown_menus()) {
        if (&menu not_eq this and menu.is_open()) {
            menu_open = true;
            menu.close();
        }
    }

    if (menu_open) {
        on_click();
    }
}



ScenePtr boot_desktop_os(Optional<DeferredScene> resume)
{
    return make_scene<DesktopOS>(resume);
}



} // namespace skyland
