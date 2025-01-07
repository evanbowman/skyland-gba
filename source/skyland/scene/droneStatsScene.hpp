////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



int file_line_count(const char* name);



class DroneStatsScene : public Scene
{
public:
    DroneStatsScene(SharedEntityRef<Drone> drone) : drone_(drone)
    {
    }


    static constexpr const auto text_colors =
        FontColors{custom_color(0x57400f), custom_color(0xc7b289)};

    static constexpr const auto text_colors_inv =
        FontColors{custom_color(0xc7b289), custom_color(0x57400f)};

    void enter(Scene& prev) override
    {
        PLATFORM.speaker().play_sound("tw_bell", 2);

        PLATFORM.screen().schedule_fade(0.5f);

        for (u8 x = 2; x < 28; ++x) {
            for (u8 y = 2; y < 18; ++y) {
                Text::print(" ", OverlayCoord{x, y}, text_colors);
            }
        }

        Text::print(
            SYS_CSTR(drone_info_heading), OverlayCoord{3, 3}, text_colors);

        Text::print(
            "________________________", OverlayCoord{3, 4}, text_colors);

        StringBuffer<96> temp;
        temp = "name:";
        temp += drone_->name();
        Text::print(temp.c_str(), OverlayCoord{3, 6}, text_colors);

        Text::print(
            "________________________", OverlayCoord{3, 7}, text_colors);


        Text::print(
            SYS_CSTR(drone_info_notes), OverlayCoord{3, 9}, text_colors_inv);

        temp.clear();

        const char* drone_info_file = "/strings/drone_info.txt";

        auto lcnt = file_line_count(drone_info_file);

        int linum = (drone_->position().x + drone_->position().y +
                     APP.current_world_location()) %
                    lcnt;

        auto line = get_line_from_file(drone_info_file, linum + 1);

        const char* str = line->c_str();
        tv_.emplace();
        tv_->assign(
            str, OverlayCoord{3, 11}, OverlayCoord{25, 5}, 0, text_colors);
    }


    void exit(Scene& next) override
    {
        tv_.reset();
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().schedule_fade(0);
    }


    ScenePtr update(Time delta) override
    {
        if (APP.player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
        }

        return null_scene();
    }


private:
    SharedEntityRef<Drone> drone_;
    Optional<TextView> tv_;
};



} // namespace skyland
