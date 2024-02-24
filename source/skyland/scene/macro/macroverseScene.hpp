////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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


#pragma once

#include "graphics/overlay.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene.hpp"



namespace skyland::macro
{


class MacroverseScene : public Scene
{
public:
    MacroverseScene(bool fastload = false) : fastload_(fastload)
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& prev) override;


    ScenePtr<Scene> update(Time delta) override;


    void display() override;

private:
    enum class State {
        reveal,
        wait,
        fade_in,
        show,
        options,
        options_2,
        text_prompt,
        create_colony_options,
        create_colony,
        select_colony_layout,
    } state_ = State::reveal;


    void describe_selected(macro::EngineImpl& state);


    void show_layout_text();


    Time timer_;

    Vec2<s8> selected_ = {};
    Vec2<s8> initial_sector_;
    Vec2<Float> camera_;
    Optional<Vec2<s8>> selected_colony_;

    terrain::Sector::Shape shape_ = terrain::Sector::Shape::cube;

    Buffer<Vec2<s8>, 4> colony_create_slots_;

    bool exit_ = false;
    bool fastload_ = false;
    bool abandon_ = false;
    bool freebuild_size_configure_ = false;

    u8 opt_cursor_ = 0;

    Buffer<Text, 6> text_objs_;
};



} // namespace skyland::macro
