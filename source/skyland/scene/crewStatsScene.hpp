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

#pragma once

#include "skyland/characterId.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



class CrewStatsScene : public Scene
{
public:
    CrewStatsScene(CharacterId selected_chr);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    Optional<DeferredScene> next_;


    Float bkg_fade_amount_ = 0.5f;
    Float exit_fade_ = 0.f;


private:
    Vector<CharacterId> chrs_;
    int page_index_ = 0;
    int icon_sel_ = 0;
    bool exit_ = false;

    enum State : u8 {
        regular,
        icon_select,
    } state_ = State::regular;

    void show_page();
};



} // namespace skyland
