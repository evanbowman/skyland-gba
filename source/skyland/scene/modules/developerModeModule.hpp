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
#include "skyland/scene/module.hpp"



namespace skyland
{



class DeveloperModeModule : public Module<DeveloperModeModule>
{
public:
    DeveloperModeModule()
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    static SystemString module_name()
    {
        return SystemString::module_developer_mode;
    }


    static u16 icon()
    {
        return 2472;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool stop_sound()
    {
        return false;
    }


    ScenePtr<Scene> update(Time delta) override;


    void set_opt(bool value);


    static Factory factory_;

    Optional<TextView> message_;
    Optional<TextView> message_overflow_;
    Optional<Text> option_text_;
    bool was_developer_mode_ = false;

    bool exit_ = false;

    bool option_ = false;
};



} // namespace skyland
