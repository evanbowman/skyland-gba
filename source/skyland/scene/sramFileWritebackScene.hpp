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

#include "containers/vector.hpp"
#include "graphics/overlay.hpp"
#include "modules/userContext.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



// The user edited a ROM file. Obviously, we cannot write the modified file back
// to ROM, but we should notify the user that we are about to save a file to the
// SRAM filesystem instead, and give him/her the option to cancel.



namespace skyland
{



class SramFileWritebackScene : public Scene
{
public:
    SramFileWritebackScene(const char* path,
                           Vector<char>&& text_buffer,
                           UserContext&& user_context);


    ScenePtr<Scene> update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


private:
    StringBuffer<64> path_;
    Vector<char> text_buffer_;

    UserContext user_context_;


    Optional<TextView> menu_text_;
};



} // namespace skyland
