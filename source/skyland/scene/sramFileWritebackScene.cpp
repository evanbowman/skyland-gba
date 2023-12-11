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


#include "sramFileWritebackScene.hpp"
#include "modules/fileBrowserModule.hpp"
#include "platform/flash_filesystem.hpp"



namespace skyland
{



SramFileWritebackScene::SramFileWritebackScene(const char* path,
                                               Vector<char>&& text_buffer,
                                               UserContext&& user_context)
    : path_(path), text_buffer_(std::move(text_buffer)),
      user_context_(std::move(user_context))
{
}



ScenePtr<Scene> SramFileWritebackScene::update(Time delta)
{
    flash_filesystem::store_file_data_text(
        path_.c_str(), text_buffer_, {.use_compression_ = true});

    return scene_pool::alloc<FileBrowserModule>(
        std::move(user_context_), path_.c_str(), true);
}



void SramFileWritebackScene::enter(Scene& prev)
{
}



void SramFileWritebackScene::exit(Scene& next)
{
}



} // namespace skyland
