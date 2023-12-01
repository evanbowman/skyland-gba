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


#include "bugReportModule.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "version.hpp"



namespace skyland
{



ScenePtr<Scene> BugReportModule::update(Microseconds delta)
{
    StringBuffer<64> hash("Build hash: ");

    if (auto str =
            PLATFORM.load_file_contents("", "/strings/commit_hash.txt")) {
        while (*str not_eq '\0' and *str not_eq '\n' and *str not_eq '\r') {
            hash.push_back(*str);
            ++str;
        }
    }

    hash += format(", Release: %.%.%.%",
                   PROGRAM_MAJOR_VERSION,
                   PROGRAM_MINOR_VERSION,
                   PROGRAM_SUBMINOR_VERSION,
                   PROGRAM_VERSION_REVISION);

    auto next = scene_pool::alloc<ConfiguredURLQRViewerScene>(
        "/scripts/config/bug_report.lisp",
        "",
        hash.c_str(),
        scene_pool::make_deferred_scene<TitleScreenScene>(3));

    next->format_ = 2;

    return next;
}



BugReportModule::Factory BugReportModule::factory_(false);



} // namespace skyland
