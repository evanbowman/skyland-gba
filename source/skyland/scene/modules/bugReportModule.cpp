////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "bugReportModule.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "version.hpp"



namespace skyland
{



ScenePtr BugReportModule::update(Time delta)
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

    auto next = make_scene<ConfiguredURLQRViewerScene>(
        "/scripts/config/bug_report.lisp", "", hash.c_str(), [] {
            PLATFORM.screen().schedule_fade(1);
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            return make_scene<TitleScreenScene>(3);
        });

    next->format_ = 2;

    return next;
}



BugReportModule::Factory BugReportModule::factory_(false);



} // namespace skyland
