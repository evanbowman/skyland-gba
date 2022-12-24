////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "bugReportModule.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "version.hpp"



namespace skyland
{



ScenePtr<Scene>
BugReportModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    StringBuffer<64> hash("Build hash: ");

    if (auto str = pfrm.load_file_contents("", "/strings/commit_hash.txt")) {
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
