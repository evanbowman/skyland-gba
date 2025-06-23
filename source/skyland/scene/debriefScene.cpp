////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "debriefScene.hpp"
#include "boxedDialogScene.hpp"
#include "script/lisp.hpp"
#include "worldScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



ScenePtr DebriefScene::update(Time delta)
{
    auto var = lisp::get_var("debrief-strs");
    if (var == L_NIL) {
        return make_scene<ZoneImageScene>();
    } else if (not lisp::is_list(var)) {
        if (APP.is_developer_mode()) {
            PLATFORM.fatal("debrief-strs is non-list!?");
        }
        return make_scene<ZoneImageScene>();
    } else {
        auto str = var->cons().car()->string().value();
        auto dialog = allocate_dynamic<DialogString>("dialog-buffer");
        while (*str not_eq '\0') {
            dialog->push_back(*(str++));
        }
        lisp::set_var("debrief-strs", var->cons().cdr());

        auto ret = make_scene<BoxedDialogScene>(std::move(dialog));
        ret->set_next_scene(make_deferred_scene<DebriefScene>());
        return ret;
    }
}



} // namespace skyland
