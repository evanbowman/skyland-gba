////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "macrocosmScene.hpp"



namespace skyland::macro
{



class CreateBlockScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr update(Player& player, macro::EngineImpl& state) override;


    void show_options(macro::EngineImpl& state);


    virtual ScenePtr onclick(macro::EngineImpl&);
    virtual void adjust_cursor_z(Player& player, macro::EngineImpl&);


    virtual void init_cursor(macro::EngineImpl& state);


    virtual void collect_options(macro::EngineImpl& state);


    virtual void message(macro::EngineImpl&);


    virtual void edit(macro::EngineImpl& state, terrain::Type t);


    virtual terrain::Cost cost(macro::EngineImpl& state, terrain::Type t);


    virtual bool check_z()
    {
        return true;
    }


protected:
    Buffer<macro::terrain::Type, 34> options_;

    static s8 selector_;
};



class BuildImprovementScene : public CreateBlockScene
{
public:
    void collect_options(macro::EngineImpl& state) override;

    void adjust_cursor_z(Player& player, macro::EngineImpl&) override
    {
    }

    void init_cursor(macro::EngineImpl& state) override;

    terrain::Cost cost(macro::EngineImpl& state, terrain::Type t) override;


    bool check_z() override
    {
        return false;
    }


    void edit(macro::EngineImpl& state, terrain::Type t) override;
};



} // namespace skyland::macro
