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


#pragma once

#include "macrocosmScene.hpp"



namespace skyland::macro
{



class CreateBlockScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr<Scene> update(Player& player, macro::EngineImpl& state) override;


    void show_options(macro::EngineImpl& state);


    virtual ScenePtr<Scene> onclick(macro::EngineImpl&);
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
