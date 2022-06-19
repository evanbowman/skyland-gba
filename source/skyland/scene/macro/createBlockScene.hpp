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
    void enter(Platform& pfrm, macro::StateImpl& state, Scene& prev) override;


    void exit(Platform& pfrm, macro::StateImpl& state, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::StateImpl& state) override;


    void show_options(Platform& pfrm, macro::StateImpl& state);


    virtual ScenePtr<Scene> onclick(Platform&, macro::StateImpl&);
    virtual void adjust_cursor_z(Platform&, Player& player, macro::StateImpl&);


    virtual void init_cursor(macro::StateImpl& state);


    virtual void collect_options(macro::StateImpl& state);


    virtual void message(Platform& pfrm, macro::StateImpl&);


    virtual void edit(Platform&, macro::StateImpl& state, terrain::Type t);


    virtual Coins cost(macro::StateImpl& state, terrain::Type t);


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
    void collect_options(macro::StateImpl& state) override;

    void adjust_cursor_z(Platform&, Player& player, macro::StateImpl&) override
    {
    }

    void init_cursor(macro::StateImpl& state) override;

    Coins cost(macro::StateImpl& state, terrain::Type t) override;


    bool check_z() override
    {
        return false;
    }


    void edit(Platform&, macro::StateImpl& state, terrain::Type t) override;
};



class ConfigurePortScene : public CreateBlockScene
{
public:
    void collect_options(macro::StateImpl& state) override;

    void adjust_cursor_z(Platform&, Player& player, macro::StateImpl&) override
    {
    }

    void message(Platform&, macro::StateImpl&) override;


    ScenePtr<Scene> onclick(Platform&, macro::StateImpl&) override;

    Buffer<terrain::Commodity::Type, 32> commodity_types_;
};



class ConfigurePortCountScene : public MacrocosmScene
{
public:
    ConfigurePortCountScene(terrain::Commodity::Type t) : type_(t)
    {
    }


    void enter(Platform& pfrm, macro::StateImpl& state, Scene& prev) override;
    void exit(Platform& pfrm, macro::StateImpl& state, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::StateImpl& state) override;


    void show(Platform& pfrm, macro::StateImpl& state);


private:
    terrain::Commodity::Type type_;
    std::optional<Text> text_;
    int count_ = 0;
};



class ConfigurePortDestScene : public MacrocosmScene
{
public:
    ConfigurePortDestScene(terrain::Commodity::Type t, int export_count)
        : type_(t), export_count_(export_count), selection_(0)
    {
    }


    void enter(Platform& pfrm, macro::StateImpl& state, Scene& prev) override;
    void exit(Platform& pfrm, macro::StateImpl& state, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::StateImpl& state) override;


    void show(Platform& pfrm, macro::StateImpl& state);


private:
    Buffer<Vec2<s8>, 32> export_options_;

    std::optional<Text> text_;
    terrain::Commodity::Type type_;
    int export_count_;
    int selection_;
};



} // namespace skyland::macro
