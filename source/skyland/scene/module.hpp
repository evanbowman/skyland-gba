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


#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"



// The Module class defines a plugin interface through which applets on the
// title screen extras menu may be instantiated. A module is also child class of
// Scene, so modules may be instantiated by other scenes without needing to use
// the Module interface.



namespace skyland
{



namespace detail
{
class _Module : public Scene
{
public:
    class Factory
    {
    public:
        Factory(bool requires_developer_mode)
        {
            if (not requires_developer_mode) {
                next_ = _Module::factories_;
                _Module::factories_ = this;
            } else {
                next_ = _Module::developer_mode_factories_;
                _Module::developer_mode_factories_ = this;
            }
        }


        virtual SystemString name() = 0;


        virtual u16 icon() = 0;


        virtual ~Factory()
        {
        }


        virtual bool requires_developer_mode() = 0;


        virtual bool run_scripts() = 0;


        virtual bool stop_sound() = 0;


        virtual bool enable_custom_scripts() = 0;


        static Factory* get(int index, bool is_developer_mode)
        {
            auto current = factories_;
            while (current and index >= 0) {
                if (index-- == 0) {
                    return current;
                }
                current = current->next_;
            }

            if (is_developer_mode) {
                current = developer_mode_factories_;
                while (current and index >= 0) {
                    if (index-- == 0) {
                        return current;
                    }
                    current = current->next_;
                }
            }

            return nullptr;
        }


        virtual ScenePtr create() = 0;

        Factory* next_;
    };

    static Factory* factories_;
    static Factory* developer_mode_factories_;
};
} // namespace detail



template <typename T> class Module : public detail::_Module
{
public:
    static bool enable_custom_scripts()
    {
        return false;
    }


    static bool stop_sound()
    {
        return true;
    }


    static bool requires_developer_mode()
    {
        return false;
    }


    class Factory : public _Module::Factory
    {
    public:
        Factory(bool requires_developer_mode = false)
            : _Module::Factory(requires_developer_mode)
        {
        }


        SystemString name() override
        {
            return T::module_name();
        }


        u16 icon() override
        {
            return T::icon();
        }


        bool run_scripts() override
        {
            return T::run_scripts();
        }


        bool requires_developer_mode() override
        {
            return T::requires_developer_mode();
        }


        bool stop_sound() override
        {
            return T::stop_sound();
        }


        bool enable_custom_scripts() override
        {
            return T::enable_custom_scripts();
        }


        ScenePtr create() override
        {
            return make_scene<T>();
        }
    };
};



} // namespace skyland
