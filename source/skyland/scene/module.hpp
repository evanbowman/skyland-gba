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
