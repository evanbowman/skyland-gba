#pragma once


#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"



// The Module class defines a plugin interface through which applets on the
// title screen extras menu may be instantiated. A module is also child class of
// Scene, so modules may be instantiated by other scenes without needing to use
// the Module interface.



namespace skyland {



namespace detail {
class _Module : public Scene
{
public:
    class Factory
    {
    public:
        Factory()
        {
            next_ = _Module::factories_;
            _Module::factories_ = this;
        }


        virtual SystemString name() = 0;


        virtual u16 icon() = 0;


        virtual ~Factory()
        {
        }


        virtual bool run_scripts() = 0;


        virtual bool stop_sound() = 0;


        virtual bool enable_custom_scripts() = 0;


        static Factory* get(int index)
        {
            auto current = factories_;
            while (current and index >= 0) {
                if (index-- == 0) {
                    return current;
                }
                current = current->next_;
            }
            return nullptr;
        }


        virtual ScenePtr<Scene> create(Platform&) = 0;

        Factory* next_;
    };

    static Factory* factories_;
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


    class Factory : public _Module::Factory
    {
    public:
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


        bool stop_sound() override
        {
            return T::stop_sound();
        }


        bool enable_custom_scripts() override
        {
            return T::enable_custom_scripts();
        }


        ScenePtr<Scene> create(Platform&) override
        {
            return scene_pool::alloc<T>();
        }
    };
};



} // namespace skyland
