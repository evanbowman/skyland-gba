#pragma once


#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



namespace detail {
class _Module : public Scene {
public:
    static ScenePtr<Scene> load(Platform&, const char* name);


    class Factory {
    public:
        Factory()
        {
            next_ = _Module::factories_;
            _Module::factories_ = this;
        }


        virtual const char* name() = 0;


        virtual u16 icon() = 0;


        virtual ~Factory()
        {
        }


        virtual bool run_scripts() = 0;


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


inline ScenePtr<Scene> load_module(Platform& pfrm, const char* name)
{
    return detail::_Module::load(pfrm, name);
}



template <typename T> class Module : public detail::_Module {
public:
    static bool enable_custom_scripts()
    {
        return false;
    }


    class Factory : public _Module::Factory {
    public:
        const char* name() override
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
