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
}


inline ScenePtr<Scene> load_module(Platform& pfrm, const char* name)
{
    return detail::_Module::load(pfrm, name);
}



template <typename T>
class Module : public detail::_Module {
public:

    class Factory : public _Module::Factory {
    public:

        virtual const char* name() override
        {
            return T::module_name();
        }


        virtual u16 icon() override
        {
            return T::icon();
        }


        ScenePtr<Scene> create(Platform&) override
        {
            return scene_pool::alloc<T>();
        }
    };

};



}
