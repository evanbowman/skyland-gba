#include "module.hpp"
#include "string.hpp"



namespace skyland {



detail::_Module::Factory* detail::_Module::factories_;



ScenePtr<Scene> detail::_Module::load(Platform& pfrm, const char* name)
{
    auto current = factories_;

    while (current) {
        if (str_cmp(name, current->name()) == 0) {
            return current->create(pfrm);
        }

        current = current->next_;
    }


    return null_scene();
}



} // namespace skyland
