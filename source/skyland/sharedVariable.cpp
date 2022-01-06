#include "sharedVariable.hpp"
#include "string.hpp"



namespace skyland {



static SharedVariable* __shared_variables = nullptr;



SharedVariable::SharedVariable(const char* name)
    : name_(name), value_(0)
{
    next_ = __shared_variables;
    __shared_variables = this;
}



SharedVariable* SharedVariable::load(const char* name)
{
    auto list = __shared_variables;

    while (list) {
        if (str_cmp(list->name_, name) == 0) {
            return list;
        }
        list = list->next_;
    }

    return nullptr;
}



SharedVariable::~SharedVariable()
{
    auto list = __shared_variables;
    SharedVariable* prev = nullptr;

    while (list) {
        const bool found_self = list == this;
        if (found_self) {
            if (prev) {
                // Unlink ourself from the list.
                prev->next_ = next_;
            } else {
                // We're the first element of the list!
                __shared_variables = next_;
            }
            return;
        }

        prev = list;
        list = list->next_;
    }
}



}
