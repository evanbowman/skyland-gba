#include "extension.hpp"
#include "allocator.hpp"



namespace skyland
{



struct ExtensionMem
{
    ExtensionRef* backlink_;
    u8 mem_[32];
};



struct Extensions
{
    Buffer<ExtensionMem, 55> slots_;
};



Optional<DynamicMemory<Extensions>> extension_mem;



bool extension_alloc(ExtensionRef* ref)
{
    if (not extension_mem) {
        extension_mem = allocate_dynamic<Extensions>("extension-mem");
    }

    if ((*extension_mem)->slots_.full()) {
        return false;
    }

    (*extension_mem)->slots_.emplace_back();
    (*extension_mem)->slots_.back().backlink_ = ref;
    ref->cell_ = (*extension_mem)->slots_.size() - 1;
    return true;
}



void* extension_deref(ExtensionRef* ref)
{
    return (*extension_mem)->slots_[ref->cell_].mem_;
}



void extension_free(ExtensionRef* ref)
{
    if (ref->cell_ == (*extension_mem)->slots_.size() - 1) {
        (*extension_mem)->slots_.pop_back();
    } else {
        auto gap_idx = ref->cell_;
        auto& slots = (*extension_mem)->slots_;
        slots[gap_idx].backlink_ = slots.back().backlink_;
        slots[gap_idx].backlink_->cell_ = gap_idx;
        memcpy(slots[gap_idx].mem_, slots.back().mem_, 32);
        slots.pop_back();
    }
}



}
