#include "extension.hpp"
#include "allocator.hpp"



namespace skyland
{



struct ExtensionMem
{
    ExtensionRef* backlink_;
    u8 mem_[32];
};



Vector<ExtensionMem> extension_mem("extension-mem");



ExtensionStats extension_stats()
{
    ExtensionStats result;
    result.used = extension_mem.size();
    return result;
}



bool extension_alloc(ExtensionRef* ref)
{
    extension_mem.emplace_back();
    extension_mem.back().backlink_ = ref;
    ref->cell_ = extension_mem.size() - 1;

    return true;
}



void* extension_deref(ExtensionRef* ref)
{
    return extension_mem[ref->cell_].mem_;
}



void extension_free(ExtensionRef* ref)
{
    if (ref->cell_ == extension_mem.size() - 1) {
        extension_mem.pop_back();
    } else {
        auto gap_idx = ref->cell_;
        auto& slots = extension_mem;
        slots[gap_idx].backlink_ = slots.back().backlink_;
        slots[gap_idx].backlink_->cell_ = gap_idx;
        memcpy(slots[gap_idx].mem_, slots.back().mem_, 32);
        slots.pop_back();
        extension_mem.shrink_to_fit();
    }
}



} // namespace skyland
