#include "malloc.hpp"
#include "bitvector.hpp"
#include "containers/vector.hpp"



// I didn't add a malloc implementation until nearly the end of this
// project. Only exists in case I want to leverage external
// dependencies. Skyland does not use malloc anywhere. NOTE: max allocation size
// is slightly less than SCRATCH_BUFFER_SIZE.



namespace malloc_compat
{



struct Heap
{
    struct Sector
    {
        static const int word_count = 460;

        struct Word
        {
            // NOTE: I was previously using max_align_t, but align() (defined in
            // allocator.hpp), seemingly cannot align sizes larger than the cpu
            // wordsize anyway.
            alignas(sizeof(void*)) u8 data_[sizeof(void*)];
        };

        Bitvector<word_count> taken_;
        alignas(void*) Word words_[word_count];


        bool contains_address(void* addr) const
        {
            return addr >= words_ and addr < words_ + word_count;
        }


        bool empty() const
        {
            return taken_.empty();
        }


        void free(Word* addr)
        {
            if (((intptr_t)addr) % sizeof(Word) not_eq 0) {
                Platform::fatal(format("free misaligned address! %",
                                       (intptr_t)addr).c_str());
            }

            --addr; // fetch size from slot -1
            int count = *(int*)addr;
            int start_index = addr - words_;

            for (int i = start_index; i < start_index + count; ++i) {
                if (not taken_.get(i)) {
                    Platform::fatal("heap corruption! (double free?)");
                }
                taken_.set(i, false);
            }
        }


        void* try_alloc(u32 size)
        {
            if (size > (word_count + 1) * sizeof(Word)) {
                Platform::fatal(format("allocation of % exceeds max size!",
                                       size).c_str());
            }

            int required_words = size / sizeof(Word);
            if (size % sizeof(Word)) {
                ++required_words;
            }
            ++required_words; // +1 for allocation size.

            for (int i = 0; i < word_count; ++i) {
                if (not taken_.get(i)) {
                    bool found = true;
                    for (int ii = i; ii < required_words; ++ii) {
                        if (ii == word_count) {
                            return nullptr;
                        }
                        if (taken_.get(ii)) {
                            found = false;
                            break;
                        }
                    }

                    if (found) {
                        Word* start = &words_[i];
                        // Store allocation size in the first slot.
                        *(int*)(start) = required_words;
                        ++start;

                        for (int ii = i; ii < required_words; ++ii) {
                            taken_.set(ii, true);
                        }

                        return start;
                    }
                }
            }

            return nullptr;
        }
    };

    using Sectors = Vector<Sector>;

    std::optional<Sectors> sectors_;
    bool locked_ = true;
};



static Heap heap_;



void create_heap()
{
    heap_.locked_ = false;
}



}



extern "C" {



void* malloc(size_t sz)
{
    using namespace malloc_compat;

    if (heap_.locked_) {
        // We don't create the malloc heap at startup, because even though we
        // aren't using exceptions (-fno-exceptions), libstdc++ allocates an
        // exception pool in a global constructor and there's no way to disable
        // it.
        return nullptr;
    }

    if (not heap_.sectors_) {
        warning(Platform::instance(), "create new malloc heap!");
        heap_.sectors_.emplace();
    }

    for (auto& s : *heap_.sectors_) {
        if (auto p = s.try_alloc(sz)) {
            return p;
        }
    }

    heap_.sectors_->emplace_back();

    for (auto& s : *heap_.sectors_) {
        if (auto p = s.try_alloc(sz)) {
            return p;
        }
    }

    return nullptr;
}



void free(void* ptr)
{
    using namespace malloc_compat;

    if (heap_.locked_) {
        return;
    }

    for (auto& s : *heap_.sectors_) {
        if (s.contains_address(ptr)) {
            s.free((Heap::Sector::Word*)ptr);

            bool empty_heap = true;
            for (auto& s : *heap_.sectors_) {
                if (not s.empty()) {
                    empty_heap = false;
                    break;
                }
            }

            if (empty_heap) {
                info(Platform::instance(), "destroying empty malloc heap");
                heap_.sectors_.reset();
            }

            return;
        }
    }

    Platform::fatal("invalid address passed to free!");
}



}
