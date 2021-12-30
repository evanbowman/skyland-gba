#pragma once

#include "number/numeric.hpp"
#include "pool.hpp"
#include <optional>


//
// A class for Reference Counted objects.
//
// This program targets embedded systems, and intentionally doesn't
// link with the C++ standard library (although we use some standard
// headers, like <type_traits>). Otherwise I might use
// std::shared_ptr... Rc differs from the standard smart pointers in
// other ways though:
// * Can not contain null.
//
// Additionally, you need to supply a memory pool when creating an Rc<>.
//


namespace detail {

    // The default control block: requires that the reference counted object was
    // allocated from a pool. Kind of limiting, but as we do not have a malloc
    // implementation, this is the best we can really do for a general purpose
    // allocator.
    template <typename T, u32 Count>
    struct PooledControlBlock {
        template <typename... Args>
        PooledControlBlock(ObjectPool<PooledControlBlock, Count>* pool,
                           void (*finalizer_hook)(PooledControlBlock*),
                           Args&&... args)
            : data_(std::forward<Args>(args)...), pool_(pool),
              finalizer_hook_(finalizer_hook), strong_count_(0), weak_count_(0)
        {
            if (finalizer_hook_ == nullptr) {
                finalizer_hook_ = [](PooledControlBlock* ctrl) {
                    ctrl->pool_->post(ctrl);
                };
            }
        }

        T& data()
        {
            return data_;
        }

        T data_;
        ObjectPool<PooledControlBlock, Count>* pool_;
        // Because the pool is an input parameter, I do not see much reason to
        // allow custom finalizers, but in any event, having the option to
        // customize deallocation might be useful in some unforseen way.
        void (*finalizer_hook_)(PooledControlBlock*);
        Atomic<u32> strong_count_;
        Atomic<u32> weak_count_;
    };


    template <typename T>
    class IntrusiveControlBlock {
    public:
        IntrusiveControlBlock() :
            data_(nullptr),
            finalizer_hook_(nullptr),
            strong_count_(0),
            weak_count_(0)
        {
        }

        IntrusiveControlBlock(void (*finalizer_hook)(IntrusiveControlBlock*)) :
            data_(nullptr),
            finalizer_hook_(finalizer_hook),
            strong_count_(0),
            weak_count_(0)
        {
        }

        T& data()
        {
            return *data_;
        }

        T* data_;
        void (*finalizer_hook_)(IntrusiveControlBlock*);
        Atomic<u32> strong_count_;
        Atomic<u32> weak_count_;
    };
}



template <typename T, u32 Size>
using PooledRcControlBlock = detail::PooledControlBlock<T, Size>;



template <typename T>
using IntrusiveRcControlBlock = detail::IntrusiveControlBlock<T>;



template <typename ControlBlockT> class RcBase {
public:
    using ControlBlock = ControlBlockT;

    size_t strong_count() const
    {
        return control_->strong_count_;
    }

protected:
    ControlBlock* control_;

    void add_strong(ControlBlock* source)
    {
        control_ = source;
        control_->strong_count_++;
    }

    void add_weak(ControlBlock* source)
    {
        control_ = source;
        control_->weak_count_++;
    }

    void remove_strong()
    {
        if (--control_->strong_count_ == 0 and control_->weak_count_ == 0) {
            if (control_->finalizer_hook_) {
                control_->finalizer_hook_(control_);
            }
        }
    }

    void remove_weak()
    {
        if (--control_->weak_count_ == 0 and control_->strong_count_ == 0) {
            if (control_->finalizer_hook_) {
                control_->finalizer_hook_(control_);
            }
        }
    }
};


template <typename T, typename ControlBlockImpl> class Rc :
    public RcBase<ControlBlockImpl> {
public:
    using Super = RcBase<ControlBlockImpl>;

    Rc(const Rc& other)
    {
        Super::add_strong(other.control_);
    }

    Rc& operator=(const Rc& other)
    {
        if (Super::control_) {
            Super::remove_strong();
        }
        Super::add_strong(other.control_);
        return *this;
    }

    T& operator*() const
    {
        return Super::control_->data();
    }

    T* operator->() const
    {
        return &Super::control_->data();
    }

    T* get() const
    {
        return &Super::control_->data();
    }

    ~Rc()
    {
        Super::remove_strong();
    }

    Rc(ControlBlockImpl* control)
    {
        Super::add_strong(control);
    }

private:
    template <typename, typename> friend class Weak;
};



template <typename T, typename ControlBlockImpl> class Weak :
    public RcBase<ControlBlockImpl> {
public:
    using Super = RcBase<ControlBlockImpl>;

    Weak() = delete;

    Weak(const Rc<T, ControlBlockImpl>& other)
    {
        Super::add_weak(other.control_);
    }

    std::optional<Rc<T, ControlBlockImpl>> upgrade()
    {
        if (Super::control_->strong_count_) {
            return Rc<T, ControlBlockImpl>(Super::control_);
        } else {
            return {};
        }
    }

    ~Weak()
    {
        Super::remove_weak();
    }
};


template <typename T, u32 PoolSize, typename... Args>
static std::optional<Rc<T, PooledRcControlBlock<T, PoolSize>>>
create_pooled_rc(ObjectPool<PooledRcControlBlock<T, PoolSize>, PoolSize>* pool,
                 void (*finalizer_hook)(PooledRcControlBlock<T, PoolSize>*),
                 Args&&... args)
{
    auto ctrl =
        pool->get(pool, finalizer_hook, std::forward<Args>(args)...);
    if (ctrl) {
        return Rc<T, PooledRcControlBlock<T, PoolSize>>(ctrl);
    } else {
        return {};
    }
}
