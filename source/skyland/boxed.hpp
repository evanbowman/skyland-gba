#pragma once


#include <type_traits>
#include <utility>



namespace skyland {


template <typename T,
          typename DefaultType,
          int mem = sizeof(T),
          int align = 8>
class Boxed {
public:

    template <typename ...Args>
    Boxed(Args&& ...args)
    {
        static_assert(std::is_base_of<T, DefaultType>::value);
        static_assert(alignof(DefaultType) <= align);
        static_assert(sizeof(DefaultType) <= mem);
        new (mem_) DefaultType(std::forward<Args>(args)...);
    }


    template <typename U, typename ...Args>
    void emplace(Args&& ...args)
    {
        static_assert(std::is_base_of<T, U>::value);
        static_assert(alignof(U) <= align);
        static_assert(sizeof(U) <= mem);
        destroy();
        new (mem_) U(std::forward<Args>(args)...);
    }


    T& operator*() const
    {
        return *reinterpret_cast<const T*>(mem_);
    }


    T* operator->() const
    {
        return reinterpret_cast<const T*>(mem_);
    }


    T& operator*()
    {
        return *reinterpret_cast<T*>(mem_);
    }


    T* operator->()
    {
        return reinterpret_cast<T*>(mem_);
    }


    Boxed(const Boxed&) = delete;


    ~Boxed()
    {
        destroy();
    }

private:

    void destroy()
    {
        reinterpret_cast<T*>(mem_)->~T();
    }

    alignas(align) char mem_[mem];
};



}
