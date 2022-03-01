#pragma once

#include <new>

#include "memory/pool.hpp"


template <typename T> struct BiNode {
    BiNode* right_;
    BiNode* left_;
    T data_;
};


// We're trying to save bytes. We want to allow the list to optionally include a
// pointer to a distinct memory pool if needed, or, an alternate implementation
// of ListData could refer to a static pool instead, saving four bytes.
template <typename T, typename _Pool>
struct ListData {

    using Pool = _Pool;

    ListData(Pool& pool)
    {
        pool_ = &pool;
    }

    BiNode<T>* begin_;
    Pool* pool_;

    Pool& pool() const
    {
        return *pool_;
    }


};


template <typename T, typename Data> class List {
public:
    using Node = BiNode<T>;
    using ValueType = T;

    using Pool = typename Data::Pool;

    template <typename ...Args>
    List(Args&& ...args) : data_(std::forward<Args>(args)...)
    {
        data_.begin_ = nullptr;

        static_assert(sizeof(Node) <= Pool::element_size() and
                      alignof(Node) <= Pool::alignment(),
                      "Pool incompatible");
    }

    List(List&& other)
    {
        data_ = other.data_;
        other.data_.begin_ = nullptr;
    }

    List(const List&) = delete;

    ~List()
    {
        clear();
    }

    void push(const T& elem)
    {
        if (auto mem = data_.pool().get()) {
            new (mem) Node{data_.begin_, nullptr, elem};
            if (data_.begin_) {
                data_.begin_->left_ = reinterpret_cast<Node*>(mem);
            }
            data_.begin_ = reinterpret_cast<Node*>(mem);
        }
    }

    void push(T&& elem)
    {
        if (auto mem = data_.pool().get()) {
            new (mem) Node{data_.begin_, nullptr, std::forward<T>(elem)};
            if (data_.begin_) {
                data_.begin_->left_ = reinterpret_cast<Node*>(mem);
            }
            data_.begin_ = reinterpret_cast<Node*>(mem);
        }
    }

    void pop()
    {
        if (data_.begin_) {
            auto popped = data_.begin_;

            popped->~Node();
            data_.pool().post(reinterpret_cast<u8*>(popped));

            data_.begin_ = data_.begin_->right_;
            if (data_.begin_) {
                data_.begin_->left_ = nullptr;
            }
        }
    }

    void clear()
    {
        while (data_.begin_)
            pop();
    }

    bool empty() const
    {
        return data_.begin_ == nullptr;
    }

    class Iterator {
    public:
        Iterator(Node* ptr) : node_(ptr)
        {
        }

        const Iterator& operator++()
        {
            node_ = node_->right_;
            return *this;
        }

        T* operator->()
        {
            return &node_->data_;
        }

        T& operator*()
        {
            return node_->data_;
        }

        // Don't implement operator-- yet! list::end() is implemented in sort of
        // a hacky way.

        bool operator==(const Iterator& other) const
        {
            return other.node_ == node_;
        }

        bool operator not_eq(const Iterator& other) const
        {
            return other.node_ not_eq node_;
        }

        Node* node_;
    };

    Iterator erase(Iterator it)
    {
        if (it.node_->left_) {
            it.node_->left_->right_ = it.node_->right_;
        }

        if (it.node_->right_) {
            it.node_->right_->left_ = it.node_->left_;
        }

        if (it.node_ == data_.begin_) {
            data_.begin_ = data_.begin_->right_;
        }

        auto next = it.node_->right_;

        it.node_->~Node();
        data_.pool().post(reinterpret_cast<u8*>(it.node_));

        return Iterator(next);
    }

    Iterator begin() const
    {
        return Iterator(data_.begin_);
    }

    Iterator end() const
    {
        // NOTE: This technically works, but prevents us from implementing the
        // decrement operator on the Iterator. FIXME!
        return Iterator(nullptr);
    }

private:
    Data data_;
};


template <typename T, typename Pool> u32 length(const List<T, Pool>& lat)
{
    u32 len = 0;

    for (auto it = lat.begin(); it not_eq lat.end(); ++it) {
        ++len;
    }

    return len;
}


template <typename T, typename Pool> T* list_ref(List<T, Pool>& lat, int i)
{
    for (auto& elem : lat) {
        if (i == 0) {
            return &elem;
        }
        --i;
    }
    return nullptr;
}
