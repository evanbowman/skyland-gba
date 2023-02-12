#pragma once

#include "string.hpp"
#include <optional>



class Platform;



namespace skyland
{



class DataCart
{
public:

    DataCart(int num) : id_(num)
    {
    }


    using LabelString = StringBuffer<32>;
    using Name = LabelString;
    using Subheading = LabelString;
    using ContentString = StringBuffer<32>;


    LabelString get_label_string(Platform& pfrm, const char* field) const;
    ContentString get_content_string(Platform& pfrm, const char* field) const;


    Name name(Platform& pfrm) const;

    Subheading subheading(Platform& pfrm) const;


    int id() const
    {
        return id_;
    }


private:

    const char* config(Platform&) const;

    int id_;
};



class DataCartLibrary
{
public:


    DataCartLibrary(Platform&);


    void store(Platform&, DataCart cart);


    std::optional<DataCart> load(int id) const;


    int max_carts() const
    {
        return max_carts_;
    }


private:
    u32 carts_;
    u32 max_carts_;
};



}
