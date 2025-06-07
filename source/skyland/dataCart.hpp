////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "platform/conf.hpp"
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
    using ContentString = Conf::String;


    LabelString get_label_string(const char* field) const;
    Optional<ContentString> get_content_string(const char* field) const;
    ContentString expect_content_string(const char* field) const;


    Name name() const;

    Subheading subheading() const;


    int id() const
    {
        return id_;
    }


private:
    const char* config() const;

    int id_;
};



class DataCartLibrary
{
public:
    DataCartLibrary();


    void store(DataCart cart);


    Optional<DataCart> load(int id) const;


    int max_carts() const
    {
        return max_carts_;
    }


private:
    u32 carts_;
    u32 max_carts_;
};



} // namespace skyland
