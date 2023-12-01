////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


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


    LabelString get_label_string(const char* field) const;
    ContentString get_content_string(const char* field) const;


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


    std::optional<DataCart> load(int id) const;


    int max_carts() const
    {
        return max_carts_;
    }


private:
    u32 carts_;
    u32 max_carts_;
};



} // namespace skyland
