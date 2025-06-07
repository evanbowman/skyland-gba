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

#include "int.h"
#include <type_traits>
#include <utility>



template <s32 precision, typename T> class FixedPointT
{
public:
    using ValueType = T;
    static_assert(std::is_integral<ValueType>());
    static_assert(precision % 2 == 0);


    static constexpr ValueType scale()
    {
        return ValueType(1) << precision;
    }


    static constexpr ValueType half_scale()
    {
        return ValueType(1) << (precision / 2);
    }


    static constexpr FixedPointT create(ValueType data)
    {
        FixedPointT value;
        value.data_ = data;

        return value;
    }



    template <s32 other_precision, typename U>
    constexpr static FixedPointT from_fp(FixedPointT<other_precision, U> other)
    {
        return create(precision < other_precision
                          ? other.data() / (other.scale() / scale())
                          : other.data() * (scale() / other.scale()));
    }



    constexpr FixedPointT() : data_(0)
    {
    }


    constexpr explicit FixedPointT(float value)
        : data_(static_cast<ValueType>(value * scale()))
    {
    }


    FixedPointT(const FixedPointT& other) : data_(other.data_)
    {
    }


    FixedPointT(FixedPointT&& other) : data_(std::move(other.data_))
    {
    }


    FixedPointT& operator=(const FixedPointT& other)
    {
        data_ = other.data_;
        return *this;
    }


    FixedPointT& operator=(FixedPointT&& other)
    {
        data_ = other.data_;
        return *this;
    }


    FixedPointT& operator=(s32 value)
    {
        data_ = value * scale();
        return *this;
    }


    constexpr float as_float() const
    {
        return static_cast<float>(data_) / scale();
    }


    constexpr s32 as_integer() const
    {
        return data_ / scale();
    }


    static constexpr FixedPointT from_integer(s32 value)
    {
        return create(value * scale());
    }


    friend bool operator<(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ < rhs.data_;
    }


    friend bool operator>(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ > rhs.data_;
    }


    friend bool operator>=(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ >= rhs.data_;
    }


    friend bool operator<=(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ <= rhs.data_;
    }


    friend bool operator==(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ == rhs.data_;
    }


    friend bool operator not_eq(FixedPointT lhs, FixedPointT rhs)
    {
        return lhs.data_ not_eq rhs.data_;
    }


    friend FixedPointT operator*(FixedPointT lhs, FixedPointT rhs)
    {
        return mul(lhs, rhs);
    }


    friend FixedPointT operator/(FixedPointT lhs, FixedPointT rhs)
    {
        return div(lhs, rhs);
    }


    friend FixedPointT operator+(FixedPointT lhs, FixedPointT rhs)
    {
        return create(lhs.data_ + rhs.data_);
    }


    friend FixedPointT operator-(FixedPointT lhs, FixedPointT rhs)
    {
        return create(lhs.data_ - rhs.data_);
    }


    FixedPointT& operator*=(FixedPointT other)
    {
        *this = (*this * other);
        return *this;
    }


    FixedPointT& operator/=(FixedPointT other)
    {
        *this = (*this / other);
        return *this;
    }


    FixedPointT& operator+=(FixedPointT other)
    {
        *this = (*this + other);
        return *this;
    }


    FixedPointT& operator-=(FixedPointT other)
    {
        *this = (*this - other);
        return *this;
    }


    ValueType data() const
    {
        return data_;
    }


    constexpr ValueType numerator() const
    {
        return as_integer();
    }


    constexpr ValueType denominator() const
    {
        return data_ & (scale() - 1);
    }


private:
    static FixedPointT mul(FixedPointT lhs, FixedPointT rhs)
    {
        ValueType data = lhs.data_ / half_scale();
        ValueType rhs_data = rhs.data_ / half_scale();
        return create(data * rhs_data);
    }


    static FixedPointT div(FixedPointT lhs, FixedPointT rhs)
    {
        ValueType data = lhs.data_ * half_scale();
        ValueType rhs_data = rhs.data_ / half_scale();
        return create(data / rhs_data);
    }


    ValueType data_;
};


using Fixnum = FixedPointT<40, s64>;


constexpr Fixnum operator"" _fixed(long double value)
{
    return Fixnum((static_cast<float>(value)));
}



#ifdef TEST

#include <iostream>



int main()
{
    Fixnum f1(0.5f);
    Fixnum f2(8.f);

    std::cout << (f1 * f2).as_float() << std::endl;

    f2 *= -1.3f;

    std::cout << (f1 * f2).as_float() << std::endl;

    std::cout << ((Fixnum(0.00015f) * 16777)).as_float() << std::endl;
    std::cout << 0.00015f * 16777 << std::endl;

    std::cout << Fixnum(0.0003f).as_float() << std::endl;
}



#endif
