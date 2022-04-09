////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "int.h"
#include <type_traits>
#include <utility>



template <s32 precision, typename T> class FixedPoint
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


    static constexpr FixedPoint create(ValueType data)
    {
        FixedPoint value;
        value.data_ = data;

        return value;
    }


    constexpr FixedPoint() : data_(0)
    {
    }


    constexpr FixedPoint(float value) :
        data_(static_cast<ValueType>(value * scale()))
    {
    }


    FixedPoint(const FixedPoint& other) :
        data_(other.data_)
    {
    }


    FixedPoint(FixedPoint&& other) :
        data_(std::move(other.data_))
    {
    }


    FixedPoint& operator=(const FixedPoint& other)
    {
        data_ = other.data_;
        return *this;
    }


    FixedPoint& operator=(FixedPoint&& other)
    {
        data_ = other.data_;
        return *this;
    }


    FixedPoint& operator=(s32 value)
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


    static FixedPoint from_integer(s32 value)
    {
        return create(value * scale());
    }


    friend bool operator<(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ < rhs.data_;
    }


    friend bool operator>(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ > rhs.data_;
    }


    friend bool operator>=(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ >= rhs.data_;
    }


    friend bool operator<=(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ <= rhs.data_;
    }


    friend bool operator==(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ == rhs.data_;
    }


    friend bool operator not_eq(FixedPoint lhs, FixedPoint rhs)
    {
        return lhs.data_ not_eq rhs.data_;
    }


    friend FixedPoint operator*(FixedPoint lhs, FixedPoint rhs)
    {
        return mul(lhs, rhs);
    }


    friend FixedPoint operator/(FixedPoint lhs, FixedPoint rhs)
    {
        return div(lhs, rhs);
    }


    friend FixedPoint operator+(FixedPoint lhs, FixedPoint rhs)
    {
        return create(lhs.data_ + rhs.data_);
    }


    friend FixedPoint operator-(FixedPoint lhs, FixedPoint rhs)
    {
        return create(lhs.data_ - rhs.data_);
    }


    FixedPoint& operator*=(FixedPoint other)
    {
        *this = (*this * other);
        return *this;
    }


    FixedPoint& operator/=(FixedPoint other)
    {
        *this = (*this / other);
        return *this;
    }


    FixedPoint& operator+=(FixedPoint other)
    {
        *this = (*this + other);
        return *this;
    }


    FixedPoint& operator-=(FixedPoint other)
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

    static FixedPoint mul(FixedPoint lhs, FixedPoint rhs)
    {
        ValueType data = lhs.data_ / half_scale();
        ValueType rhs_data = rhs.data_ / half_scale();
        return create(data * rhs_data);
    }


    static FixedPoint div(FixedPoint lhs, FixedPoint rhs)
    {
        ValueType data = lhs.data_ * half_scale();
        ValueType rhs_data = rhs.data_ / half_scale();
        return create(data / rhs_data);
    }


    ValueType data_;
};


using Fixnum = FixedPoint<40, s64>;


constexpr Fixnum operator "" _fixed(long double value)
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
