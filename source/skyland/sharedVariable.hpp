////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once



// Mainly intended for sharing integer variables with configuration scripts.



namespace skyland
{



class SharedVariable
{
public:
    SharedVariable(const char* name);
    SharedVariable(const char* name, int initial);


    SharedVariable(const SharedVariable&) = delete;


    ~SharedVariable();


    static SharedVariable* load(const char* name);


    void set(int value)
    {
        value_ = value;
    }


    int get() const
    {
        return value_;
    }


    operator int() const
    {
        return value_;
    }


private:
    const char* name_;
    SharedVariable* next_;
    int value_;
};



#define SHARED_VARIABLE(name) SharedVariable name(#name)



} // namespace skyland
