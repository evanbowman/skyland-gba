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

#include "number/numeric.hpp"



// A time-driven scheduler. TODO: Optimize this class further.



class Platform;



namespace skyland
{



class App;



class Timer
{
public:

    virtual ~Timer() {}


    virtual void timer_expired(Platform&, App&) = 0;


    Microseconds remaining() const
    {
        return clock_;
    }


    void __override_clock(Microseconds value)
    {
        clock_ = value;
    }


    Microseconds interval() const
    {
        return interval_;
    }


private:
    Microseconds clock_ = 0;
    Microseconds interval_ = 0;
    Timer* next_ = nullptr;

    friend class BulkTimer;
};




class BulkTimer
{
public:

    void update(Platform&, App&, Microseconds elapsed_delta);
    void rewind(Platform&, App&, Microseconds elapsed_delta);


    void schedule(Timer* subscriber, Microseconds timeout);


    void deschedule(Timer* subscriber);


private:
    Timer* scheduled_ = nullptr;
};



}
