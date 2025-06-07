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

#include "number/numeric.hpp"



// A time-driven scheduler. TODO: Optimize this class further.



class Platform;



namespace skyland
{



class App;



class Timer
{
public:
    virtual ~Timer()
    {
    }


    virtual void timer_expired() = 0;


    Time remaining() const
    {
        return clock_;
    }


    void __override_clock(Time value)
    {
        clock_ = value;
    }


    Time interval() const
    {
        return interval_;
    }


private:
    Time clock_ = 0;
    Time interval_ = 0;
    Timer* next_ = nullptr;

    friend class BulkTimer;
};



class BulkTimer
{
public:
    void update(Time elapsed_delta);
    void rewind(Time elapsed_delta);


    void schedule(Timer* subscriber, Time timeout);


    void deschedule(Timer* subscriber);


private:
    Timer* scheduled_ = nullptr;
};



} // namespace skyland
