#pragma once

#include "boxed.hpp"
#include "platform/platform.hpp"



namespace skyland
{



class App;



class ConsoleState
{
public:
    class Impl
    {
    public:
        virtual ~Impl()
        {
        }


        using Self = Boxed<Impl, Impl, 1900>;

        virtual void on_text(Platform& pfrm,
                             App& app,
                             Self&,
                             Platform::RemoteConsole::Line& line);
    };


    Impl::Self impl_;
};



} // namespace skyland
