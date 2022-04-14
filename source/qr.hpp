#pragma once

#include "platform/scratch_buffer.hpp"



namespace skyland
{



class QRCode
{
public:

    static std::optional<QRCode> create(const char* text);


    bool get_module(const Vec2<int>& position) const;


    using Sidelength = int;


    Sidelength size() const;


private:

    QRCode(ScratchBufferPtr qr_data_);


    ScratchBufferPtr qr_data_;
};




}
