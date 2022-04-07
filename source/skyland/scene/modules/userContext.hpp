#pragma once

#include "vector.hpp"



namespace skyland
{



struct UserContext
{
    std::optional<Vector<char>> yank_buffer_;
};



} // namespace skyland
