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


#include "pool.hpp"
#include "allocator.hpp"
#include "platform/platform.hpp"



GenericPool* GenericPool::instances_;



void GenericPool::print_diagnostics(Platform& pfrm)
{
    auto pool = GenericPool::instances();
    auto output = allocate_dynamic<Platform::RemoteConsole::Line>(
        "pool-annotation-buffer");

    *output += "        name        |   size  |  total  |  used \r\n";
    *output += "____________________|_________|_________|________\r\n";
    *output += "                    |         |         |";
    *output += "\r\n";

    while (pool) {
        *output += pool->name();
        auto name_len = str_len(pool->name());
        if (name_len < 20) {
            *output += StringBuffer<20>(' ', 20 - name_len);
        }

        *output += "| ";
        auto size_str = stringify(pool->pooled_element_size());
        if (size_str.length() < 7) {
            *output += StringBuffer<7>(' ', 7 - size_str.length());
        }
        *output += size_str;
        *output += " | ";

        auto elem_count_str = stringify(pool->pooled_element_count());
        if (elem_count_str.length() < 7) {
            *output += StringBuffer<7>(' ', 7 - elem_count_str.length());
        }
        *output += elem_count_str;
        *output += " | ";

        auto remaining_str = stringify(pool->pooled_element_count() -
                                       pool->pooled_element_remaining());
        if (remaining_str.length() < 7) {
            *output += StringBuffer<7>(' ', 7 - remaining_str.length());
        }
        *output += remaining_str;
        *output += "\r\n";

        pool = pool->next();
    }

    pfrm.logger().log(Severity::info, output->c_str());
    pfrm.remote_console().printline(output->c_str(), "sc> ");
}