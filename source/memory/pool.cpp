////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "pool.hpp"
#include "allocator.hpp"


GenericPool* GenericPool::instances_;


#ifndef __TEST__
#include "platform/platform.hpp"


void GenericPool::print_diagnostics()
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
        auto name_len = strlen(pool->name());
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

    PLATFORM.logger().log(Severity::info, output->c_str());
    PLATFORM.remote_console().printline(output->c_str(), "sc> ");
}
#endif
