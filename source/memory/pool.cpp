////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
