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


#include <iostream>
#include <string>

#if defined(_WIN32) or defined(_WIN64)
#include <windows.h>
std::string resource_path()
{
    static std::string result;
    if (result.empty()) {
        HMODULE hModule = GetModuleHandleW(nullptr);
        char buffer[MAX_PATH];
        GetModuleFileName(hModule, buffer, MAX_PATH);
        const std::string path(buffer);
        const std::size_t last_fwd_slash = path.find_last_of('\\');
        std::string path_without_binary = path.substr(0, last_fwd_slash + 1);
        result = path_without_binary + "..\\..\\";
    }
    return result;
}

#elif defined(__APPLE__)
#include <CoreFoundation/CoreFoundation.h>
#include <objc/objc-runtime.h>
#include <objc/objc.h>


std::string resource_path()
{
    id pool = reinterpret_cast<id>(objc_getClass("NSAutoreleasePool"));
    std::string rpath;
    if (not pool) {
        return rpath;
    }
    pool = ((id(*)(id, SEL))objc_msgSend)(pool, sel_registerName("alloc"));
    if (not pool) {
        return rpath;
    }
    pool = ((id(*)(id, SEL))objc_msgSend)(pool, sel_registerName("init"));
    id bundle = ((id(*)(id, SEL))objc_msgSend)(
        reinterpret_cast<id>(objc_getClass("NSBundle")),
        sel_registerName("mainBundle"));
    if (bundle) {
        id path = ((id(*)(id, SEL))objc_msgSend)(
            bundle, sel_registerName("resourcePath"));
        rpath = reinterpret_cast<const char*>(((id(*)(id, SEL))objc_msgSend)(
                    path, sel_registerName("UTF8String"))) +
                std::string("/");
    }
    ((id(*)(id, SEL))objc_msgSend)(pool, sel_registerName("drain"));
    return rpath + "../";
}

#elif __linux__
#include <linux/limits.h>
#include <unistd.h>

std::string resource_path()
{
    static std::string result;
    if (result.empty()) {
        char buffer[PATH_MAX];
        for (int i = 0; i < PATH_MAX; ++i) {
            buffer[i] = '\0';
        }
        [[gnu::unused]] const std::size_t bytes_read =
            readlink("/proc/self/exe", buffer, sizeof(buffer));

        buffer[PATH_MAX - 1] = '\0';
        const std::string path(buffer);
        const std::size_t last_fwd_slash = path.find_last_of("/");
        std::string path_without_binary = path.substr(0, last_fwd_slash + 1);
        result = path_without_binary + "../";
    }
    return result;
}
#endif
