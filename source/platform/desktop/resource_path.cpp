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
