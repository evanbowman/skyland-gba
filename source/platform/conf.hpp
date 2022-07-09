#pragma once

#include "platform.hpp"
#include "string.hpp"
#include <variant>


// This code is kind of junk. I realized that I needed an INI parser, and
// remembered that I'd written one years ago.



class Conf
{
public:
    Conf(Platform& pfrm) : pfrm_(pfrm)
    {
    }

    using Integer = int;
    using String = StringBuffer<31>;
    using Value = std::variant<std::monostate, Integer, String>;

    Value get(const char* section, const char* key);

    template <typename T> T expect(const char* section, const char* key)
    {
        const auto v = get(section, key);

        if (auto val = std::get_if<T>(&v)) {
            return *val;
        } else {
            Platform::fatal(
                format("in config ini: missing % from [%]", key, section)
                    .c_str());
        }
    }

    // While our configuration language doesn't natively support container
    // datatypes, it does offer a library abstraction for lists. If you define a
    // __next parameter in a section, you can use this function to iterate
    // through sections.
    template <typename F>
    void scan_list(const char* start_section, F&& callback)
    {
        Conf::String section;
        section = start_section;

        while (true) {
            callback(section.c_str());

            const auto next = get(section.c_str(), "__next");
            if (auto val = std::get_if<Conf::String>(&next)) {
                section = *val;
            } else {
                return;
            }
        }
    }

private:
    Platform& pfrm_;
};
