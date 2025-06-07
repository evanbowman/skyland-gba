////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "console.hpp"
#include "base32.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



const char* console_usage = "\aOptions: (s: simple console, l: lisp repl)";



class RemoteConsoleLispPrinter : public lisp::Printer
{
public:
    RemoteConsoleLispPrinter()
    {
        lines_.emplace_back();
    }

    void put_str(const char* str) override
    {
        while (*str not_eq '\0') {
            if (lines_.back().full()) {
                lines_.emplace_back();
            }
            lines_.back().push_back(*(str++));
        }
    }

    Vector<Platform::RemoteConsole::Line> lines_;
};



static auto split(const Platform::RemoteConsole::Line& line)
{
    Vector<StringBuffer<64>> result("console-parse-buffer");

    StringBuffer<64> current;

    auto pos = line.begin();
    while (pos not_eq line.end()) {
        if (*pos == ' ' and not current.empty()) {
            result.push_back(current, "console-parse-buffer");
            current.clear();
        } else {
            current.push_back(*pos);
        }
        ++pos;
    }

    if (not current.empty()) {
        result.push_back(current, "console-parse-buffer");
    }

    return result;
}



class LispConsoleImpl : public ConsoleState::Impl
{
public:
    void on_text(Self& self, Platform::RemoteConsole::Line& line) override
    {
        PLATFORM.remote_console().printline("", "");
        PLATFORM.sleep(2);

        if (line == "(quit)") {
            self.emplace<ConsoleState::Impl>();
            PLATFORM.remote_console().printline(console_usage);
            return;
        } else {
            RemoteConsoleLispPrinter printer;

            lisp::BasicCharSequence seq(line.c_str());
            lisp::read(seq);
            lisp::eval(lisp::get_op(0));

            if (lisp::get_op(0)->type() == lisp::Value::Type::error) {
                printer.lines_.back().push_back('\a');
            }

            format(lisp::get_op(0), printer);

            lisp::pop_op();
            lisp::pop_op();

            if (printer.lines_.size() == 1) {
                PLATFORM.remote_console().printline(printer.lines_[0].c_str(),
                                                    "lisp> ");
            } else {
                for (auto& l : printer.lines_) {
                    PLATFORM.remote_console().printline(l.c_str(), "\n");
                    PLATFORM.sleep(120);
                }
                PLATFORM.remote_console().printline("", "lisp> ");
            }
        }
    }
};



class ShellConsoleImpl : public ConsoleState::Impl
{
public:
    void on_text(Self& self, Platform::RemoteConsole::Line& line) override
    {
        PLATFORM.remote_console().printline("", "");
        PLATFORM.sleep(2);

        auto parsed = split(line);

        if (line == "help") {
            // clang-format off
            const char* msg =
                "help                   | show this help message\r\n"
                "pools annotate         | show memory pool statistics\r\n"
                "sbr annotate           | show memory buffers in use\r\n"
                "sbr dump @<buffer id>  | dump memory buffer as hex\r\n"
                "download <path>        | dump file to console, base32 encoded\r\n"
                "quit                   | select a different console mode\r\n"
                "ls <path>              | list files in a directory\r\n";
            // clang-format on
            PLATFORM.remote_console().printline(msg, "sc> ");
        } else if (line == "sbr annotate") {
            scratch_buffer_memory_diagnostics();
        } else if (parsed.size() == 3 and parsed[0] == "sbr" and
                   parsed[1] == "dump") {
            auto num = parsed[2].c_str();
            if (num[0] == '@') {
                ++num;
            }
            scratch_buffer_dump_sector(parse_int(num, strlen(num)));
        } else if (line == "pools annotate") {
            GenericPool::print_diagnostics();
        } else if (line == "quit") {
            PLATFORM.remote_console().printline("");
            PLATFORM.remote_console().printline(console_usage);
            self.emplace<ConsoleState::Impl>();
            return;
        } else if (parsed.size() == 2 and parsed[0] == "download") {
            Vector<char> data;
            if (flash_filesystem::read_file_data_binary(parsed[1].c_str(),
                                                        data)) {
                auto enc = base32::encode(data);
                PLATFORM_EXTENSION(console_write_buffer, enc);
                PLATFORM.remote_console().printline("\r\nComplete!", "sc> ");
            } else {
                PLATFORM.remote_console().printline("file not found!", "sc> ");
            }
        } else if (parsed.size() == 2 and parsed[0] == "ls") {
            flash_filesystem::walk_directory(
                parsed[1].c_str(), [&](const char* path) {
                    PLATFORM.remote_console().printline(path, "");
                    PLATFORM.sleep(2);
                });

            PLATFORM.walk_filesystem([&](const char* path) {
                StringBuffer<64> prefix(parsed[1].c_str());
                if (starts_with(prefix.c_str(), StringBuffer<64>(path))) {
                    PLATFORM.remote_console().printline(path, "");
                    PLATFORM.sleep(2);
                }
            });
            PLATFORM.sleep(1);
            PLATFORM.remote_console().printline("\r\n", "sc> ");

        } else {
            PLATFORM.remote_console().printline("error: type help for options",
                                                "sc> ");
        }
    }
};



void ConsoleState::Impl::on_text(Self& self,
                                 Platform::RemoteConsole::Line& line)
{
    PLATFORM.remote_console().printline("", "");
    PLATFORM.sleep(2);

    if (line.length() == 1 and line[0] == 's') {
        const char* hint = "Simple Console ready, type help to list commands";
        PLATFORM.remote_console().printline(hint, "sc> ");
        self.emplace<ShellConsoleImpl>();
        return;
    } else if (line.length() == 1 and line[0] == 'l') {
        PLATFORM.remote_console().printline(
            "Skyland LISP ready! (quit) to exit. Try (help), or (apropos "
            "\"...\")",
            "lisp> ");
        self.emplace<LispConsoleImpl>();
        return;
    } else {
        PLATFORM.remote_console().printline(console_usage);
    }
}



} // namespace skyland
