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
    RemoteConsoleLispPrinter(Platform& pfrm) : pfrm_(pfrm)
    {
    }

    void put_str(const char* str) override
    {
        fmt_ += str;
    }

    Platform::RemoteConsole::Line fmt_;
    Platform& pfrm_;
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
    void on_text(Platform& pfrm,
                 App& app,
                 Self& self,
                 Platform::RemoteConsole::Line& line) override
    {
        pfrm.remote_console().printline("", "");
        pfrm.sleep(2);

        if (line == "(quit)") {
            self.emplace<ConsoleState::Impl>();
            pfrm.remote_console().printline(console_usage);
            return;
        } else {
            RemoteConsoleLispPrinter printer(pfrm);

            lisp::BasicCharSequence seq(line.c_str());
            lisp::read(seq);
            lisp::eval(lisp::get_op(0));

            if (lisp::get_op(0)->type() == lisp::Value::Type::error) {
                printer.fmt_.push_back('\a');
            }

            format(lisp::get_op(0), printer);

            lisp::pop_op();
            lisp::pop_op();

            pfrm.remote_console().printline(printer.fmt_.c_str(), "lisp> ");
        }
    }
};



class ShellConsoleImpl : public ConsoleState::Impl
{
public:
    void on_text(Platform& pfrm,
                 App& app,
                 Self& self,
                 Platform::RemoteConsole::Line& line) override
    {
        pfrm.remote_console().printline("", "");
        pfrm.sleep(2);

        auto parsed = split(line);

        if (line == "help") {
            // clang-format off
            const char* msg =
                "help                   | show this help message\r\n"
                "pools annotate         | show memory pool statistics\r\n"
                "sbr annotate           | show memory buffers in use\r\n"
                "sbr dump @<buffer id>  | dump memory buffer as hex\r\n"
                "rom dump               | dump entire rom as hex (slow)\r\n"
                "download <path>        | dump file to console, base32 encoded\r\n"
                "quit                   | select a different console mode\r\n"
                "call <path>            | invoke a lisp script\r\n"
                "ls <path>              | list files in a directory\r\n";
            // clang-format on
            pfrm.remote_console().printline(msg, "sc> ");
        } else if (line == "sbr annotate") {
            pfrm.system_call("print-memory-diagnostics", nullptr);
        } else if (parsed.size() == 3 and parsed[0] == "sbr" and
                   parsed[1] == "dump") {
            auto num = parsed[2].c_str();
            if (num[0] == '@') {
                ++num;
            }
            scratch_buffer_dump_sector(pfrm, parse_int(num, str_len(num)));
        } else if (line == "pools annotate") {
            GenericPool::print_diagnostics(pfrm);
        } else if (line == "quit") {
            pfrm.remote_console().printline("");
            pfrm.remote_console().printline(console_usage);
            self.emplace<ConsoleState::Impl>();
            return;
        } else if (line == "rom dump") {
            pfrm.remote_console().printline("Dumping the entire rom as hex. "
                                            "This will take a couple hours...",
                                            "");
            pfrm.sleep(180);
            pfrm.system_call("dump-rom", nullptr);
            pfrm.remote_console().printline("\r\nDone!", "sc> ");
        } else if (parsed.size() == 2 and parsed[0] == "download") {
            Vector<char> data;
            if (flash_filesystem::read_file_data_binary(
                    pfrm, parsed[1].c_str(), data)) {
                auto enc = base32::encode(data);
                pfrm.system_call("console-write-buffer", &enc);
                pfrm.remote_console().printline("\r\nComplete!", "sc> ");
            } else {
                pfrm.remote_console().printline("file not found!", "sc> ");
            }
        } else if (parsed.size() == 2 and parsed[0] == "ls") {
            flash_filesystem::walk_directory(
                pfrm, parsed[1].c_str(), [&](const char* path) {
                    pfrm.remote_console().printline(path, "");
                    pfrm.sleep(2);
                });

            pfrm.walk_filesystem([&](const char* path) {
                StringBuffer<64> prefix(parsed[1].c_str());
                if (starts_with(prefix.c_str(), StringBuffer<64>(path))) {
                    pfrm.remote_console().printline(path, "");
                    pfrm.sleep(2);
                }
            });
            pfrm.sleep(1);
            pfrm.remote_console().printline("\r\n", "sc> ");

        } else if (parsed.size() == 2 and parsed[0] == "call") {
            auto result = app.invoke_script(pfrm, parsed[1].c_str());
            RemoteConsoleLispPrinter printer(pfrm);
            format(result, printer);
            pfrm.remote_console().printline(printer.fmt_.c_str(), "sc> ");
        } else {
            pfrm.remote_console().printline("error: type help for options",
                                            "sc> ");
        }
    }
};



void ConsoleState::Impl::on_text(Platform& pfrm,
                                 App& app,
                                 Self& self,
                                 Platform::RemoteConsole::Line& line)
{
    pfrm.remote_console().printline("", "");
    pfrm.sleep(2);

    if (line.length() == 1 and line[0] == 's') {
        const char* hint = "Simple Console ready, type help to list commands";
        pfrm.remote_console().printline(hint, "sc> ");
        self.emplace<ShellConsoleImpl>();
        return;
    } else if (line.length() == 1 and line[0] == 'l') {
        pfrm.remote_console().printline("Skyland LISP ready! (quit) to exit.",
                                        "lisp> ");
        self.emplace<LispConsoleImpl>();
        return;
    } else {
        pfrm.remote_console().printline(console_usage);
    }
}



} // namespace skyland
