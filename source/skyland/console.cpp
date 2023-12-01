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
    }

    void put_str(const char* str) override
    {
        fmt_ += str;
    }

    Platform::RemoteConsole::Line fmt_;
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
                printer.fmt_.push_back('\a');
            }

            format(lisp::get_op(0), printer);

            lisp::pop_op();
            lisp::pop_op();

            PLATFORM.remote_console().printline(printer.fmt_.c_str(), "lisp> ");
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
                "rom dump               | dump entire rom as hex (slow)\r\n"
                "download <path>        | dump file to console, base32 encoded\r\n"
                "quit                   | select a different console mode\r\n"
                "call <path>            | invoke a lisp script\r\n"
                "ls <path>              | list files in a directory\r\n";
            // clang-format on
            PLATFORM.remote_console().printline(msg, "sc> ");
        } else if (line == "sbr annotate") {
            PLATFORM.system_call("print-memory-diagnostics", nullptr);
        } else if (parsed.size() == 3 and parsed[0] == "sbr" and
                   parsed[1] == "dump") {
            auto num = parsed[2].c_str();
            if (num[0] == '@') {
                ++num;
            }
            scratch_buffer_dump_sector(parse_int(num, str_len(num)));
        } else if (line == "pools annotate") {
            GenericPool::print_diagnostics();
        } else if (line == "quit") {
            PLATFORM.remote_console().printline("");
            PLATFORM.remote_console().printline(console_usage);
            self.emplace<ConsoleState::Impl>();
            return;
        } else if (line == "rom dump") {
            PLATFORM.remote_console().printline(
                "Dumping the entire rom as hex. "
                "This will take a couple hours...",
                "");
            PLATFORM.sleep(180);
            PLATFORM.system_call("dump-rom", nullptr);
            PLATFORM.remote_console().printline("\r\nDone!", "sc> ");
        } else if (parsed.size() == 2 and parsed[0] == "download") {
            Vector<char> data;
            if (flash_filesystem::read_file_data_binary(parsed[1].c_str(),
                                                        data)) {
                auto enc = base32::encode(data);
                PLATFORM.system_call("console-write-buffer", &enc);
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

        } else if (parsed.size() == 2 and parsed[0] == "call") {
            auto result = APP.invoke_script(parsed[1].c_str());
            RemoteConsoleLispPrinter printer;
            format(result, printer);
            PLATFORM.remote_console().printline(printer.fmt_.c_str(), "sc> ");
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
