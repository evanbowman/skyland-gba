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


#include "save.hpp"
#include "flag.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland.hpp"



// NOTE: saving is a bit of a mess. Originally, we wrote stuff to fixed
// locations in SRAM. Now, we implement a filesystem in SRAM, but some things
// still need to be written to fixed locations at the beginning of SRAM, so that
// we don't break backwards compatibility.



namespace skyland
{
namespace save
{



static const u32 save_data_magic = 0xABCD + 3;
static const u32 global_save_data_magic = 0xABCD + 2;



const char* global_data_filename = "/save/global.dat";
const char* save_data_filename = "/save/meta.dat";



bool load_global_data(Platform& pfrm, GlobalPersistentData& data)
{
    Vector<char> read;

    auto byte_count = flash_filesystem::read_file_data_binary(
        pfrm, global_data_filename, read);

    if (byte_count == sizeof(GlobalSaveData)) {
        // Read successful!

        GlobalSaveData loaded;

        auto it = read.begin();
        auto write = (u8*)&loaded;

        while (it not_eq read.end()) {
            *(write++) = *it;
            ++it;
        }

        if (loaded.magic_.get() == global_save_data_magic) {
            data = loaded.data_;
            return true;
        }
    }

    return false;
}



void store_global_data(Platform& pfrm, const GlobalPersistentData& data)
{
    GlobalSaveData out;
    out.magic_.set(global_save_data_magic);
    out.data_ = data;

    auto out_ptr = (char*)&out;

    Vector<char> buffer;
    for (u32 i = 0; i < sizeof(out); ++i) {
        buffer.push_back(*(out_ptr++));
    }
    flash_filesystem::store_file_data_binary(
        pfrm, global_data_filename, buffer);
}



class LispPrinter : public lisp::Printer
{
public:
    LispPrinter(Platform& pfrm) : pfrm_(pfrm)
    {
    }

    void put_str(const char* str) override
    {
        fmt_ += str;
    }

    StringBuffer<1024> fmt_;
    Platform& pfrm_;
};



void EmergencyBackup::init(Platform& pfrm, App& app)
{
    persistent_data_ = app.persistent_data();

    LispPrinter p(pfrm);
    auto val = app.invoke_script(pfrm, "/scripts/save.lisp");
    lisp::format(val, p);

    lisp_data_ = p.fmt_;

    valid_ = true;
}



static void store(Platform& pfrm, const SaveData& sd)
{
    Vector<char> out_buffer;
    auto out_ptr = (const u8*)&sd;
    for (u32 i = 0; i < sizeof sd; ++i) {
        out_buffer.push_back(*(out_ptr++));
    }
    flash_filesystem::store_file_data_binary(
        pfrm, save_data_filename, out_buffer);
}



void EmergencyBackup::store(Platform& pfrm)
{
    SaveData save_data;
    save_data.magic_.set(save_data_magic);


    save_data.script_length_.set(0);

    save_data.data_ = persistent_data_;

    save::store(pfrm, save_data);

    flash_filesystem::store_file_data(
        pfrm, "/save/data.lisp", lisp_data_.c_str(), lisp_data_.length());
}



void store(Platform& pfrm, App& app, const PersistentData& d)
{
    lisp::_Printer<Vector<char>> p;
    auto val = app.invoke_script(pfrm, "/scripts/save.lisp");
    lisp::format(val, p);
    p.data_.push_back('\0');

    SaveData save_data;
    save_data.magic_.set(save_data_magic);

    // Assigned zero, so that save files produced by newer versions of the game
    // do not break when loaded into older versions.
    save_data.script_length_.set(0);

    memcpy(&save_data.data_, &d, sizeof d);

    store(pfrm, save_data);

    flash_filesystem::store_file_data_text(pfrm, "/save/data.lisp", p.data_);

    synth_notes_store(pfrm, app.player_island(), "/save/synth.dat");
    speaker_data_store(pfrm, app.player_island(), "/save/speaker.dat");

    // NOTE: sram-flash-writeback persists save media to flash memory. Required
    // on reprogrammed gba cartridges, where sram is volatile and data needs to
    // be manually written back to flash.
    pfrm.system_call("sram-flash-writeback", nullptr);
}



bool load(Platform& pfrm, App& app, PersistentData& d)
{
    Vector<char> data;

    const auto byte_count =
        flash_filesystem::read_file_data_binary(pfrm, save_data_filename, data);

    if (byte_count == sizeof(SaveData)) {
        SaveData save_data;

        auto it = data.begin();
        auto write = (u8*)&save_data;

        while (it not_eq data.end()) {
            *(write++) = *it;
            ++it;
        }

        if (save_data.magic_.get() not_eq save_data_magic) {
            return false;
        }

        memcpy(&d, &save_data.data_, sizeof d);
    } else {
        return false;
    }

    data.clear();


    auto bytes =
        flash_filesystem::read_file_data_text(pfrm, "/save/data.lisp", data);

    if (bytes == 0) {
        return false;
    }

    lisp::VectorCharSequence seq(data);
    lisp::read(seq);             // (0)
    lisp::eval(lisp::get_op(0)); // (1)

    // Sorry if all the pushes and pops are hard to follow. My lisp interpreter
    // has a very strict garbage collector, and everything that isn't stored in
    // a variable, or on the operand stack, could be collected. We read lisp
    // data from a string stored in sram, then, separately, we load a lambda
    // expression from another file, and invoke the lambda with the save data
    // list as an argument.

    auto arg = lisp::get_op(0); // result of eval()

    auto fn = app.invoke_script(pfrm, "/scripts/restore_save.lisp");
    if (fn->type() == lisp::Value::Type::function) {
        lisp::push_op(arg); // pass save data buffer on stack
        funcall(fn, 1);     // one argument (the save data)
        lisp::pop_op();     // funcall result
    } else {
        pfrm.fatal("not function!");
    }

    lisp::pop_op(); // result of eval() (1)
    lisp::pop_op(); // result of read() (0)


    synth_notes_load(pfrm, app.player_island(), "/save/synth.dat");
    speaker_data_load(pfrm, app.player_island(), "/save/speaker.dat");

    return true;
}



void erase(Platform& pfrm)
{
    flash_filesystem::unlink_file(pfrm, save_data_filename);
}



} // namespace save
} // namespace skyland
