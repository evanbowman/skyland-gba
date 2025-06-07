////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "save.hpp"
#include "flag.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "skyland.hpp"



namespace skyland
{
namespace save
{



static const u32 save_data_magic = 0xABCD + 3;
static const u32 global_save_data_magic = 0xABCD + 2;



const char* global_data_filename = "/save/global.dat";
const char* save_data_filename = "/save/adventure.dat";
const char* save_data_lisp_filename = "/save/adventure.lisp";



bool load_global_data(GlobalPersistentData& data)
{
    Vector<char> read;

    auto byte_count =
        flash_filesystem::read_file_data_binary(global_data_filename, read);

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



void store_global_data(const GlobalPersistentData& data)
{
    GlobalSaveData out;
    out.magic_.set(global_save_data_magic);
    out.data_ = data;

    auto out_ptr = (char*)&out;

    Vector<char> buffer;
    for (u32 i = 0; i < sizeof(out); ++i) {
        buffer.push_back(*(out_ptr++));
    }
    flash_filesystem::StorageOptions opts{.use_compression_ = true};
    flash_filesystem::store_file_data_binary(
        global_data_filename, buffer, opts);
}



class LispPrinter : public lisp::Printer
{
public:
    LispPrinter(Vector<char>& v) : v_(v)
    {
    }

    void put_str(const char* str) override
    {
        while (*str not_eq '\0') {
            v_.push_back(*str);
            str++;
        }
    }

    Vector<char>& v_;
};



void EmergencyBackup::init()
{
    persistent_data_ = APP.persistent_data();

    lisp::gc();

    lisp_data_.emplace();

    LispPrinter p(*lisp_data_);
    auto val = APP.invoke_script("/scripts/save.lisp");
    lisp::format(val, p);

    lisp_data_->push_back('\0');

    rng_state_ = rng::critical_state;

    valid_ = true;
}



static void store(const char* prefix_path, const SaveData& sd)
{
    StringBuffer<128> opath;
    opath += prefix_path;
    opath += save_data_filename;

    Vector<char> out_buffer;
    auto out_ptr = (const u8*)&sd;
    for (u32 i = 0; i < sizeof sd; ++i) {
        out_buffer.push_back(*(out_ptr++));
    }
    flash_filesystem::StorageOptions opts{.use_compression_ = true};
    flash_filesystem::store_file_data_binary(opath.c_str(), out_buffer, opts);
}



void EmergencyBackup::store()
{
    if (not valid_) {
        return;
    }

    SaveData save_data;
    save_data.magic_.set(save_data_magic);


    save_data.script_length_.set(0);

    save_data.data_ = persistent_data_;

    save::store("", save_data);

    flash_filesystem::StorageOptions opts{.use_compression_ = true};
    flash_filesystem::store_file_data_text(
        save_data_lisp_filename, *lisp_data_, opts);
}



void store(const char* prefix_path, const PersistentData& d)
{
    lisp::_Printer<Vector<char>> p;
    auto val = APP.invoke_script("/scripts/save.lisp");
    lisp::format(val, p);
    p.data_.push_back('\0');

    SaveData save_data;
    save_data.magic_.set(save_data_magic);

    // Assigned zero, so that save files produced by newer versions of the game
    // do not break when loaded into older versions.
    save_data.script_length_.set(0);

    memcpy(&save_data.data_, &d, sizeof d);
    save_data.data_.rng_.set(rng::critical_state);

    if (APP.is_developer_mode()) {
        save_data.data_.set_flag(PersistentData::StateFlag::dev_mode_active);
    }

    store(prefix_path, save_data);

    StringBuffer<128> opath;
    opath += prefix_path;
    opath += save_data_lisp_filename;

    flash_filesystem::StorageOptions opts{.use_compression_ = true};
    flash_filesystem::store_file_data_text(opath.c_str(), p.data_, opts);

    synth_notes_store(APP.player_island(), "/save/synth.dat");
    speaker_data_store(APP.player_island(), "/save/speaker.dat");
}



bool load(const char* prefix_path, PersistentData& d)
{
    Vector<char> data;

    const auto byte_count =
        flash_filesystem::read_file_data_binary(save_data_filename, data);

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
        rng::critical_state = d.rng_.get();

        if (APP.is_developer_mode()) {
            d.set_flag(PersistentData::StateFlag::dev_mode_active);
        }

    } else {
        return false;
    }

    data.clear();


    auto bytes =
        flash_filesystem::read_file_data_text(save_data_lisp_filename, data);

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

    auto fn = APP.invoke_script("/scripts/restore_save.lisp");
    if (fn->type() == lisp::Value::Type::function) {
        lisp::push_op(arg);    // pass save data buffer on stack
        lisp::safecall(fn, 1); // one argument (the save data)
        lisp::pop_op();        // funcall result
    } else {
        PLATFORM.fatal("not function!");
    }

    lisp::pop_op(); // result of eval() (1)
    lisp::pop_op(); // result of read() (0)


    synth_notes_load(APP.player_island(), "/save/synth.dat");
    speaker_data_load(APP.player_island(), "/save/speaker.dat");

    return true;
}



void erase()
{
    flash_filesystem::unlink_file(save_data_filename);
}



} // namespace save
} // namespace skyland
