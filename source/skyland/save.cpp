#include "save.hpp"
#include "flag.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "platform/ram_filesystem.hpp"



// NOTE: saving is a bit of a mess. Originally, we wrote stuff to fixed
// locations in SRAM. Now, we implement a filesystem in SRAM, but some things
// still need to be written to fixed locations at the beginning of SRAM, so that
// we don't break backwards compatibility.



namespace skyland {
namespace save {



static const u32 save_data_magic = 0xABCD + 2;
static const u32 global_save_data_magic = 0xABCD;



bool load_global_data(Platform& pfrm, GlobalPersistentData& data)
{
    GlobalSaveData loaded;

    pfrm.read_save_data(&loaded, sizeof loaded, 0);

    if (loaded.magic_.get() == global_save_data_magic) {
        data = loaded.data_;
        return true;
    }

    return false;
}



void store_global_data(Platform& pfrm, const GlobalPersistentData& data)
{
    GlobalSaveData out;
    out.magic_.set(global_save_data_magic);
    out.data_ = data;

    pfrm.write_save_data(&out, sizeof out, 0);
}



class LispPrinter : public lisp::Printer {
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



void store(Platform& pfrm, const PersistentData& d)
{
    LispPrinter p(pfrm);
    if (auto script = pfrm.load_file_contents("scripts", "save.lisp")) {
        lisp::read(script);
        lisp::eval(lisp::get_op(0));
        lisp::format(lisp::get_op(0), p);
        lisp::pop_op(); // result of eval()
        lisp::pop_op(); // result of read()
    }

    SaveData save_data;
    save_data.magic_.set(save_data_magic);

    // Assigned zero, so that save files produced by newer versions of the game
    // do not break when loaded into older versions.
    save_data.script_length_.set(0);

    memcpy(&save_data.data_, &d, sizeof d);

    u32 offset = sizeof(GlobalSaveData);

    pfrm.write_save_data(&save_data, sizeof save_data, offset);

    offset += sizeof save_data;

    ram_filesystem::store_file_data(pfrm,
                                    "/save/data.lisp",
                                    p.fmt_.c_str(),
                                    p.fmt_.length());
}



bool load(Platform& pfrm, PersistentData& d)
{
    u32 offset = sizeof(GlobalSaveData);

    SaveData save_data;
    pfrm.read_save_data(&save_data, sizeof save_data, offset);

    offset += sizeof save_data;

    if (save_data.magic_.get() not_eq save_data_magic) {
        return false;
    }


    auto sbr = pfrm.make_scratch_buffer();

    __builtin_memset(sbr->data_, 0, sizeof sbr->data_);

    auto bytes = ram_filesystem::read_file_data(pfrm,
                                                "/save/data.lisp",
                                                sbr);

    if (bytes == 0) {
        pfrm.fatal("failed to load save");
        return false;
    }

    memcpy(&d, &save_data.data_, sizeof d);

    lisp::read(sbr->data_);      // (0)
    lisp::eval(lisp::get_op(0)); // (1)

    // Sorry if all the pushes and pops are hard to follow. My lisp interpreter
    // has a very strict garbage collector, and everything that isn't stored in
    // a variable, or on the operand stack, could be collected. We read lisp
    // data from a string stored in sram, then, separately, we load a lambda
    // expression from another file, and invoke the lambda with the save data
    // list as an argument.

    auto arg = lisp::get_op(0); // result of eval()

    if (auto script = pfrm.load_file_contents("scripts", "restore_save.lisp")) {
        lisp::read(script);          // leaves result on stack (2)
        lisp::eval(lisp::get_op(0)); // eval result of read() (3)

        auto fn = lisp::get_op(0);
        if (fn->type() == lisp::Value::Type::function) {
            lisp::push_op(arg); // pass save data buffer on stack
            funcall(fn, 1);     // one argument (the save data)
            lisp::pop_op();     // funcall result
        } else {
            pfrm.fatal("not function!");
        }

        lisp::pop_op(); // result of eval() (3)
        lisp::pop_op(); // result of read() (2)
    }

    lisp::pop_op(); // result of eval() (1)
    lisp::pop_op(); // result of read() (0)


    return true;
}



void erase(Platform& pfrm)
{
    SaveData save_data;
    save_data.magic_.set(0);
    pfrm.write_save_data(&save_data, sizeof save_data, sizeof(GlobalSaveData));
}



} // namespace save
} // namespace skyland
