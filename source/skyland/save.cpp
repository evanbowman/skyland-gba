#include "save.hpp"
#include "platform/platform.hpp"
#include "script/lisp.hpp"
#include "flag.hpp"



namespace skyland {
namespace save {



struct SaveData {
    HostInteger<u32> magic_;
    PersistentData data_;

    // We have some persistent data used by the application (above). But, we
    // also want to serialize a bunch of data used by the lisp interpreter,
    // and just eval it later.
    HostInteger<u32> script_length_;
    // u8 script_[...]; variable-sized data to follow...
};



static_assert(std::is_trivially_copyable<SaveData>::value,
              "SaveData will be memcpy'd to the output destination, and "
              "therefore must be trivially copyable.");





static const u32 save_data_magic = 0xABCD + 2;
static const u32 global_save_data_magic = 0xABCD;



struct GlobalSaveData {
    HostInteger<u32> magic_;
    GlobalPersistentData data_;
};



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
    save_data.script_length_.set(p.fmt_.length());
    memcpy(&save_data.data_, &d, sizeof d);

    u32 offset = sizeof(GlobalSaveData);

    pfrm.write_save_data(&save_data, sizeof save_data, offset);

    offset += sizeof save_data;

    pfrm.write_save_data(p.fmt_.c_str(), p.fmt_.length(), offset);
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

    static const int buffer_size = 1024;
    char buffer[buffer_size];
    __builtin_memset(buffer, '\0', sizeof buffer);

    if (buffer_size > save_data.script_length_.get()) {
        pfrm.read_save_data(&buffer, save_data.script_length_.get(), offset);


    } else {
        return false;
    }

    memcpy(&d, &save_data.data_, sizeof d);

    lisp::read(buffer);          // (0)
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
        if (fn->type_ == lisp::Value::Type::function) {
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
