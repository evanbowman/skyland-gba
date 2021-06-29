#include "save.hpp"
#include "script/lisp.hpp"
#include "platform/platform.hpp"



namespace skyland {
namespace save {



static const u32 save_data_magic = 0xABCD;



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

    pfrm.write_save_data(&save_data, sizeof save_data, 0);
    pfrm.write_save_data(p.fmt_.c_str(), p.fmt_.length(), sizeof save_data);
}



bool load(Platform& pfrm, PersistentData& d)
{
    SaveData save_data;
    pfrm.read_save_data(&save_data, sizeof save_data, 0);

    if (save_data.magic_.get() not_eq save_data_magic) {
        return false;
    }

    static const int buffer_size = 1024;
    char buffer[buffer_size];
    __builtin_memset(buffer, '\0', sizeof buffer);

    if (buffer_size > save_data.script_length_.get()) {
        pfrm.read_save_data(&buffer,
                            save_data.script_length_.get(),
                            sizeof save_data);


    } else {
        return false;
    }

    memcpy(&d, &save_data.data_, sizeof d);

    lisp::read(buffer); // (0)
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
            funcall(fn, 1); // one argument (the save data)
            lisp::pop_op(); // funcall result
        } else {
            pfrm.fatal("not function!");
        }

        lisp::pop_op(); // result of eval() (3)
        lisp::pop_op(); // result of read() (2)
    }

    lisp::pop_op(); // result of eval() (1)
    lisp::pop_op(); // result of read() (0)


    return false;
}



void erase(Platform& pfrm)
{
    SaveData d;
    d.magic_.set(0);
    pfrm.write_save_data(&d, sizeof d, 0);
}



}
}
