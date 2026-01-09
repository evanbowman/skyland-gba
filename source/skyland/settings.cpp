#include "settings.hpp"
#include "skyland/skyland.hpp"
#include "platform/flash_filesystem.hpp"



namespace skyland::settings
{



static const char* const settings_path = "/save/settings.lisp";



Settings::Settings() : data_(L_NIL)
{
}



void load(Settings& output)
{
    Vector<char> buffer;
    if (flash_filesystem::read_file_data_text(settings_path, buffer)) {
        lisp::VectorCharSequence seq(buffer);
        output.data_ = lisp::dostring(seq, [](lisp::Value& v) {
            if (APP.is_developer_mode()) {
                PLATFORM.fatal("settings corrupted");
            }
        });
    } else {
        output.data_ = APP.invoke_script("/scripts/config/settings.lisp");
    }
}



StringBuffer<96> Settings::get(const char* key)
{
    StringBuffer<96> result;

    if (is_list(data_)) {
        lisp::l_foreach(data_, [&result, key](lisp::Value* kvp) {
            if (kvp->type() == lisp::Value::Type::cons) {
                auto l_key = kvp->cons().car();
                if (l_key->type() == lisp::Value::Type::symbol and
                    str_eq(l_key->symbol().name(), key)) {
                    auto cdr = kvp->cons().cdr();
                    if (cdr->type() == lisp::Value::Type::string) {
                        result = cdr->string().value();
                    }
                }
            }
        });
    }

    return result;
}



void Settings::set(const char* key, const char* value)
{
    lisp::l_foreach(data_, [key, value](lisp::Value* kvp) {
        if (kvp->type() == lisp::Value::Type::cons) {
            auto l_key = kvp->cons().car();
            if (l_key->type() == lisp::Value::Type::symbol and
                str_eq(l_key->symbol().name(), key)) {
                if (value == nullptr or strlen(value) == 0) {
                    kvp->cons().set_cdr(L_NIL);
                } else {
                    kvp->cons().set_cdr(lisp::make_string(value));
                }
            }
        }
    });
}



void Settings::save()
{
    lisp::_Printer<Vector<char>> output;
    lisp::format(data_, output);
    output.data_.push_back('\0');
    flash_filesystem::store_file_data_text(settings_path, output.data_);
}



}
