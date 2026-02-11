#include "settings.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/skyland.hpp"



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
    bool assigned = false;
    lisp::l_foreach(data_, [key, value, &assigned](lisp::Value* kvp) {
        if (kvp->type() == lisp::Value::Type::cons) {
            auto l_key = kvp->cons().car();
            if (l_key->type() == lisp::Value::Type::symbol and
                str_eq(l_key->symbol().name(), key)) {
                if (value == nullptr or strlen(value) == 0) {
                    kvp->cons().set_cdr(L_NIL);
                } else {
                    kvp->cons().set_cdr(lisp::make_string(value));
                }
                assigned = true;
            }
        }
    });
    if (not assigned) {
        lisp::Protected new_kvp = L_CONS(L_SYM(key), L_STRING(value));
        lisp::Protected new_l = L_CONS(new_kvp, data_);
        data_ = (lisp::Value*)new_l;
    }
}



void Settings::save()
{
    lisp::_Printer<Vector<char>> output;
    lisp::format(data_, output);
    output.data_.push_back('\0');
    flash_filesystem::store_file_data_text(settings_path, output.data_);
}



void apply()
{
    Settings settings;
    load(settings);

    if (auto cm = PLATFORM.get_extensions().apply_color_correction) {
        auto col = settings.get("color-profile");
        if (col.length()) {
            cm(col.c_str());
        }
    }

    if (settings.get("rumble") == "on") {
        state_bit_store(StateBit::rumble_enabled, true);
    }

    if (settings.get("lighting") == "on") {
        state_bit_store(StateBit::lighting_enabled, true);
    }

    if (auto map = PLATFORM.get_extensions().map_button) {
#define GET_S(STR) (settings.get(STR).c_str())
        // clang-format off
        map(Button::action_1, GET_S("key_action1"));
        map(Button::action_2, GET_S("key_action2"));
        map(Button::alt_1,    GET_S("key_alt1"));
        map(Button::alt_2,    GET_S("key_alt2"));
        map(Button::start,    GET_S("key_start"));
        map(Button::select,   GET_S("key_select"));
        map(Button::up,       GET_S("key_up"));
        map(Button::down,     GET_S("key_down"));
        map(Button::left,     GET_S("key_left"));
        map(Button::right,    GET_S("key_right"));
        // clang-format on
    }
}



}
