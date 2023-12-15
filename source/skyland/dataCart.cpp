#include "dataCart.hpp"
#include "containers/vector.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"



namespace skyland
{



const char* DataCart::config() const
{
    auto f = PLATFORM.load_file_contents(
        "", format("scripts/misc/cart/cart%.ini", id_).c_str());

    if (not f) {
        Platform::fatal(format("missing cart %", id_).c_str());
    }

    return f;
}



DataCart::Name DataCart::name() const
{
    return get_label_string("name");
}



DataCart::Subheading DataCart::subheading() const
{
    return get_label_string("subheading");
}



DataCart::LabelString DataCart::get_label_string(const char* field) const
{
    auto f = config();
    Conf c;
    auto result = c.get(f, "label", field);
    if (auto val = std::get_if<Conf::String>(&result)) {
        return (*val)->c_str();
    } else {
        Platform::fatal(
            format("key % missing from label in cart% ini", field, id_)
                .c_str());
    }
}



DataCart::ContentString DataCart::expect_content_string(const char* field) const
{
    if (auto result = get_content_string(field)) {
        return std::move(*result);
    } else {
        Platform::fatal(
            format("key % missing from content in cart% ini", field, id_)
                .c_str());
    }
}



std::optional<DataCart::ContentString>
DataCart::get_content_string(const char* field) const
{
    auto f = config();
    Conf c;
    auto result = c.get(f, "contents", field);
    if (auto val = std::get_if<Conf::String>(&result)) {
        return std::move(*val);
    } else {
        return std::nullopt;
    }
}



static const char* const save_path = "/save/carts.dat";



// In case sram fails?
static u32 cart_lib_backup_data;



DataCartLibrary::DataCartLibrary() : carts_(cart_lib_backup_data)
{
    Vector<char> output;
    host_u32 input;

    const auto bytes_read =
        flash_filesystem::read_file_data_binary(save_path, output);

    if (bytes_read == sizeof(input)) {
        for (u32 i = 0; i < bytes_read; ++i) {
            ((u8*)&input)[i] = output[i];
        }

        carts_ = input.get();
    }

    Conf c;
    auto f = PLATFORM.load_file_contents("", "scripts/misc/cart/library.ini");
    if (f) {
        auto found = c.get(f, "info", "cart_count");
        if (auto val = std::get_if<Conf::Integer>(&found)) {
            max_carts_ = *val;
        }
    }
}



void DataCartLibrary::store(DataCart cart)
{
    if ((u32)cart.id() > sizeof(carts_) * 8 or (u32) cart.id() >= max_carts_) {
        Platform::fatal("cart id too high!");
    }

    if (load(cart.id())) {
        // No reason to write a file, we've already done so.
        return;
    }

    carts_ |= 1 << cart.id();
    cart_lib_backup_data = carts_;

    Vector<char> output;
    host_u32 d;
    d.set(carts_);

    for (u32 i = 0; i < sizeof d; ++i) {
        output.push_back(((u8*)&d)[i]);
    }

    flash_filesystem::store_file_data_binary(save_path, output);
}



std::optional<DataCart> DataCartLibrary::load(int id) const
{
    if (carts_ & (1 << id)) {
        return DataCart(id);
    }

    return {};
}



} // namespace skyland
