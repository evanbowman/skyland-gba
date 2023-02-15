#include "dataCart.hpp"
#include "containers/vector.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"



namespace skyland
{



const char* DataCart::config(Platform& pfrm) const
{
    auto f = pfrm.load_file_contents(
        "", format("scripts/misc/cart/cart%.ini", id_).c_str());

    if (not f) {
        Platform::fatal(format("missing cart %", id_).c_str());
    }

    return f;
}



DataCart::Name DataCart::name(Platform& pfrm) const
{
    return get_label_string(pfrm, "name");
}



DataCart::Subheading DataCart::subheading(Platform& pfrm) const
{
    return get_label_string(pfrm, "subheading");
}



DataCart::LabelString DataCart::get_label_string(Platform& pfrm,
                                                 const char* field) const
{
    auto f = config(pfrm);
    Conf c(pfrm);
    auto result = c.get(f, "label", field);
    if (auto val = std::get_if<Conf::String>(&result)) {
        return *val;
    } else {
        Platform::fatal(
            format("key % missing from label in cart% ini", field, id_)
                .c_str());
    }
}



DataCart::ContentString DataCart::get_content_string(Platform& pfrm,
                                                     const char* field) const
{
    auto f = config(pfrm);
    Conf c(pfrm);
    auto result = c.get(f, "contents", field);
    if (auto val = std::get_if<Conf::String>(&result)) {
        return *val;
    } else {
        Platform::fatal(
            format("key % missing from contents cart% ini", field, id_)
                .c_str());
    }
}



static const char* const save_path = "/save/carts.dat";



// In case sram fails?
static u32 cart_lib_backup_data;



DataCartLibrary::DataCartLibrary(Platform& pfrm) : carts_(cart_lib_backup_data)
{
    Vector<char> output;
    host_u32 input;

    const auto bytes_read =
        flash_filesystem::read_file_data_binary(pfrm, save_path, output);

    if (bytes_read == sizeof(input)) {
        for (u32 i = 0; i < bytes_read; ++i) {
            ((u8*)&input)[i] = output[i];
        }

        carts_ = input.get();
    }

    Conf c(pfrm);
    auto f = pfrm.load_file_contents("", "scripts/misc/cart/library.ini");
    if (f) {
        auto found = c.get(f, "info", "cart_count");
        if (auto val = std::get_if<Conf::Integer>(&found)) {
            max_carts_ = *val;
        }
    }
}



void DataCartLibrary::store(Platform& pfrm, DataCart cart)
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

    flash_filesystem::store_file_data_binary(pfrm, save_path, output);
}



std::optional<DataCart> DataCartLibrary::load(int id) const
{
    if (carts_ & (1 << id)) {
        return DataCart(id);
    }

    return {};
}



} // namespace skyland
