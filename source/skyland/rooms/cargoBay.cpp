#include "cargoBay.hpp"
#include "skyland/tile.hpp"
#include <string.h>
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"



namespace skyland {



CargoBay::CargoBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
    set_cargo("", 0);
}



void CargoBay::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cargo_bay)->c_str();
}



bool CargoBay::set_cargo(const char* cargo, u8 count)
{
    if (str_len(cargo) + 1 > sizeof cargo_) {
        return false;
    }

    count_ = count;

    auto dest = cargo_;
    auto src = cargo;

    while (*src not_eq '\0') {
        *dest++ = *src++;
    }
    *dest = '\0';

    return true;
}



void CargoBay::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void CargoBay::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cargo_bay;
    buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
}



void CargoBay::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



lisp::Value* CargoBay::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    builder.push_back(lisp::make_string(cargo()));

    if (health() not_eq max_health()) {
        builder.push_back(lisp::make_integer(health()));
    }

    return builder.result();
}



void CargoBay::deserialize(lisp::Value* list)
{
    auto c = lisp::get_list(list, 4);
    if (c->type() == lisp::Value::Type::string) {
        set_cargo(c->string().value(), str_len(c->string().value()));
    }

    if (lisp::length(list) >= 4) {
        __set_health(lisp::get_list(list, 5)->integer().value_);
    }
}



} // namespace skyland
