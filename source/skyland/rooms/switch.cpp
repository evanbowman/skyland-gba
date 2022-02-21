#include "switch.hpp"
#include "skyland/tile.hpp"
#include "skyland/island.hpp"
#include "script/listBuilder.hpp"



namespace skyland {



void Switch::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_switch)->c_str();
}



ScenePtr<Scene> Switch::select(Platform& pfrm,
                               App& app,
                               const Vec2<u8>& cursor)
{
    if (Vec2<u8>{u8(cursor.x - 1), cursor.y} == position()) {
        on_ = not on_;
        parent()->repaint(pfrm, app);
    } else {
        // TODO...
    }

    return null_scene();
}




Switch::Switch(Island* parent, const Vec2<u8>& position)
    : Decoration(parent, name(), position)
{
}



lisp::Value* Switch::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    builder.push_back(L_CONS(L_INT(branch_1_.x),
                             L_INT(branch_1_.y)));

    builder.push_back(L_CONS(L_INT(branch_2_.x),
                             L_INT(branch_2_.y)));

    if (health() not_eq max_health()) {
        builder.push_back(lisp::make_integer(health()));
    }

    return builder.result();
}



void Switch::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 6) {
        __set_health(lisp::get_list(list, 5)->integer().value_);
    }

    // TODO: read branch_1_ and branch_2_ from list!
}



void Switch::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = InteriorTile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = InteriorTile::switch_off;
    }
}



void Switch::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = Tile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = Tile::switch_off;
    }
}



} // namespace skyland
