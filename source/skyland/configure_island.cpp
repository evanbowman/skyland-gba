#include "configure_island.hpp"
#include "room_metatable.hpp"



namespace skyland {



void configure_island(Platform& pfrm,
                      Island& island,
                      lisp::Value* island_desc_lat)
{
    island.rooms().clear();

    lisp::foreach (island_desc_lat, [&](lisp::Value* val) {
        auto name_symb = lisp::get_list(val, 0);
        if (name_symb->type_ not_eq lisp::Value::Type::symbol) {
            return;
        }
        u8 x = lisp::get_list(val, 1)->integer_.value_;
        u8 y = lisp::get_list(val, 2)->integer_.value_;

        if (auto c = load_metaclass(name_symb->symbol_.name_)) {
            (*c)->create(pfrm, &island, Vec2<u8>{x, y});
        }
    });

    island.repaint(pfrm);
}



void configure_island_from_codestring(Platform& pfrm,
                                      Island& island,
                                      const char* lisp_data)
{
    lisp::read(lisp_data); // leaves result of (read) at top of operand stack.

    configure_island(pfrm, island, lisp::get_op(0));

    lisp::pop_op();
}



} // namespace skyland
