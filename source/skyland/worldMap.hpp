#pragma once


#include "number/numeric.hpp"



namespace skyland {



struct WorldMap {
    struct Node {
        enum Type {
            null,
            clear,
            storm,
            hostile,
        } type_ = clear;

        struct Connections {
            enum : u8 {
                l = 1 << 0,
                r = 1 << 1,
                u = 1 << 2,
                d = 1 << 3,
                lu = 1 << 4,
                ru = 1 << 5, // connected in the up/right direction
                rd = 1 << 6, // connected in the right/down direction
                ld = 1 << 7  // etc.
            };
            u8 mask_ = 0;
        } connections_;
    };


    Node matrix_[8][3];


    WorldMap()
    {
        init();
    }


    void init()
    {
        for (auto& arr : matrix_) {
            for (auto& cell : arr) {
                cell = Node{};
            }
        }

        // clip corners
        matrix_[0][0].type_ = Node::null;
        matrix_[0][2].type_ = Node::null;
        matrix_[7][0].type_ = Node::null;
        matrix_[7][2].type_ = Node::null;


        for (int x = 0; x < 8; ++x) {
            matrix_[x][1].connections_.mask_ |= (Node::Connections::l | Node::Connections::r);
        }

        matrix_[0][1].connections_.mask_ &= ~(Node::Connections::l);
        matrix_[7][1].connections_.mask_ &= ~(Node::Connections::r);

        matrix_[0][1].connections_.mask_ |= Node::Connections::ru | Node::Connections::rd;
        matrix_[7][1].connections_.mask_ |= Node::Connections::lu | Node::Connections::ld;


        for (int x = 1; x < 7; ++x) {
            matrix_[x][0].connections_.mask_ |= (Node::Connections::l | Node::Connections::r);
            matrix_[x][2].connections_.mask_ |= (Node::Connections::l | Node::Connections::r);
        }

        matrix_[1][0].connections_.mask_ &= ~(Node::Connections::l);
        matrix_[6][0].connections_.mask_ &= ~(Node::Connections::r);
        matrix_[1][2].connections_.mask_ &= ~(Node::Connections::l);
        matrix_[6][2].connections_.mask_ &= ~(Node::Connections::r);


        matrix_[1][0].connections_.mask_ |= Node::Connections::ld;
        matrix_[6][0].connections_.mask_ |= Node::Connections::rd;
        matrix_[1][2].connections_.mask_ |= Node::Connections::lu;
        matrix_[6][2].connections_.mask_ |= Node::Connections::ru;


    }
};


}
