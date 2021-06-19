#pragma once


#include "memory/buffer.hpp"
#include "number/numeric.hpp"
#include "number/random.hpp"



namespace skyland {



struct WorldMap {
    struct Node {
        enum Type {
            null,
            clear,
            storm_clear,
            storm_hostile,
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

        bool visited_ : 1;

        Node() : visited_(false)
        {
        }
    };

    Node matrix_[8][3];


    WorldMap()
    {
        generate();
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
            matrix_[x][1].connections_.mask_ |=
                (Node::Connections::l | Node::Connections::r);
        }

        matrix_[0][1].connections_.mask_ &= ~(Node::Connections::l);
        matrix_[7][1].connections_.mask_ &= ~(Node::Connections::r);

        matrix_[0][1].connections_.mask_ |=
            Node::Connections::ru | Node::Connections::rd;
        matrix_[7][1].connections_.mask_ |=
            Node::Connections::lu | Node::Connections::ld;


        for (int x = 1; x < 7; ++x) {
            matrix_[x][0].connections_.mask_ |=
                (Node::Connections::l | Node::Connections::r);
            matrix_[x][2].connections_.mask_ |=
                (Node::Connections::l | Node::Connections::r);
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


    void generate()
    {
    RETRY:
        init();

        int accumulated_weight = 0;

        auto gen = [&](auto& node) {
            auto val = rng::choice(10000, rng::critical_state);
            val -= 2500 * accumulated_weight;
            if (val < 5000) {
                node.type_ = WorldMap::Node::Type::clear;
                --accumulated_weight;
            } else {
                node.type_ = WorldMap::Node::Type::hostile;
                ++accumulated_weight;
            }
        };

        auto conceal = [&](int row) {
            if (accumulated_weight not_eq 2) {
                accumulated_weight = abs(accumulated_weight);

                if (accumulated_weight < 2) {
                    accumulated_weight = 2;
                }
                while (accumulated_weight--) {
                    int x = rng::choice<6>(rng::critical_state);
                    auto& node = matrix_[x + 1][row];
                    if (node.type_ == Node::Type::hostile) {
                        node.type_ = Node::Type::storm_hostile;
                    } else if (node.type_ == Node::Type::clear) {
                        node.type_ = Node::Type::storm_clear;
                    }
                }
            }
        };

        for (int i = 1; i < 7; ++i) {
            auto& node = matrix_[i][1];
            gen(node);
        }

        conceal(1);

        for (int i = 1; i < 6; ++i) {
            auto& node = matrix_[i][0];
            gen(node);
        }

        conceal(0);

        for (int i = 1; i < 6; ++i) {
            auto& node = matrix_[i][2];
            gen(node);
        }

        conceal(2);


        int weights[3] = {0, 0, 0};
        bool relief[3] = {false, false, false};

        auto convert_weight = [](Node::Type t) {
            switch (t) {
            case Node::Type::clear:
                return -1;
            case Node::Type::hostile:
                return 1;
            case Node::Type::storm_clear:
                return -1;
            case Node::Type::storm_hostile:
                return 1;
            default:
                return 0;
            }
        };

        for (int i = 0; i < 6; ++i) {
            for (int j = 0; j < 3; ++j) {
                weights[j] += convert_weight(matrix_[i][j].type_);
            }

            bool r0 = false;

            if (abs(weights[0] - weights[1]) > 2 and not relief[0] and
                matrix_[i][0].type_ not_eq matrix_[i + 1][1].type_) {

                matrix_[i][0].connections_.mask_ |= Node::Connections::rd;
                matrix_[i + 1][1].connections_.mask_ |= Node::Connections::lu;
                relief[0] = true;
                r0 = true;
            }

            bool r1 = false;
            if (abs(weights[0] - weights[1]) > 2 and
                rng::choice<2>(rng::critical_state) and not relief[1] and
                not r0 and matrix_[i][1].type_ not_eq matrix_[i + 1][0].type_) {

                matrix_[i][1].connections_.mask_ |= Node::Connections::ru;
                matrix_[i + 1][0].connections_.mask_ |= Node::Connections::ld;
                relief[1] = true;
                r1 = true;
            }

            if (abs(weights[1] - weights[2]) > 2 and not relief[1] and
                matrix_[i][1].type_ not_eq matrix_[i + 1][2].type_) {

                matrix_[i][1].connections_.mask_ |= Node::Connections::rd;
                matrix_[i + 1][2].connections_.mask_ |= Node::Connections::lu;
                relief[1] = true;
                r1 = true;
            }

            if (abs(weights[1] - weights[2]) > 2 and not relief[2] and
                not r1 and matrix_[i][2].type_ not_eq matrix_[i + 1][1].type_) {

                matrix_[i][2].connections_.mask_ |= Node::Connections::ru;
                matrix_[i + 1][1].connections_.mask_ |= Node::Connections::ld;
                relief[2] = true;
            }
        }

        if (not relief[0] and not relief[1] and not relief[2]) {
            // We generated a boring map.
            goto RETRY;
        }
    }
};


} // namespace skyland
