#pragma once


#include "skyland/macrocosmSector.hpp"



namespace skyland::macro
{



struct CheckerBoard
{
    enum Checker : u8
    {
        none,
        red,
        black,
        red_king,
        black_king,
    };

    Checker data_[10][10];
    terrain::Sector::Orientation orientation_;


    int score() const
    {
        int score = 0;
        for (u8 x = 0; x < 9; ++x) {
            for (u8 y = 0; y < 9; ++y) {
                if (data_[x][y] == red) {
                    score += 2;
                } else if (data_[x][y] == black) {
                    score -= 2;
                } else if (data_[x][y] == red_king) {
                    score += 3;
                } else if (data_[x][y] == black_king) {
                    score -= 3;
                }
            }
        }

        return score;
    }


    static CheckerBoard from_sector(const terrain::Sector& s)
    {
        CheckerBoard result;
        result.orientation_ = s.orientation();

        for (u8 x = 0; x < 10; ++x) {
            for (u8 y = 0; y < 10; ++y) {
                auto t = s.get_block({x, y, 1}).type();
                switch (t) {
                case terrain::Type::checker_red_king:
                    result.data_[x][y] = CheckerBoard::Checker::red_king;
                    break;

                case terrain::Type::checker_black_king:
                    result.data_[x][y] = CheckerBoard::Checker::black_king;
                    break;

                case terrain::Type::checker_red:
                    result.data_[x][y] = CheckerBoard::Checker::red;
                    break;

                case terrain::Type::checker_black:
                    result.data_[x][y] = CheckerBoard::Checker::black;
                    break;

                default:
                    result.data_[x][y] = CheckerBoard::Checker::none;
                    break;
                }
            }
        }

        return result;
    }
};




}
