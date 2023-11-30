////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "path.hpp"
#include "island.hpp"



namespace skyland
{



struct PathVertexData
{
    PathVertexData* prev_ = nullptr;
    u16 dist_ = std::numeric_limits<u16>::max();
    RoomCoord coord_;
};



std::optional<Path> find_path(Island* island,
                              BasicCharacter* for_character,
                              const RoomCoord& start,
                              const RoomCoord& end)
{
    BulkAllocator<2> vertex_memory_;

    Buffer<PathVertexData*, 256> priority_q;
    PathVertexData* vertex_mat[16][16] = {0};



    bool matrix[16][16];
    island->plot_walkable_zones(matrix, for_character);

    PathVertexData* start_v = nullptr;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y]) {
                if (auto obj = vertex_memory_.alloc<PathVertexData>()) {
                    obj->coord_ = {x, y};
                    if (priority_q.push_back(obj.release())) {
                        if (priority_q.back()->coord_ == start) {
                            start_v = priority_q.back();
                            start_v->dist_ = 0;
                        }
                        vertex_mat[x][y] = priority_q.back();
                    } else {
                        error("failed to push vertex");
                    }
                }
            }
        }
    }

    if (not start_v) {
        error("missing startv");
        return {};
    }

    auto neighbors = [&](PathVertexData* data) {
        Buffer<PathVertexData*, 4> result;
        if (data->coord_.x > 0) {
            auto n = vertex_mat[data->coord_.x - 1][data->coord_.y];
            if (n) {
                result.push_unsafe(n);
            }
        }
        if (data->coord_.x < 15) {
            auto n = vertex_mat[data->coord_.x + 1][data->coord_.y];
            if (n) {
                result.push_unsafe(n);
            }
        }
        if (data->coord_.y > 0) {
            auto n = vertex_mat[data->coord_.x][data->coord_.y - 1];
            if (n) {
                result.push_unsafe(n);
            }
        }
        if (data->coord_.y < 15) {
            auto n = vertex_mat[data->coord_.x][data->coord_.y + 1];
            if (n) {
                result.push_unsafe(n);
            }
        }
        return result;
    };

    auto sort_q = [&] {
        std::sort(priority_q.begin(),
                  priority_q.end(),
                  [](auto& lhs, auto& rhs) { return lhs->dist_ > rhs->dist_; });
    };

    sort_q();

    while (true) {
        if (not priority_q.empty()) {
            auto min = priority_q.back();
            if (min->dist_ == std::numeric_limits<u16>::max()) {
                return {};
            }
            if (min->coord_ == end) {
                auto path_mem = allocate_dynamic<PathBuffer>("path-buffer");
                if (not path_mem) {
                    return {};
                }

                auto current_v = priority_q.back();
                while (current_v) {
                    path_mem->push_back(current_v->coord_);
                    current_v = current_v->prev_;
                }
                return path_mem;
            }
            priority_q.pop_back();

            for (auto& neighbor : neighbors(min)) {
                auto alt = min->dist_ +
                           manhattan_length(min->coord_, neighbor->coord_);
                if (alt < neighbor->dist_) {
                    neighbor->dist_ = alt;
                    neighbor->prev_ = min;
                }
            }
            sort_q();

        } else {
            return {};
        }
    }
}



} // namespace skyland
