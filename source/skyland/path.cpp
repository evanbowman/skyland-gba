////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "path.hpp"
#include "island.hpp"
#include "skyland/scene/constructionScene.hpp"



namespace skyland
{



struct PathVertexData
{
    PathVertexData* prev_ = nullptr;
    u16 dist_ = std::numeric_limits<u16>::max();
    RoomCoord coord_;
};



Optional<Path> find_path(Island* island,
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
        // NOTE: start at the minimum y for which rooms can be built. Do not
        // include the 16th (final) row, because it just contains terrain.
        for (u8 y = construction_zone_min_y; y < 15; ++y) {
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
