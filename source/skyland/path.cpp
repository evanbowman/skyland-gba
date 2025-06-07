////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "path.hpp"
#include "island.hpp"
#include "skyland/rooms/portal.hpp"
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
                         Character* for_character,
                         const RoomCoord& start,
                         const RoomCoord& end)
{
    BulkAllocator<2> vertex_memory_;

    using VertexBuffer = Buffer<PathVertexData*, 256>;
    VertexBuffer priority_q(VertexBuffer::SkipZeroFill{});
    PathVertexData* vertex_mat[16][16] = {};



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
            if (auto room = island->get_room(min->coord_)) {
                if (room->cast<Portal>()) {
                    for (auto& o : island->rooms()) {
                        if (o.get() not_eq room and o->cast<Portal>()) {
                            auto alt =
                                min->dist_ +
                                manhattan_length(min->coord_, o->position());
                            if (auto n = vertex_mat[o->position().x]
                                                   [o->position().y]) {
                                if (alt < n->dist_) {
                                    n->dist_ = alt;
                                    n->prev_ = min;
                                }
                            }
                        }
                    }
                }
            }
            sort_q();

        } else {
            return {};
        }
    }
}



} // namespace skyland
