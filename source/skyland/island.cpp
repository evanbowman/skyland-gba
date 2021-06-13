#include "island.hpp"
#include "rooms/core.hpp"
#include "rooms/stairwell.hpp"
#include "rooms/exteriorWall.hpp"
#include "roomPool.hpp"
#include "tile.hpp"




namespace skyland {



Island::Island(Platform& pfrm, Layer layer, u8 width) :
    layer_(layer),
    timer_(0),
    interior_visible_(false)
{
    terrain_.push_back(13), --width;

    for (int i = 0; i < width - 1; ++i) {
        terrain_.push_back(12);
    }

    terrain_.push_back(14);

    render_terrain(pfrm);

    add_room<Core>(pfrm, {1, 13});
    // add_room<Stairwell>(pfrm, {0, 11});
    // add_room<ExteriorWall>(pfrm, {3, 13});
    // add_room<Core>(pfrm, {4, 13});

}



Island::Rooms& Island::rooms()
{
    return rooms_;
}



void Island::update(Platform& pfrm, App& app, Microseconds dt)
{
    timer_ += dt;

    ambient_movement_ = 4 *
        float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
        std::numeric_limits<s16>::max();


    for (auto& room : rooms_) {
        room->update(pfrm, app, dt);
    }
}



void Island::render_terrain(Platform& pfrm)
{
    for (u32 i = 0; i < terrain_.size(); ++i) {
        pfrm.set_tile(layer_, i, 15, terrain_[i]);
    }
}



const Vec2<Float>& Island::get_position() const
{
    return position_;
}



void Island::set_position(const Vec2<Float>& position)
{
    position_ = position;
}



void Island::render_interior(Platform& pfrm)
{
    for (auto& room : rooms()) {
        room->render_interior(pfrm, layer_);
    }

    interior_visible_ = true;
}



void Island::render_exterior(Platform& pfrm)
{
    for (auto& room : rooms()) {
        room->render_exterior(pfrm, layer_);
    }

    interior_visible_ = false;
}



void Island::plot_rooms(bool matrix[16][16]) const
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x][y] = 0;
        }
    }

    for (auto& room : rooms_) {
        auto pos = room->position();
        auto sz = room->size();

        for (int x = 0; x < sz.x; ++x) {
            for (int y = 0; y < sz.y; ++y) {
                matrix[x + pos.x][y + pos.y] = true;
            }
        }
    }
}



void Island::plot_construction_zones(bool matrix[16][16]) const
{
    bool matrix_temp[16][16];

    plot_rooms(matrix_temp);

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            matrix[x][y] = matrix_temp[x][y] == 0 and y < 15 and matrix_temp[x][y + 1];
        }
    }

    for (int x = 0; x < 16; ++x) {
        matrix[x][15] = false;
    }

    for (u32 x = 0; x < terrain_.size(); ++x) {
        if (matrix_temp[x][14] == 0) {
            matrix[x][14] = true;
        }
    }
}



void Island::repaint(Platform& pfrm)
{
    bool matrix[16][16];

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            pfrm.set_tile(layer_, x, y, 0);
        }
    }

    if (interior_visible_) {
        render_interior(pfrm);
    } else {
        render_exterior(pfrm);
    }

    plot_rooms(matrix);

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            if (matrix[x][y] == 0 and y < 31 and matrix[x][y + 1]) {
                pfrm.set_tile(layer_, x, y, Tile::roof_plain);
            } else if (y == 14 and matrix[x][y] == 0 and x < (int)terrain_.size()) {
                pfrm.set_tile(layer_, x, y, Tile::grass);
            }
        }
    }
}


Vec2<Float> Island::origin() const
{
    return {position_.x, position_.y + ambient_movement_};
}


}
