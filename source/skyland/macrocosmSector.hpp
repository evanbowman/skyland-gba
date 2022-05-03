#pragma once

#include "allocator.hpp"
#include "macrocosmBlock.hpp"



namespace skyland::macro
{



namespace fiscal
{



struct LineItem
{
    using Label = StringBuffer<24>;

    Label label_;
    Float contribution_;
    LineItem* next_;
};



class Ledger
{
public:
    void add_entry(LineItem::Label, Float contribution);


    const LineItem* entries() const;


private:
    LineItem* entries_ = nullptr;
    ScratchBufferBulkAllocator alloc_;
};



} // namespace fiscal



} // namespace skyland::macro



namespace skyland::macro::terrain
{



class Sector
{
public:
    enum Orientation : u8 { north, east, south, west };

    enum class Shape : u8 {
        cube, pancake
    };


    struct ExportInfo
    {
        Commodity::Type c;
        Vec3<u8> source_coord_;
        Vec2<s8> destination_;
        host_u16 export_supply_;
    };


    Sector(Vec2<s8> position, Shape shape = Shape::cube);


    void set_block(const Vec3<u8>& coord, Type type);

    void rotate();
    void update();
    void advance(int years);

    void render_setup(Platform& pfrm);
    void render(Platform& pfrm);

    using Population = float;
    void set_population(Population p);


    void shadowcast();
    void erase();


    void clear_cache();



    using Exports = Buffer<ExportInfo, 24>;


    const Exports& exports() const;
    void set_export(const ExportInfo& e);
    void remove_export(Vec3<u8> source_coord);


    u16 quantity_non_exported(Commodity::Type t);


    const Block& get_block(const Vec3<u8>& coord) const;


    Stats stats() const;


    static const int z_limit = 9;



    Population population() const;

    Float population_growth_rate() const;
    Coins coin_yield() const;


    fiscal::Ledger budget() const;


    Vec2<s8> coordinate() const;


    Vec3<u8> cursor() const
    {
        return p_.cursor_;
    }

    void set_cursor(const Vec3<u8>& pos, bool lock_to_floor = true);

    // Projected position of the cursor onto the frame buffer.
    u16 cursor_raster_pos() const;


    Orientation orientation() const
    {
        return p_.orientation_;
    }


    bool set_z_view(u8 z_view);
    u8 get_z_view() const
    {
        return z_view_;
    }


    static const int name_len = 12;


    // Should include almost all data that needs to be written to save memory,
    // except for the blocks themselves.
    struct Persistent
    {

        Orientation orientation_ = Orientation::north;

        Vec3<u8> cursor_;

        char name_[name_len];
        u8 population_packed_[sizeof(Population)];

        s8 x_;
        s8 y_;

        Shape shape_;

        u8 pad_[1]; // FIXME: remove. Added while packing this struct.
    };
    static_assert(std::is_trivially_copyable<Persistent>());
    static_assert(alignof(Persistent) == 1);


    void set_name(const StringBuffer<name_len - 1>& name);
    StringBuffer<name_len - 1> name();



    Stats base_stats() const;


    void repaint();


    Vec3<u8> size()
    {
        return {8, 8, 9};
    }


private:
    Persistent p_;

    // Recalculating stats for everything when we have multiple levels slows
    // down the game significantly, so we cache previous results. I mean, a
    // sector has ~512 blocks, and if you have 20 sectors, that's a lot of
    // number crunching and will definitely lag the game if done frequently.
    mutable std::optional<Stats> base_stats_cache_;

    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)

    Shape shape_;


    Exports exports_;

public:
    // Restore from a previous save.
    void restore(const Persistent& p, u8 blocks[z_limit][8][8]);


    const Persistent& persistent() const
    {
        return p_;
    }
};



}
