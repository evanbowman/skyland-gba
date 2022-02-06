#pragma once

#include "bitvector.hpp"
#include "bulkAllocator.hpp"
#include "img.hpp"



namespace skyland {



class CustomTileMapper {
public:
    static const int mapping_count = 15;


    // returns -1 upon failure.
    int map_image(Platform& pfrm, const img::Image& image);


    // Push all mapped images to video memory
    void publish_as_tiles(Platform& pfrm);

    void publish_as_sprites(Platform& pfrm);


    void clear()
    {
        mappings_.reset();
    }


private:
    struct Mappings {
        img::Image textures_[mapping_count];
        Bitvector<mapping_count> textures_in_use_;
    };

    std::optional<DynamicMemory<Mappings>> mappings_;
};



} // namespace skyland
