// NOTE: I know that I've done some bad things in this source file. CMake
// doesn't handle multiple build toolchains in a single project very well, so I
// just did a unity build so that I can just run g++ on a single file to build
// this utility. Actually, I have no good excuse, I was just feeling lazy today.
// At time of writing, I've worked on this game for a year, it's behind schedule
// and over budget, and I just want to finish.
//
//
// NOTE: The below compile command is really nasty. Explanation: The webserver
// host does not allow me to install anything with sudo, and there's no good png
// library for C++, so I originally built this with SFML. But sfml depends on a
// bunch of shared libraries, which I cannot install on the webserver host (at
// least, not to the directories where the sfml build system expects them to
// be). So I tried for about five minutes to get it to work, decided that doing
// things correctly wasn't worth the trouble, and just compiled the sfml sources
// into the macro_rast executable. The only interface that I need from SFML is
// the image class, which just wraps stb_image, so it's not really as bad as it
// looks. This repository includes both stb and the sfml source code.
//
// g++ standalone_island_raster.cpp -std=c++17 -I../ -I../../external -I../../external/SFML-2.5.1/src/ -I../../external/stb/ -o macro_rast ../../external/SFML-2.5.1/src/SFML/Graphics/Image.cpp ../../external/SFML-2.5.1/src/SFML/Graphics/Color.cpp ../../external/SFML-2.5.1/src/SFML/Graphics/ImageLoader.cpp ../../external/SFML-2.5.1/src/SFML/System/Err.cpp -D__CMD_MACRO_RAST__
//
// This file produces a commandline tool that accepts data extracted from a
// qr-encoded island and outputs an image. Intended for the skyland webserver.


#define HS_COMPRESSOR_OFF
#include "base32.cpp"
#include "macrocosmCubeSector.cpp"
#include "macrocosmEngine.cpp"
#include "macrocosmOutpostSector.cpp"
#include "macrocosmPancakeSector.cpp"
#include "macrocosmPillarSector.cpp"
#include "macrocosmSector.cpp"
#include "macrocosmSector.hpp"
#include "macrocosmSectorImpl.hpp"
#include "platform/platform.hpp"
#include "platform/scratch_buffer.cpp"
#include "qr.cpp"
#include "string.cpp"
#include <iostream>
extern "C" {
#include "qr/qrcodegen.c"
}
#include "memory/pool.cpp"
#include "rle.cpp"
#include "sharedVariable.cpp"
#include <SFML/Graphics.hpp>
#include <sstream>



auto bkg_color = sf::Color{90, 173, 239};



namespace flash_filesystem
{


u32 read_file_data(Platform& pfrm, const char* path, Vector<char>& output)
{
    return 0;
}


bool store_file_data(Platform& pfrm, const char* path, Vector<char>& data)
{
    return false;
}


} // namespace flash_filesystem


// Boostrap just the platform code that we need:
static Platform* platform;

Platform::Platform()
{
    ::platform = this;
}


Platform& Platform::instance()
{
    return *platform;
}


void Platform::fatal(const char* msg)
{
    std::cout << msg << std::endl;
    exit(EXIT_FAILURE);
}


void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        u16 val,
                        std::optional<u16> palette)
{
    // ...
}


static sf::Image* texture;
static sf::Image* img;


void Platform::blit_t0_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    if (hard) {
        blit_t0_erase(to_index);
    }

    auto x_start = 0;
    x_start += from_index * 8;

    int dest_x = (to_index % 30) * 8;
    int dest_y = (to_index / 30) * 8;

    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            const int src_x = x_start + x;
            const int src_y = y;

            auto px = texture->getPixel(src_x, src_y);
            if (px.a not_eq 0) {
                img->setPixel(dest_x + x, dest_y + y, px);
            }
        }
    }
}


void Platform::blit_t1_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
    if (hard) {
        blit_t1_erase(to_index);
    }

    auto x_start = 0;
    x_start += from_index * 8;

    int dest_x = (to_index % 30) * 8;
    int dest_y = (to_index / 30) * 8;

    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            const int src_x = x_start + x;
            const int src_y = y;

            auto px = texture->getPixel(src_x, src_y);
            if (px.a not_eq 0) {
                img->setPixel(dest_x + x, (16 * 8) + dest_y + y, px);
            }
        }
    }
}


void Platform::blit_t0_erase(u16 index)
{
    // int dest_x = (index % 30) * 8;
    // int dest_y = (index / 30) * 8;

    // for (int x = 0; x < 8; ++x) {
    //     for (int y = 0; y < 8; ++y) {
    //         img->setPixel(dest_x + x, dest_y + y, bkg_color);
    //     }
    // }
}


void Platform::blit_t1_erase(u16 index)
{
    // int dest_x = (index % 30) * 8;
    // int dest_y = (index / 30) * 8;

    // for (int x = 0; x < 8; ++x) {
    //     for (int y = 0; y < 8; ++y) {
    //         img->setPixel(dest_x + x, (16 * 8) + dest_y + y, bkg_color);
    //     }
    // }
}


Microseconds Platform::DeltaClock::sample() const
{
    return 1;
}


void Platform::Logger::clear()
{
}


bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    // std::cout << prompt << text << std::endl;
    return true;
}


void Platform::sleep(u32)
{
}


void* Platform::system_call(char const*, void*)
{
    return nullptr;
}


Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    fatal("jdklfjsd");
}


void Platform::overwrite_overlay_tile(u16 index, const EncodedTile& t)
{
}


Platform::SystemClock::SystemClock()
{
}


Platform::NetworkPeer::NetworkPeer()
{
}


Platform::DeltaClock::DeltaClock()
{
}


Platform::Screen::Screen()
{
}


Platform::Speaker::Speaker()
{
}


Platform::Logger::Logger()
{
}


Platform::DeltaClock::~DeltaClock()
{
}


Platform::NetworkPeer::~NetworkPeer()
{
}


Platform::~Platform()
{
}



std::optional<DateTime> Platform::SystemClock::now()
{
    return std::nullopt;
}



void Platform::Logger::log(Severity level, const char* msg)
{
}


void arabic__to_string(int num, char* buffer, int base)
{
    int i = 0;
    bool is_negative = false;

    if (num == 0) {
        buffer[i++] = '0';
        buffer[i] = '\0';
        return;
    }

    // Based on the behavior of itoa()
    if (num < 0 && base == 10) {
        is_negative = true;
        num = -num;
    }

    while (num != 0) {
        int rem = num % base;
        buffer[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
        num = num / base;
    }

    if (is_negative) {
        buffer[i++] = '-';
    }

    buffer[i] = '\0';

    str_reverse(buffer, i);

    return;
}


namespace skyland
{
SystemStringBuffer loadstr(Platform& pfrm, SystemString str)
{
    return allocate_dynamic<StringBuffer<1900>>("dummy");
}

bool App::is_developer_mode()
{
    return true;
}

void App::set_developer_mode(bool value)
{
    // ...
}

static lisp::Nil lnil;

lisp::Value*
App::invoke_script(Platform& pfrm, const char* path, bool rom_fs_only)
{
    // lol super hacky
    lnil.hdr_.type_ = lisp::Value::Type::nil;
    return (lisp::Value*)&lnil;
}
} // namespace skyland

const char* lisp::String::value()
{
    return "";
}



static int get_octet(int block)
{
    assert(block >= 0 && block < 8);
    return (block * 5) / 8;
}

static int get_offset(int block)
{
    assert(block >= 0 && block < 8);
    return (8 - 5 - (5 * block) % 8);
}

/**
 * Like "b >> offset" but it will do the right thing with negative offset.
 * We need this as bitwise shifting by a negative offset is undefined
 * behavior.
 */
static unsigned char shift_right(unsigned char byte, char offset)
{
    if (offset > 0)
        return byte >> offset;
    else
        return byte << -offset;
}

static unsigned char shift_left(unsigned char byte, char offset)
{
    return shift_right(byte, -offset);
}


static int decode_char(unsigned char c)
{
    char retval = -1;

    if (c >= 'A' && c <= 'Z')
        retval = c - 'A';
    if (c >= '2' && c <= '7')
        retval = c - '2' + 26;

    assert(retval == -1 || ((retval & 0x1F) == retval));

    return retval;
}



static int decode_sequence(const unsigned char* coded, unsigned char* plain)
{
    assert(CHAR_BIT == 8);
    assert(coded && plain);

    plain[0] = 0;
    for (int block = 0; block < 8; block++) {
        int offset = get_offset(block);
        int octet = get_octet(block);

        int c = decode_char(coded[block]);
        if (c < 0) // invalid char, stop here
            return octet;

        plain[octet] |= shift_left(c, offset);
        if (offset < 0) { // does this block overflows to next octet?
            assert(octet < 4);
            plain[octet + 1] = shift_left(c, 8 + offset);
        }
    }
    return 5;
}

size_t base32_decode(const unsigned char* coded, unsigned char* plain)
{
    size_t written = 0;
    for (size_t i = 0, j = 0;; i += 8, j += 5) {
        int n = decode_sequence(&coded[i], &plain[j]);
        written += n;
        if (n < 5)
            return written;
    }
}


int main(int argc, char** argv)
{
    Platform pfrm;

    sf::Image texture;
    sf::Image result;
    ::img = &result;
    ::texture = &texture;

    result.create(240, 240, bkg_color);

    std::stringstream output_json;
    output_json << "{";


    bool json_output_has_field = false;


    auto put_field = [&](const char* name, int value) {
        if (json_output_has_field) {
            output_json << ',';
        }
        json_output_has_field = true;
        output_json << '"';
        output_json << name;
        output_json << "\":";
        output_json << value;
    };


    if (argc not_eq 4) {
        puts("usage: macro-rast <name> <texture_path> "
             "<base32-island-data-from-qr-code>");
        return EXIT_FAILURE;
    }

    if (not texture.loadFromFile(argv[2])) {
        puts("failed to load macro texture!");
        return EXIT_FAILURE;
    }

    auto out_name = argv[1];
    auto arg = argv[3];
    u8 decode_buffer[4096];
    int decoded = base32_decode((const u8*)arg, decode_buffer);

    Buffer<char, 1000> decomp_input;
    for (int i = 0; i < decoded; ++i) {
        decomp_input.push_back(decode_buffer[i]);
    }

    using namespace skyland;

    Buffer<char, 1000> decomp;
    decompress(decomp_input, decomp);

    auto shape = (macro::terrain::Sector::Shape)decomp[0];
    switch (shape) {
    case macro::terrain::Sector::Shape::cube:
        if (decomp.size() not_eq 577) {
            puts("decoded bad input size");
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::pancake:
        if (decomp.size() not_eq 577) {
            puts("decoded bad input size");
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::pillar:
        if (decomp.size() not_eq 577) {
            puts("decoded bad input size");
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::freebuild_wide:
        if (decomp.size() not_eq 865) {
            puts("decoded bad input size");
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::freebuild_flat:
        if (decomp.size() not_eq 981) {
            puts("decoded bad input size");
            std::cout << decomp.size() << std::endl;
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::freebuild:
        if (decomp.size() not_eq 901) {
            puts("decoded bad input size for freebuild sector");
            std::cout << decomp.size() << std::endl;
            return EXIT_FAILURE;
        }
        break;

    case macro::terrain::Sector::Shape::outpost:
        break;

    default:
        puts("invalid sector type!");
        return EXIT_FAILURE;
    }

    macro::EngineImpl engine(pfrm, nullptr);

    int block_count = 0;

    if (auto s = engine.make_sector({0, 1}, shape)) {
        auto sz = s->size();

        for (u8 z = 0; z < sz.z; ++z) {
            for (u8 x = 0; x < sz.x; ++x) {
                for (u8 y = 0; y < sz.y; ++y) {
                    auto index = 1 + y + sz.y * (x + sz.x * z);
                    auto blk = (macro::terrain::Type)decomp[index];
                    if (blk not_eq macro::terrain::Type::selector) {
                        s->set_block({x, y, z}, blk);
                        if (blk not_eq macro::terrain::Type::air) {
                            ++block_count;
                        }
                    } else {
                        s->set_block({x, y, z}, macro::terrain::Type::air);
                    }
                }
            }
        }

        std::string name(out_name);

        s->render(pfrm);
        result.saveToFile(name + "1.png");
        s->rotate();

        result.create(240, 240, bkg_color);

        s->render(pfrm);
        result.saveToFile(name + "2.png");
        s->rotate();

        result.create(240, 240, bkg_color);

        s->render(pfrm);
        result.saveToFile(name + "3.png");
        s->rotate();

        result.create(240, 240, bkg_color);

        s->render(pfrm);
        result.saveToFile(name + "4.png");

        put_field("w", s->size().x);
        put_field("h", s->size().z - 1);
        put_field("freebuild",
                  shape not_eq macro::terrain::Sector::Shape::cube and
                      shape not_eq macro::terrain::Sector::Shape::pancake and
                      shape not_eq macro::terrain::Sector::Shape::pillar and
                      shape not_eq macro::terrain::Sector::Shape::outpost);

        auto stats = s->stats();
        put_field("food", stats.food_);
        put_field("housing", stats.housing_);
        put_field("employment", stats.employment_);
    }

    put_field("block_count", block_count);

    output_json << "}";

    std::cout << output_json.str() << std::endl;

    return EXIT_SUCCESS;
}
