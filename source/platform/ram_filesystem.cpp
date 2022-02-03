#include "ram_filesystem.hpp"



namespace ram_filesystem {



int fs_begin_offset;



static bool mounted = false;



bool is_mounted()
{
    return mounted;
}



std::optional<ScratchBufferPtr> path_cache_;



int fs_offset()
{
    return fs_begin_offset;
}



int fs_contents_offset()
{
    // Block zero reserved for file root and fileinfo stuff.
    return fs_offset() + block_size;
}



Root load_root(Platform& pfrm)
{
    Root root;
    pfrm.read_save_data(&root, sizeof root, fs_offset());

    return root;
}



Statistics statistics(Platform& pfrm)
{
    const auto block_count = (pfrm.save_capacity() - fs_offset()) / block_size;

    Statistics stats;
    stats.blocks_available_ = 0;

    auto root = load_root(pfrm);
    auto freelist = root.freelist_.get();

    while (freelist) {
        FileContents::Header header;
        pfrm.read_save_data(&header,
                            sizeof header,
                            fs_contents_offset() + freelist * block_size);

        ++stats.blocks_available_;

        freelist = header.next_.get();
    }

    stats.blocks_used_ = block_count - stats.blocks_available_;

    return stats;
}



bool store_root(Platform& pfrm, const Root& root)
{
    return pfrm.write_save_data(&root, sizeof root, fs_offset());
}



void destroy(Platform& pfrm)
{
    char buffer[block_size * 2];
    __builtin_memset(buffer, 0, sizeof buffer);

    pfrm.write_save_data(buffer, sizeof buffer, fs_offset());
}



InitStatus initialize(Platform& pfrm, int fs_begin_offset)
{
    ram_filesystem::fs_begin_offset = fs_begin_offset;

    auto root = load_root(pfrm);

    pfrm.read_save_data(&root, sizeof root, fs_offset());

    static const char fs_version = '1';

    if (root.magic_[0] == '_' and root.magic_[1] == 'F' and
        root.magic_[2] == 'S' and root.magic_[3] == fs_version) {

        ::ram_filesystem::mounted = true;

        // Already initialized previously.
        return InitStatus::already_initialized;
    }

    root.magic_[0] = '_';
    root.magic_[1] = 'F';
    root.magic_[2] = 'S';
    root.magic_[3] = fs_version;

    root.file_count_.set(0);
    root.freelist_.set(0);

    // Construct the freelist. Only needs to be done once, the first time that
    // the game boots, as the filesystem persists in SRAM.
    const auto block_count = (pfrm.save_capacity() - fs_offset()) / block_size;
    auto offset = fs_contents_offset();
    for (int i = 1; i < block_count; ++i) {
        FileContents::Header header;
        pfrm.read_save_data(&header, sizeof header, offset);
        header.next_.set(root.freelist_.get());
        pfrm.write_save_data(&header, sizeof header, offset);

        root.freelist_.set(i - 1);

        offset += block_size;
    }

    if (store_root(pfrm, root)) {
        ::ram_filesystem::mounted = true;
        return InitStatus::initialized;
    }

    return InitStatus::failed;
}



static u16 allocate_file_chunk(Platform& pfrm)
{
    auto root = load_root(pfrm);

    if (root.freelist_.get() == 0) {
        return 0;
    } else {
        auto allocated = root.freelist_.get();

        FileContents::Header header;
        pfrm.read_save_data(&header,
                            sizeof header,
                            fs_contents_offset() + allocated * block_size);

        root.freelist_.set(header.next_.get());

        store_root(pfrm, root);

        return allocated;
    }
}



void link_file(Platform& pfrm, u16 file, u16 length)
{
    auto root = load_root(pfrm);

    auto offset = fs_offset() + sizeof(Root);

    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileInfo info;
        pfrm.read_save_data(&info, sizeof info, offset);

        if (info.file_contents_.get() == 0 and info.file_size_.get() == 0) {
            info.file_size_.set(length);
            info.file_contents_.set(file);
            pfrm.write_save_data(&info, sizeof info, offset);
            return;
        }

        offset += sizeof info;
    }

    FileInfo info;
    info.file_size_.set(length);
    info.file_contents_.set(file);
    pfrm.write_save_data(&info, sizeof info, offset);

    root.file_count_.set(root.file_count_.get() + 1);

    store_root(pfrm, root);
}



void free_file(Platform& pfrm, u16 file)
{
    auto root = load_root(pfrm);

    while (file) {
        FileContents::Header header;
        pfrm.read_save_data(
            &header, sizeof header, fs_contents_offset() + file * block_size);

        auto freelist = root.freelist_.get();
        root.freelist_.set(file);
        auto next_file = header.next_.get();
        header.next_.set(freelist);

        pfrm.write_save_data(
            &header, sizeof header, fs_contents_offset() + file * block_size);

        file = next_file;
    }

    store_root(pfrm, root);
}



template <typename F>
void with_file(Platform& pfrm, const char* path, F&& callback)
{
    auto root = load_root(pfrm);

    auto offset = fs_offset() + sizeof(Root);

    for (int i = 0; i < root.file_count_.get(); ++i) {
        FileInfo info;
        pfrm.read_save_data(&info, sizeof info, offset);

        if (auto file = info.file_contents_.get()) {
            char path_buffer[max_path + 1];
            path_buffer[max_path] = '\0';

            pfrm.read_save_data(path_buffer,
                                max_path,
                                fs_contents_offset() + file * block_size +
                                    sizeof(FileContents::Header));

            if (str_cmp(path_buffer, path) == 0) {
                callback(info, file, offset);
                return;
            }
        }

        offset += sizeof info;
    }
}



bool file_exists(Platform& pfrm, const char* path)
{
    if (not ::ram_filesystem::mounted) {
        return false;
    }

    bool found = false;

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        found = true;
    });

    return found;
}



void unlink_file(Platform& pfrm, const char* path)
{
    if (not ::ram_filesystem::mounted) {
        return;
    }

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        // Unbind the existing file
        info.file_size_.set(0);
        info.file_contents_.set(0);
        pfrm.write_save_data(&info, sizeof info, fs_offset);

        free_file(pfrm, file);
    });
}



static u8 checksum(const FileContents& contents)
{
    u8 result = 0;

    for (u32 i = 0; i < FileContents::capacity; ++i) {
        result += contents.data_[i];
    }

    return result;
}


size_t read_file_data(Platform& pfrm, const char* path, Vector<char>& output)
{
    if (not ::ram_filesystem::mounted) {
        return 0;
    }

    const auto path_len = str_len(path);

    with_file(pfrm, path, [&](FileInfo& info, u16 file, u16 fs_offset) {
        FileContents contents;

        pfrm.read_save_data(&contents,
                            sizeof contents,
                            fs_contents_offset() + file * block_size);

        auto on_corruption = [&] {
            output.clear();
            const char* msg = "DISK_CORRUPTION";
            const auto msg_len = str_len(msg);
            for (u32 i = 0; i < msg_len; ++i) {
                output.push_back(msg[i]);
            }
        };

        if (checksum(contents) not_eq contents.header_.checksum_) {
            on_corruption();
            return;
        }

        const auto len = info.file_size_.get() - (path_len + 1);

        for (u16 i = path_len + 1;
             i < FileContents::capacity and output.size() not_eq len;
             ++i) {

            output.push_back(contents.data_[i]);
        }

        file = contents.header_.next_.get();

        while (file) {
            pfrm.read_save_data(&contents,
                                sizeof contents,
                                fs_contents_offset() + file * block_size);

            if (checksum(contents) not_eq contents.header_.checksum_) {
                on_corruption();
                return;
            }

            for (u16 i = 0;
                 i < FileContents::capacity and output.size() not_eq len;
                 ++i) {

                output.push_back(contents.data_[i]);
            }

            file = contents.header_.next_.get();
        }
    });

    auto result = output.size();

    output.push_back('\0');

    return result;
}



bool store_file_data(Platform& pfrm, const char* path, Vector<char>& data)
{
    if (not ::ram_filesystem::mounted) {
        return false;
    }

    unlink_file(pfrm, path);

    const u16 path_len = str_len(path);

    if (path_len > max_path) {
        return false;
    }

    const int length = data.size() - 1; // -1 for the assumed null terminator.
    u16 remaining = length + path_len + 1;

    const auto file_begin = allocate_file_chunk(pfrm);
    auto file = file_begin;
    if (file == 0) {
        return false;
    }

    FileContents contents;
    __builtin_memset(&contents, 0, sizeof contents);

    memcpy(contents.data_, path, path_len + 1);
    remaining -= path_len + 1;

    const auto initial_data_copy =
        std::min((int)FileContents::capacity - (path_len + 1), (int)length);


    for (int i = 0; i < initial_data_copy; ++i) {
        contents.data_[i + path_len + 1] = data[i];
    }

    int offset = initial_data_copy;

    remaining -= initial_data_copy;

    auto store_chunk = [&] {
        contents.header_.checksum_ = checksum(contents);
        pfrm.write_save_data((u8*)&contents,
                             sizeof contents,
                             fs_contents_offset() + file * block_size);
    };

    if (remaining == 0) {
        store_chunk();
        link_file(pfrm, file_begin, length + path_len + 1);
        return true;
    }

    while (remaining) {
        auto next_file = allocate_file_chunk(pfrm);
        if (next_file == 0) {
            free_file(pfrm, file_begin);
            return false;
        }
        contents.header_.next_.set(next_file);

        store_chunk();

        __builtin_memset(&contents, 0, sizeof contents);

        file = next_file;

        const auto copy_len =
            std::min((int)FileContents::capacity, (int)remaining);

        for (int i = 0; i < copy_len; ++i) {
            contents.data_[i] = data[offset + i];
        }

        offset += copy_len;
        remaining -= copy_len;
    }

    store_chunk();


    link_file(pfrm, file_begin, length + path_len + 1);


    return true;
}



} // namespace ram_filesystem
