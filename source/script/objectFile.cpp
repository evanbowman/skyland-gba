////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "objectFile.hpp"
#include "bytecode.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"
#include "platform/flash_filesystem.hpp"


namespace lisp
{


using DefinitionCountField = host_u16;


ObjectFile::ObjectFile(const Fingerprint& f, bool relocatable) :
    relocatable_(relocatable)
{
    Header header;
    header.fingerprint_ = f;
    header.flags_.set(0);
    if (relocatable) {
        header.set_flag(Header::Flag::relocatable);
    }
    append((void*)&header, sizeof header);

    if (relocatable) {
        // Placeholder — patched in save()
        RelocationTableInfo info;
        info.offset_.set(0);
        append((void*)&info, sizeof info);
    }
}


void ObjectFile::append(void* data, u32 len)
{
    for (u32 i = 0; i < len; ++i) {
        bytes_.push_back(((u8*)data)[i]);
    }
}


Symbol::SymtabIndex ObjectFile::store_symbol(Symbol::SymtabIndex input)
{
    if (not relocatable_) {
        return input;
    }

    auto name = load_from_symtab(input * symtab_stride);

    Symbol::SymtabIndex i = 0;
    for (auto& str : relocation_table_) {
        if (str == name) {
            return i;
        }
        ++i;
    }
    relocation_table_.push_back(name);
    return i;
}


Symbol::SymtabIndex ObjectFile::link_symbol(Symbol::SymtabIndex input,
                                            RelocationTable& relocation_table,
                                            bool relocatable)
{
    if (not relocatable) {
        return input;
    }

    if (input >= relocation_table.size()) {
        PLATFORM.fatal(::format("failed to link symbol index % "
                                "in relocatable object file",
                                input));
    }

    auto name = relocation_table[input].c_str();
    if (auto idx = get_symtab_index(name)) {
        return *idx;
    } else {
        PLATFORM.fatal(::format("symbol % referenced in object file missing "
                                "from symtab!",
                                name));
        while (1);
    }
}


void ObjectFile::append(Symbol::SymtabIndex sym, Function& fn)
{
    if (fn.hdr_.mode_bits_ not_eq Function::ModeBits::lisp_bytecode_function) {
        PLATFORM.fatal(::format<86>("cannot save non-compiled function % in "
                                    "object file library!",
                                    load_from_symtab(sym * symtab_stride)));
        return;
    }

    auto data = fn.bytecode_impl_.databuffer()->databuffer().value();
    auto start_offset = fn.bytecode_impl_.bytecode_offset()->integer().value_;

    instruction::InstructionList instr;
    parse_instructions(*data, instr, start_offset);

    auto visit_symbol_opcodes = [&](auto cb) {
        for (auto inst : instr) {
            using namespace instruction;
            if (is_symtab_opcode(inst)) {
                auto st = ((LoadSymtab*)inst);
                cb(st);
            }
        }
    };

    visit_symbol_opcodes([this](auto st) {
        // NOTE: overwrite opcodes in instructions with symbol indices referring
        // to the relocation table entries.
        st->symtab_index_.set(store_symbol(st->symtab_index_.get()));
    });

    auto begin = data->data_ + start_offset;
    auto end = instr.back();

    using namespace instruction;
    if (end->op_ not_eq Ret::op() and end->op_ not_eq RetNil::op()) {
        LOGIC_ERROR();
    }

    auto bytecode_size = ((char*)end - begin) + sizeof(Ret);

    Definition def;
    def.sym_.set(store_symbol(sym));
    def.sig_ = fn.sig_;

    append(&def, sizeof(def));
    append(begin, bytecode_size);

    visit_symbol_opcodes([this](auto st) {
        // NOTE: we previously transformed the bytecode in-place, so we need to
        // update the symbol table indices to point to the applications symbol
        // table by re-linking everything. This also gives us confirmation that
        // link_symbol will succeed in the future when we perform a cold boot
        // and load bytecode from an object file library.
        st->symtab_index_.set(link_symbol(st->symtab_index_.get(),
                                          relocation_table_,
                                          relocatable_));
    });

    ++definition_count_;
}


Vector<char>& ObjectFile::export_data()
{
    if (exported_) {
        return bytes_;
    }

    if (bytes_.size() < sizeof(Header)) {
        LOGIC_ERROR();
    }

    auto header = (Header*)&*bytes_.begin();
    header->definition_count_.set(definition_count_);

    if (relocatable_) {
        // Patch the relocation table offset to point at current end
        auto info = (RelocationTableInfo*)(&*bytes_.begin() + sizeof(Header));
        info->offset_.set(bytes_.size());

        // Append relocation strings inline
        for (auto& str : relocation_table_) {
            for (char c : str) {
                bytes_.push_back(c);
            }
            bytes_.push_back('\0');
        }
    }

    return bytes_;
}


void ObjectFile::save(const char* path)
{
    flash_filesystem::store_file_data_binary(path, export_data(), {.use_compression_ = true});
}


template <typename T>
Optional<T> read_mem(Vector<char>::Iterator& it, Vector<char>::Iterator end)
{
    T result;
    for (u32 i = 0; i < sizeof(T); ++i) {
        if (it == end) {
            return nullopt();
        }
        ((char*)&result)[i] = *(it++);
    }
    return result;
}


u32 parse_and_link_bytecode(Vector<char>::Iterator it,
                            ObjectFile::RelocationTable& relocation_table,
                            bool relocatable)
{
    u32 bc_size = 0;

    int depth = 0;
    auto hdr = it;
    while (true) {
        auto op = *hdr;
        if (op == instruction::PushString::op()) {
            instruction::PushString ps;
            for (u32 i = 0; i < sizeof ps; ++i) {
                ((u8*)&ps)[i] = *(hdr++);
            }
            auto appended_len = ps.length_;
            while (appended_len) {
                ++hdr;
                --appended_len;
            }
            bc_size += sizeof(instruction::PushString) + ps.length_;
        } else {
            instruction::Header h;
            h.op_ = op;
            auto sz = instruction::instruction_size(h);
            bc_size += sz;

            if (relocatable) {
                using namespace instruction;
                if (is_symtab_opcode(&h)) {
                    auto field = hdr;
                    ++field; // skip opcode
                    HostInteger<Symbol::SymtabIndex> idx;
                    auto r = field;
                    for (u32 b = 0; b < sizeof(idx); ++b) {
                        ((u8*)&idx)[b] = *r;
                        ++r;
                    }
                    idx.set(ObjectFile::link_symbol(idx.get(),
                                                    relocation_table,
                                                    relocatable));
                    for (u32 b = 0; b < sizeof(idx); ++b) {
                        *field = ((char*)&idx)[b];
                        ++field;
                    }
                }
            }

            while (sz) {
                ++hdr;
                --sz;
            }
        }
        switch (op) {
        case instruction::PushLambda::op():
            ++depth;
            break;

        case instruction::RetNil::op():
        case instruction::Ret::op():
            if (depth == 0) {
                goto DONE;
            } else {
                --depth;
            }
            break;
        }
    }
DONE:;
    return bc_size;
}


static bool load_relocation_table(Vector<char>& bytes,
                                  Vector<char>::Iterator& it,
                                  ObjectFile::RelocationTable& output)
{
    auto info = read_mem<ObjectFile::RelocationTableInfo>(it, bytes.end());
    if (not info) {
        return false;
    }

    auto rtab_it = bytes.begin();
    for (u32 i = 0; i < info->offset_.get(); ++i) {
        if (rtab_it == bytes.end()) {
            return false;
        }
        ++rtab_it;
    }

    StringBuffer<32> current;
    while (rtab_it not_eq bytes.end()) {
        char c = *(rtab_it++);
        if (c == '\0') {
            output.push_back(current);
            current.clear();
        } else {
            current.push_back(c);
        }
    }

    return true;
}


bool ObjectFile::disassemble(const char* path, Vector<char>& output)
{
    output.clear();

    Vector<char> bytes;
    load_file_contents(bytes, path);
    auto it = bytes.begin();

    if (bytes.size() < sizeof(Header)) {
        return false;
    }

    auto hdr = read_mem<Header>(it, bytes.end());
    if (not hdr) {
        return false;
    }

    const bool relocatable = hdr->has_flag(Header::Flag::relocatable);
    const u16 definition_count = hdr->definition_count_.get();

    // For relocatable files, read the relocation table so we can
    // resolve symbol names.
    Vector<StringBuffer<32>> reloc_table;

    if (relocatable) {
        if (not load_relocation_table(bytes, it, reloc_table)) {
            return false;
        }
    }

    auto resolve_name = [&](u16 sym_index) -> const char* {
        if (relocatable) {
            if (sym_index < reloc_table.size()) {
                return reloc_table[sym_index].c_str();
            }
            return "???";
        } else {
            return load_from_symtab(sym_index * symtab_stride);
        }
    };

    auto print = [&output](const char* str) {
        while (*str not_eq '\0') {
            output.push_back(*(str++));
        }
    };

    print("\nSkyland LISP OBJ File\n\n");
    print(::format<64>("% functions, % bytes",
                       definition_count,
                       bytes.size())
              .c_str());
    print("\n\n");

    if (relocatable) {
        print("(relocatable)\n\n");
    }

    for (int i = 0; i < definition_count; ++i) {
        auto def_header = read_mem<Definition>(it, bytes.end());
        if (not def_header) {
            return false;
        }

        auto bc_size = parse_and_link_bytecode(it,
                                               reloc_table,
                                               relocatable);

        auto name = resolve_name(def_header->sym_.get());
        for (int j = 0; j < 30; ++j) {
            output.push_back('_');
        }
        print("\n\n(");
        print(name);
        auto sig = def_header->sig_;
        if (sig.required_args_) {
            u8 args = sig.required_args_;
            if (args > 0) {
                print(" ");
                print(repr_arg_type(sig.arg0_type_));
            }
            if (args > 1) {
                print(" ");
                print(repr_arg_type(sig.arg1_type_));
            }
            if (args > 2) {
                print(" ");
                print(repr_arg_type(sig.arg2_type_));
            }
            if (args > 3) {
                print(" ");
                print(repr_arg_type(sig.arg3_type_));
            }
            if (args > 4) {
                print("...");
            }
        }
        print("):");
        print("\n\n");

        auto buf = make_zeroed_sbr("obj-disassembly-buffer");
        for (u32 j = 0; j < bc_size; ++j) {
            buf->data_[j] = *(it++);
        }

        instruction::disassemble(&*buf, 0, [&print](const char* str) {
            print(str);
            print("\n\n");
        });
    }

    return true;
}


Value* ObjectFile::load(const Fingerprint& f, const char* path)
{
    ListBuilder result;

    definition_count_ = 0;
    bytes_.clear();

    load_file_contents(bytes_, path);
    auto it = bytes_.begin();

    if (bytes_.size() < sizeof(Header)) {
        return L_NIL;
    }

    auto header = read_mem<Header>(it, bytes_.end());
    if (not header) {
        return L_NIL;
    }

    relocatable_ = header->has_flag(Header::Flag::relocatable);

    if (not relocatable_ and
        memcmp(&header->fingerprint_, &f, sizeof f) not_eq 0) {
        return L_NIL;
    }

    definition_count_ = header->definition_count_.get();

    if (relocatable_) {
        if (not load_relocation_table(bytes_, it, relocation_table_)) {
            return make_error(::format("failed to load relocation table from "
                                       "relocatable object file (%)",
                                       path));
        }
    }

    // Find the existing bytecode buffer and its free tail, once.
    auto existing_buffer = get_bytecode_buffer();
    Value* target_buf = nullptr;
    int write_cursor = 0;

    if (existing_buffer not_eq L_NIL) {
        target_buf = existing_buffer;
        auto scratch = target_buf->databuffer().value();
        write_cursor = SCRATCH_BUFFER_SIZE - 1;
        for (; write_cursor > 0; --write_cursor) {
            if ((Opcode)scratch->data_[write_cursor] !=
                instruction::Fatal::op()) {
                ++write_cursor;
                break;
            }
        }
    }

    for (int i = 0; i < definition_count_; ++i) {
        auto def_header = read_mem<Definition>(it, bytes_.end());
        if (not def_header) {
            return L_NIL;
        }

        const auto sym_index = link_symbol(def_header->sym_.get(),
                                           relocation_table_,
                                           relocatable_);

        auto bc_size = parse_and_link_bytecode(it,
                                               relocation_table_,
                                               relocatable_);

        // Check we have enough bytes remaining in the file
        // (avoid walking past end in the copy loop)
        {
            auto check = it;
            for (u32 j = 0; j < bc_size; ++j) {
                if (check == bytes_.end()) {
                    return L_NIL;
                }
                ++check;
            }
        }

        if (not target_buf or
            (SCRATCH_BUFFER_SIZE - write_cursor) < (int)bc_size) {

            push_op(make_databuffer("lisp-bytecode"));
            if (get_op(0)->type() not_eq Value::Type::databuffer) {
                pop_op();
                return L_NIL;
            }
            target_buf = get_op(0);
            get_bytecode_buffer() = target_buf;
            pop_op();
            write_cursor = 0;
        }

        auto scratch = target_buf->databuffer().value();

        // Copy bytecode into the target buffer
        int offset = write_cursor;
        for (u32 j = 0; j < bc_size; ++j) {
            scratch->data_[write_cursor++] = *(it++);
        }

        // Build the function value: cons(offset, databuffer)
        Protected off(make_integer(offset));
        push_op(make_cons(off, target_buf));
        if (get_op(0)->type() not_eq Value::Type::cons) {
            pop_op();
            return L_NIL;
        }
        auto fn = make_bytecode_function(get_op(0));
        pop_op();
        if (fn->type() not_eq Value::Type::function) {
            return L_NIL;
        }
        fn->function().sig_ = def_header->sig_;

        // Bind the function to its symbol
        Protected pfn(fn);
        Protected sym(make_symtab_symbol(sym_index));
        result.push_back(make_cons(sym, pfn));
    }

    return result.result();
}


void ObjectFile::load_file_contents(Vector<char>& output, const char* path)
{
    flash_filesystem::read_file_data(path, output);
    if (output.size() == 0) {
        auto [data, len] = PLATFORM.load_file("", path);
        if (len) {
            for (u32 i = 0; i < len; ++i) {
                output.push_back(data[i]);
            }
        }
    }
}


} // namespace lisp
