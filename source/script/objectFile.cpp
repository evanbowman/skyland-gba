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
#include "platform/flash_filesystem.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"


namespace lisp
{


using DefinitionCountField = host_u16;


ObjectFile::ObjectFile(const Fingerprint& f)
{
    append((void*)&f, sizeof f);

    for (u32 i = 0; i < sizeof(DefinitionCountField); ++i) {
        append((void*)"\0", 1);
    }
}


void ObjectFile::append(void* data, u32 len)
{
    for (u32 i = 0; i < len; ++i) {
        bytes_.push_back(((u8*)data)[i]);
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

    auto begin = data->data_ + start_offset;
    auto end = instr.back();

    using namespace instruction;
    if (end->op_ not_eq Ret::op() and end->op_ not_eq RetNil::op()) {
        LOGIC_ERROR();
    }

    auto bytecode_size = ((char*)end - begin) + sizeof(Ret);

    Definition def;
    def.sym_.set(sym);
    def.sig_ = fn.sig_;

    append(&def, sizeof(def));
    append(begin, bytecode_size);

    ++definition_count_;
}


void ObjectFile::save(const char* path)
{
    auto it = bytes_.begin();
    for (u32 i = 0; i < sizeof(Fingerprint); ++i) {
        // Skip over fingerprint to definition count field
        ++it;
    }

    auto def_count = (DefinitionCountField*)&*it;
    def_count->set(definition_count_);

    flash_filesystem::store_file_data_binary(path, bytes_, {
            .use_compression_ = true
        });
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


u32 find_bc_size(Vector<char>::Iterator it)
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


bool ObjectFile::disassemble(const char* path, Vector<char>& output)
{
    output.clear();
    u16 definition_count = 0;

    Vector<char> bytes;
    flash_filesystem::read_file_data(path, bytes);
    auto it = bytes.begin();

    if (bytes.size() < sizeof(DefinitionCountField) + sizeof(Fingerprint)) {
        return false;
    }

    read_mem<Fingerprint>(it, bytes.end());

    auto def_count = read_mem<DefinitionCountField>(it, bytes.end());
    if (not def_count) {
        return false;
    }
    definition_count = def_count->get();

    auto print = [&output](const char* str) {
        while (*str not_eq '\0') {
            output.push_back(*(str++));
        }
    };

    print("\nSkyland LISP OBJ File\n\n");
    print(::format<32>("% functions, % bytes\n\n",
                       definition_count,
                       bytes.size()).c_str());

    for (int i = 0; i < definition_count; ++i) {
        auto def_header = read_mem<Definition>(it, bytes.end());
        if (not def_header) {
            return false;
        }

        const auto sym_index = def_header->sym_.get();

        auto bc_size = find_bc_size(it);

        auto name = load_from_symtab(sym_index * symtab_stride);
        for (int i = 0; i < 30; ++i) {
            output.push_back('_');
        }
        print("\n\n");
        print(name);
        print(":\n\n");

        auto buf = make_zeroed_sbr("obj-disassembly-buffer");
        for (u32 j = 0; j < bc_size; ++j) {
            buf->data_[j] = *(it++);
        }

        instruction::disassemble(&*buf,
                                 0,
                                 [&print](const char* str) {
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

    flash_filesystem::read_file_data(path, bytes_);
    auto it = bytes_.begin();

    if (bytes_.size() < sizeof(DefinitionCountField) + sizeof(Fingerprint)) {
        return L_NIL;
    }

    auto fingerprint = read_mem<Fingerprint>(it, bytes_.end());
    if (not fingerprint) {
        return L_NIL;
    }
    if (memcmp(&*fingerprint, &f, sizeof f) not_eq 0) {
        return L_NIL;
    }

    auto def_count = read_mem<DefinitionCountField>(it, bytes_.end());
    if (not def_count) {
        return L_NIL;
    }
    definition_count_ = def_count->get();

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

        const auto sym_index = def_header->sym_.get();

        auto bc_size = find_bc_size(it);

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


}
