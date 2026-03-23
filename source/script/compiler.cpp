////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "bytecode.hpp"
#include "lisp.hpp"
#include "number/endian.hpp"
#ifndef __GBA__
#include <iostream>
#endif
#include "lisp_internal.hpp"


namespace lisp
{


using instruction::InstructionList;
using instruction::parse_instructions;


Value* make_bytecode_function(Value* bytecode);


u16 symbol_offset(const char* symbol);


struct CompilerContext
{
};


int compile_impl(CompilerContext& ctx,
                 ScratchBuffer& buffer,
                 int write_pos,
                 Value* code,
                 int jump_offset,
                 bool tail_expr);


template <typename Instruction>
static Instruction*
append(CompilerContext& ctx, ScratchBuffer& buffer, int& write_pos)
{
    if (write_pos + sizeof(Instruction) >= SCRATCH_BUFFER_SIZE) {
        // FIXME: propagate error! We should return nullptr, but the caller does
        // not check the return value yet. Now, a lambda that takes up 2kb of
        // bytecode seems highly unlikely in the first place, but you never know
        // I guess...
        PLATFORM.fatal("bytecode for function is too large!");
    }

    auto result = (Instruction*)(buffer.data_ + write_pos);
    result->header_.op_ = Instruction::op();
    write_pos += sizeof(Instruction);
    return result;
}


int compile_fn(CompilerContext& ctx,
               ScratchBuffer& buffer,
               int write_pos,
               Value* code,
               int jump_offset)
{
    bool first = true;

    auto lat = code;

    if (length(lat) == 0) {
        append<instruction::PushNil>(ctx, buffer, write_pos);
    }

    while (lat not_eq get_nil()) {
        if (lat->type() not_eq Value::Type::cons) {
            // error...
            break;
        }

        if (not first) {
            append<instruction::Pop>(ctx, buffer, write_pos);
        } else {
            first = false;
        }

        bool tail_expr = lat->cons().cdr() == get_nil();

        write_pos = compile_impl(
            ctx, buffer, write_pos, lat->cons().car(), jump_offset, tail_expr);

        lat = lat->cons().cdr();
    }

    append<instruction::Ret>(ctx, buffer, write_pos);

    return write_pos;
}


int compile_quoted(CompilerContext& ctx,
                   ScratchBuffer& buffer,
                   int write_pos,
                   Value* code,
                   bool tail_expr)
{
    if (code->type() == Value::Type::integer) {
        write_pos = compile_impl(ctx, buffer, write_pos, code, 0, tail_expr);
    } else if (code->type() == Value::Type::symbol) {
        if (code->symbol().hdr_.mode_bits_ == (u8)Symbol::ModeBits::small) {
            auto inst =
                append<instruction::PushSmallSymbol>(ctx, buffer, write_pos);
            auto name = code->symbol().name();
            memcpy(inst->name_, name, Symbol::buffer_size);
        } else {
            if (auto symtab_index = get_symtab_index(code->symbol())) {
                auto inst =
                    append<instruction::LoadSymtab>(ctx, buffer, write_pos);
                inst->symtab_index_.set(*symtab_index);
            } else {
                auto inst =
                    append<instruction::PushSymbolRT>(ctx, buffer, write_pos);
                inst->ptr_.set(code->symbol().name());
            }
        }

    } else if (code->type() == Value::Type::cons) {
        auto cdr = code->cons().cdr();
        if (cdr->type() not_eq Value::Type::cons and
            cdr->type() not_eq Value::Type::nil) {
            write_pos = compile_quoted(
                ctx, buffer, write_pos, code->cons().car(), tail_expr);
            write_pos = compile_quoted(ctx, buffer, write_pos, cdr, tail_expr);
            append<instruction::MakePair>(ctx, buffer, write_pos);
        } else {
            int list_len = 0;

            while (code not_eq get_nil()) {
                if (code->type() not_eq Value::Type::cons) {
                    // ...
                    break;
                }
                write_pos = compile_quoted(
                    ctx, buffer, write_pos, code->cons().car(), tail_expr);

                code = code->cons().cdr();

                list_len++;

                if (list_len == 255) {
                    PLATFORM.fatal(
                        "cannot compile list with more than 255 slots!");
                }
            }

            auto inst = append<instruction::PushList>(ctx, buffer, write_pos);
            inst->element_count_ = list_len;
        }
    } else if (code->type() == Value::Type::nil) {
        append<instruction::PushNil>(ctx, buffer, write_pos);
    }

    return write_pos;
}


int compile_progn(CompilerContext& ctx,
                  ScratchBuffer& buffer,
                  int write_pos,
                  Value* code,
                  int jump_offset,
                  bool tail_expr)
{
    if (code->type() not_eq Value::Type::cons) {
        while (true)
            ;
        // TODO: raise error
    }

    bool first = true;

    while (code not_eq get_nil()) {

        bool tail = tail_expr and code->cons().cdr() == get_nil();

        if (not first) {
            append<instruction::Pop>(ctx, buffer, write_pos);
        } else {
            first = false;
        }

        write_pos = compile_impl(
            ctx, buffer, write_pos, code->cons().car(), jump_offset, tail);

        code = code->cons().cdr();
    }

    return write_pos;
}


int compile_let(CompilerContext& ctx,
                ScratchBuffer& buffer,
                int write_pos,
                Value* code,
                int jump_offset,
                bool tail_expr)
{
    if (code->type() not_eq Value::Type::cons) {
        while (true)
            ;
        // TODO: raise error
    }

    const auto binding_count = length(code->cons().car());

    if (binding_count not_eq 0) {
        append<instruction::LexicalFramePush>(ctx, buffer, write_pos);
    }

    l_foreach(code->cons().car(), [&](Value* val) {
        if (val->type() == Value::Type::cons) {
            auto sym = val->cons().car();
            auto bind = val->cons().cdr();
            if (sym->type() == Value::Type::symbol and
                bind->type() == Value::Type::cons) {

                write_pos = compile_impl(ctx,
                                         buffer,
                                         write_pos,
                                         bind->cons().car(),
                                         jump_offset,
                                         false);

                if (sym->hdr_.mode_bits_ == (u8)Symbol::ModeBits::small) {
                    auto inst = append<instruction::LexicalDefSmall>(
                        ctx, buffer, write_pos);
                    auto name = sym->symbol().name();
                    memcpy(inst->name_, name, Symbol::buffer_size);
                } else {
                    if (auto symtab_index = get_symtab_index(sym->symbol())) {
                        auto inst = append<instruction::LexicalDef>(
                            ctx, buffer, write_pos);
                        inst->symtab_index_.set(*symtab_index);
                    } else {
                        auto inst = append<instruction::LexicalDefRT>(
                            ctx, buffer, write_pos);
                        inst->ptr_.set(sym->symbol().name());
                    }
                }
            } else if (sym->type() == Value::Type::cons) {
                PLATFORM.fatal("destructuring let unimplemented for compiled "
                               "bytecode!");
            }
        }
    });

    code = code->cons().cdr();

    bool first = true;

    while (code not_eq get_nil()) {

        bool tail = tail_expr and code->cons().cdr() == get_nil();

        if (not first) {
            append<instruction::Pop>(ctx, buffer, write_pos);
        } else {
            first = false;
        }

        write_pos = compile_impl(
            ctx, buffer, write_pos, code->cons().car(), jump_offset, tail);

        code = code->cons().cdr();
    }

    if (binding_count not_eq 0) {
        append<instruction::LexicalFramePop>(ctx, buffer, write_pos);
    }

    return write_pos;
}


bool is_quoted_symbol(Value* v)
{
    return v->type() == Value::Type::cons and
           v->cons().cdr()->type() == Value::Type::symbol and
           v->cons().car()->type() == Value::Type::symbol and
           str_eq(v->cons().car()->symbol().name(), "'");
}


int compile_impl(CompilerContext& ctx,
                 ScratchBuffer& buffer,
                 int write_pos,
                 Value* code,
                 int jump_offset,
                 bool tail_expr)
{
TOP:
    if (code->type() == Value::Type::ratio) {
        auto r = append<instruction::PushRatio>(ctx, buffer, write_pos);
        r->num_.set(dcompr(code->ratio().numerator_)->integer().value_);
        r->div_.set(code->ratio().divisor_);
    } else if (code->type() == Value::Type::nil) {

        append<instruction::PushNil>(ctx, buffer, write_pos);

    } else if (code->type() == Value::Type::integer) {

        if (code->integer().value_ == 0) {
            append<instruction::Push0>(ctx, buffer, write_pos);
        } else if (code->integer().value_ == 1) {
            append<instruction::Push1>(ctx, buffer, write_pos);
        } else if (code->integer().value_ == 2) {
            append<instruction::Push2>(ctx, buffer, write_pos);
        } else if (code->integer().value_ < 127 and
                   code->integer().value_ > -127) {

            append<instruction::PushSmallInteger>(ctx, buffer, write_pos)
                ->value_ = code->integer().value_;

        } else {
            append<instruction::PushInteger>(ctx, buffer, write_pos)
                ->value_.set(code->integer().value_);
        }
    } else if (code->type() == Value::Type::fp) {
        auto f = code->fp().value_;
        append<instruction::PushFloat>(ctx, buffer, write_pos)->f_.set(f);
    } else if (code->type() == Value::Type::string) {
        const auto str = code->string().value();
        const auto len = strlen(str);
        append<instruction::PushString>(ctx, buffer, write_pos)->length_ =
            len + 1;

        if (write_pos + len + 1 >= SCRATCH_BUFFER_SIZE) {
            while (true)
                ;
        }

        for (u32 i = 0; i < len; ++i) {
            *(buffer.data_ + write_pos++) = str[i];
        }

        *(buffer.data_ + write_pos++) = '\0';

    } else if (code->type() == Value::Type::array) {
        PLATFORM.fatal("bytecode compilation unsupported for array literals, "
                       "try using the (array ...) or (make-array n) functions "
                       "instead");
    } else if (code->type() == Value::Type::symbol) {

        if (code->symbol().name()[0] == '$' and
            code->symbol().name()[1] not_eq 'V') {
            s32 argn = 0;
            for (u32 i = 1; code->symbol().name()[i] not_eq '\0'; ++i) {
                argn = argn * 10 + (code->symbol().name()[i] - '0');
            }

            switch (argn) {
            case 0:
                append<instruction::Arg0>(ctx, buffer, write_pos);
                break;

            case 1:
                append<instruction::Arg1>(ctx, buffer, write_pos);
                break;

            case 2:
                append<instruction::Arg2>(ctx, buffer, write_pos);
                break;

            default:
                append<instruction::PushSmallInteger>(ctx, buffer, write_pos)
                    ->value_ = argn;
                append<instruction::Arg>(ctx, buffer, write_pos);
                break;
            }

        } else {

            // NOTE: the stack optimizer replaces local variable loads with
            // register loads if possible later on.

            if (code->symbol().hdr_.mode_bits_ == (u8)Symbol::ModeBits::small) {
                auto inst =
                    append<instruction::LoadVarSmall>(ctx, buffer, write_pos);
                auto name = code->symbol().name();
                memcpy(inst->name_, name, Symbol::buffer_size);
            } else {
                if (auto symtab_index = get_symtab_index(code->symbol())) {
                    auto inst =
                        append<instruction::LoadVarS>(ctx, buffer, write_pos);
                    inst->symtab_index_.set(*symtab_index);
                } else {
                    append<instruction::LoadVarRT>(ctx, buffer, write_pos)
                        ->ptr_.set(code->symbol().name());
                }
            }
        }

    } else if (code->type() == Value::Type::cons) {

        auto lat = code;

        auto fn = lat->cons().car();

        if (fn->type() == Value::Type::symbol and
            str_eq(fn->symbol().name(), "let")) {

            write_pos = compile_let(ctx,
                                    buffer,
                                    write_pos,
                                    lat->cons().cdr(),
                                    jump_offset,
                                    tail_expr);

        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "progn")) {
            write_pos = compile_progn(ctx,
                                      buffer,
                                      write_pos,
                                      lat->cons().cdr(),
                                      jump_offset,
                                      tail_expr);
        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "await")) {
            lat = lat->cons().cdr();
            if (lat->type() not_eq Value::Type::cons) {
                while (true)
                    ; // TODO: raise error!
            }
            write_pos = compile_impl(
                ctx, buffer, write_pos, lat->cons().car(), jump_offset, false);
            append<instruction::Await>(ctx, buffer, write_pos);
            append<instruction::Resume>(ctx, buffer, write_pos);
        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "set") and length(lat) == 3 and
                   is_quoted_symbol(lat->cons().cdr()->cons().car())) {

            auto sym = lat->cons().cdr()->cons().car()->cons().cdr();

            auto arg = lat->cons().cdr()->cons().cdr();
            write_pos = compile_impl(
                ctx, buffer, write_pos, arg->cons().car(), jump_offset, false);

            if (sym->symbol().hdr_.mode_bits_ == (u8)Symbol::ModeBits::small) {
                auto inst =
                    append<instruction::SetVarSmall>(ctx, buffer, write_pos);
                auto name = sym->symbol().name();
                memcpy(inst->name_, name, Symbol::buffer_size);
            } else {
                if (auto symtab_index = get_symtab_index(sym->symbol())) {
                    auto inst =
                        append<instruction::SetVar>(ctx, buffer, write_pos);
                    inst->symtab_index_.set(*symtab_index);
                } else {
                    append<instruction::SetVarRT>(ctx, buffer, write_pos)
                        ->ptr_.set(sym->symbol().name());
                }
            }

        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "if")) {

            lat = lat->cons().cdr();
            if (lat->type() not_eq Value::Type::cons) {
                while (true)
                    ; // TODO: raise error!
            }

            write_pos = compile_impl(
                ctx, buffer, write_pos, lat->cons().car(), jump_offset, false);

            auto jne = append<instruction::JumpIfFalse>(ctx, buffer, write_pos);

            auto true_branch = get_nil();
            auto false_branch = get_nil();

            if (lat->cons().cdr()->type() == Value::Type::cons) {
                true_branch = lat->cons().cdr()->cons().car();

                if (lat->cons().cdr()->cons().cdr()->type() ==
                    Value::Type::cons) {
                    false_branch =
                        lat->cons().cdr()->cons().cdr()->cons().car();
                }
            }

            write_pos = compile_impl(
                ctx, buffer, write_pos, true_branch, jump_offset, tail_expr);

            auto jmp = append<instruction::Jump>(ctx, buffer, write_pos);

            jne->offset_.set(write_pos - jump_offset);

            write_pos = compile_impl(
                ctx, buffer, write_pos, false_branch, jump_offset, tail_expr);

            jmp->offset_.set(write_pos - jump_offset);

        } else if (fn->type() == Value::Type::symbol and
                   (str_eq(fn->symbol().name(), "lambda"))) {
            // A lambda has not yet ben transfomed to it's intermediate
            // representation. Expand it now...
            auto body = lat->cons().cdr();
            perform_argument_substitution(body);
            lat->cons().set_car(make_symbol("fn"));
            lat->cons().set_cdr(lat->cons().cdr()->cons().cdr());
            goto TOP;
        } else if (fn->type() == Value::Type::symbol and
                   (str_eq(fn->symbol().name(), "fn"))) {

            lat = lat->cons().cdr();

            if (lat->type() not_eq Value::Type::cons) {
                while (true)
                    ; // TODO: raise error!
            }

            auto lambda =
                append<instruction::PushLambda>(ctx, buffer, write_pos);

            auto lambda_start_pos = write_pos;

            bool first = true;

            if (length(lat) == 0) {
                append<instruction::PushNil>(ctx, buffer, write_pos);
            }

            while (lat not_eq get_nil()) {
                if (lat->type() not_eq Value::Type::cons) {
                    // error...
                    break;
                }

                if (not first) {
                    append<instruction::Pop>(ctx, buffer, write_pos);
                } else {
                    first = false;
                }

                bool tail_expr = lat->cons().cdr() == get_nil();

                write_pos = compile_impl(ctx,
                                         buffer,
                                         write_pos,
                                         lat->cons().car(),
                                         lambda_start_pos,
                                         tail_expr);

                lat = lat->cons().cdr();
            }

            append<instruction::Ret>(ctx, buffer, write_pos);

            lambda->lambda_end_.set(write_pos - jump_offset);

        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "'")) {
            write_pos = compile_quoted(
                ctx, buffer, write_pos, lat->cons().cdr(), tail_expr);
        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "while")) {
            lat = lat->cons().cdr();
            const auto loop_top = write_pos;
            write_pos = compile_impl(
                ctx, buffer, write_pos, lat->cons().car(), jump_offset, false);
            auto jne = append<instruction::JumpIfFalse>(ctx, buffer, write_pos);
            auto true_branch = lat->cons().cdr();
            bool first = true;
            while (true_branch not_eq get_nil()) {
                if (not first) {
                    append<instruction::Pop>(ctx, buffer, write_pos);
                } else {
                    first = false;
                }
                write_pos = compile_impl(ctx,
                                         buffer,
                                         write_pos,
                                         true_branch->cons().car(),
                                         jump_offset,
                                         false);
                true_branch = true_branch->cons().cdr();
            }
            append<instruction::Pop>(ctx, buffer, write_pos);
            auto jmp = append<instruction::Jump>(ctx, buffer, write_pos);
            jmp->offset_.set(loop_top - jump_offset);
            jne->offset_.set(write_pos - jump_offset);
            append<instruction::PushNil>(ctx, buffer, write_pos);
        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "`")) {
            PLATFORM.fatal("TODO: cannot bytecode-compile quasiquote!");
        } else {
            u8 argc = 0;

            lat = lat->cons().cdr();

            // Compile each function arument
            while (lat not_eq get_nil()) {
                if (lat->type() not_eq Value::Type::cons) {
                    // ...
                    break;
                }

                write_pos = compile_impl(ctx,
                                         buffer,
                                         write_pos,
                                         lat->cons().car(),
                                         jump_offset,
                                         false);

                lat = lat->cons().cdr();

                argc++;

                if (argc == 255) {
                    // FIXME: raise error!
                    while (true)
                        ;
                }
            }

            if (fn->type() == Value::Type::symbol and
                str_eq(fn->symbol().name(), "cons") and argc == 2) {

                append<instruction::MakePair>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "+")) {

                append<instruction::Add>(ctx, buffer, write_pos)->operands_ =
                    argc;

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "*")) {

                append<instruction::Multiply>(ctx, buffer, write_pos)
                    ->operands_ = argc;

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "/") and argc == 2) {

                append<instruction::Divide>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "-") and argc == 2) {

                append<instruction::Subtract>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "incr") and argc == 1) {

                append<instruction::Incr>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "decr") and argc == 1) {

                append<instruction::Decr>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "length") and argc == 1) {

                append<instruction::Length>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "get") and argc == 2) {

                append<instruction::Get>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       (str_eq(fn->symbol().name(), "car")) and argc == 1) {

                append<instruction::First>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       (str_eq(fn->symbol().name(), "cdr")) and argc == 1) {

                append<instruction::Rest>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "<") and argc == 2) {

                append<instruction::CmpLess>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), ">") and argc == 2) {

                append<instruction::CmpGreater>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "this") and argc == 0) {
                append<instruction::PushThis>(ctx, buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "not") and argc == 1) {
                append<instruction::Not>(ctx, buffer, write_pos);
            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "set") and argc == 2) {
                append<instruction::Set>(ctx, buffer, write_pos);
            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "equal") and argc == 2) {
                append<instruction::IsEqual>(ctx, buffer, write_pos);
            } else {

                write_pos = compile_impl(
                    ctx, buffer, write_pos, fn, jump_offset, false);

                if (tail_expr) {
                    switch (argc) {
                    case 1:
                        append<instruction::TailCall1>(ctx, buffer, write_pos);
                        break;

                    case 2:
                        append<instruction::TailCall2>(ctx, buffer, write_pos);
                        break;

                    case 3:
                        append<instruction::TailCall3>(ctx, buffer, write_pos);
                        break;

                    default:
                        append<instruction::TailCall>(ctx, buffer, write_pos)
                            ->argc_ = argc;
                        break;
                    }
                } else {
                    switch (argc) {
                    case 1:
                        append<instruction::Funcall1>(ctx, buffer, write_pos);
                        break;

                    case 2:
                        append<instruction::Funcall2>(ctx, buffer, write_pos);
                        break;

                    case 3:
                        append<instruction::Funcall3>(ctx, buffer, write_pos);
                        break;

                    default:
                        append<instruction::Funcall>(ctx, buffer, write_pos)
                            ->argc_ = argc;
                        break;
                    }
                }
            }
        }

    } else {
        append<instruction::PushNil>(ctx, buffer, write_pos);
    }
    return write_pos;
}


void live_values(::Function<6 * sizeof(void*), void(Value&)> callback);


class Optimizer
{
public:
    template <typename U, typename T>
    void replace(InstructionList& instructions,
                 ScratchBuffer& code_buffer,
                 U& dest,
                 T& source,
                 u32& code_size)
    {
        static_assert(sizeof dest > sizeof source);
        memcpy(&dest, &source, sizeof source);


        auto diff = (sizeof dest) - (sizeof source);

        if (diff == 0) {
            return;
        }

        const int start = ((u8*)&dest - (u8*)code_buffer.data_) + sizeof source;

        for (u32 i = start; i < code_size - diff; ++i) {
            code_buffer.data_[i] = code_buffer.data_[i + diff];
        }

        code_size -= diff;

        for (auto& p : instructions) {
            if ((char*)p >= code_buffer.data_ + start) {
                p = (instruction::Header*)((u8*)p - diff);
            }
        }

        fixup_jumps(instructions, code_buffer, start, -diff);
    }


    template <typename T>
    void remove(InstructionList& instructions,
                ScratchBuffer& code_buffer,
                T* inst,
                u32& code_size)
    {
        const int start = (u8*)inst - (u8*)code_buffer.data_;

        for (u32 i = start; i < code_size - sizeof(T); ++i) {
            code_buffer.data_[i] = code_buffer.data_[i + sizeof(T)];
        }

        code_size -= sizeof(T);

        for (auto it = instructions.begin(); it not_eq instructions.end();) {
            if (*it == (instruction::Header*)inst) {
                instructions.erase(it);
                break;
            } else {
                ++it;
            }
        }

        for (auto& p : instructions) {
            if ((char*)p >= code_buffer.data_ + start) {
                p = (instruction::Header*)((u8*)p - sizeof(T));
            }
        }

        fixup_jumps(instructions, code_buffer, start, -(int)sizeof(T));
    }


    // We have just inserted/removed an instruction, and now need to scan
    // through the bytecode, and adjust the offsets of local jumps within the
    // lambda definition.
    void fixup_jumps(InstructionList& instr,
                     ScratchBuffer& code_buffer,
                     int inflection_point,
                     int size_diff)
    {
        // info(::format("before fixup: inflection %, diff % (instruction count %)",
        //               inflection_point,
        //               size_diff,
        //               instr.size()));
        // Platform::RemoteConsole::Line out;
        // instruction::disassemble(&code_buffer, 0, [&out](const char* opcode) {
        //     out += opcode;
        //     out += "\r\n";
        // });
        // info(out.c_str());

        int index = 0;
        int depth = 0;

        struct LambdaInfo
        {
            int start_ = 0;
        };

        Buffer<LambdaInfo, 15, false> function_stack;
        function_stack.push_back({0});

        while ( // index < instr.size()
            true) {
            using namespace instruction;

            auto inst = instr[index];
            auto inst_offset = (int)((u8*)inst - (u8*)code_buffer.data_);
            int local_inflection =
                inflection_point - function_stack[depth].start_;

            switch (inst->op_) {
            case PushLambda::op(): {
                int lambda_start = inst_offset + sizeof(PushLambda);
                auto end = ((PushLambda*)inst)->lambda_end_.get();
                if (local_inflection >= 0 && end > local_inflection) {
                    ((PushLambda*)inst)->lambda_end_.set(end + size_diff);
                }
                function_stack.push_back({lambda_start});
                ++depth;
                ++index;
                break;
            }

            case Fatal::op():
                return;

            case RetNil::op():
            case Ret::op():
                if (depth == 0) {
                    return;
                }
                function_stack.pop_back();
                --depth;
                ++index;
                break;

            case Jump::op(): {
                auto offset = ((Jump*)inst)->offset_.get();
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((Jump*)inst)->offset_.set(offset + size_diff);
                }
                ++index;
                break;
            }

            case JumpIfFalse::op(): {
                auto offset = ((JumpIfFalse*)inst)->offset_.get();
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((JumpIfFalse*)inst)->offset_.set(offset + size_diff);
                }
                ++index;
                break;
            }

            case SmallJump::op(): {
                auto offset = ((SmallJump*)inst)->offset_;
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((SmallJump*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;
            }

            case SmallJumpIfFalse::op(): {
                auto offset = ((SmallJumpIfFalse*)inst)->offset_;
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((SmallJumpIfFalse*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;
            }

            case SmallJumpIfTrue::op(): {
                auto offset = ((SmallJumpIfTrue*)inst)->offset_;
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((SmallJumpIfTrue*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;
            }

            case SmallJumpNotEqual::op(): {
                auto offset = ((SmallJumpNotEqual*)inst)->offset_;
                if (local_inflection >= 0 && offset > local_inflection) {
                    ((SmallJumpNotEqual*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;
            }

            default:
                ++index;
                break;
            }
        }
    }


    using JumpTargets = Buffer<int, 128>;
    void collect_jump_targets(InstructionList& instructions,
                              ScratchBuffer& code_buffer,
                              JumpTargets& targets)
    {
        targets.clear();
        int index = 0;
        int depth = 0;

        struct LambdaInfo
        {
            int start_ = 0;
        };
        Buffer<LambdaInfo, 15> function_stack;
        function_stack.push_back({0});

        while (true) {
            using namespace instruction;
            auto inst = instructions[index];
            auto inst_offset = (int)((u8*)inst - (u8*)code_buffer.data_);
            int scope_start = function_stack[depth].start_;

            switch (inst->op_) {
            case Fatal::op():
                return;

            case PushLambda::op(): {
                int lambda_start = inst_offset + sizeof(PushLambda);
                function_stack.push_back({lambda_start});
                ++depth;
                ++index;
                break;
            }
            case RetNil::op():
            case Ret::op():
                if (depth == 0)
                    return;
                function_stack.pop_back();
                --depth;
                ++index;
                break;
            case Jump::op():
                targets.push_back(scope_start + ((Jump*)inst)->offset_.get());
                ++index;
                break;
            case JumpIfFalse::op():
                targets.push_back(scope_start +
                                  ((JumpIfFalse*)inst)->offset_.get());
                ++index;
                break;
            case SmallJump::op():
                targets.push_back(scope_start + ((SmallJump*)inst)->offset_);
                ++index;
                break;
            case SmallJumpIfFalse::op():
                targets.push_back(scope_start +
                                  ((SmallJumpIfFalse*)inst)->offset_);
                ++index;
                break;
            case SmallJumpIfTrue::op():
                targets.push_back(scope_start +
                                  ((SmallJumpIfTrue*)inst)->offset_);
                ++index;
                break;
            case SmallJumpNotEqual::op():
                targets.push_back(scope_start +
                                  ((SmallJumpNotEqual*)inst)->offset_);
                ++index;
                break;
            default:
                ++index;
                break;
            }
        }
    }


    bool is_jump_target(instruction::Header* inst,
                        ScratchBuffer& code_buffer,
                        Buffer<int, 128>& targets)
    {
        int abs = (int)((u8*)inst - (u8*)code_buffer.data_);
        for (auto& t : targets) {
            if (t == abs)
                return true;
        }
        return false;
    }
};



class PeepholeOptimizer : public Optimizer
{
public:
    u32 run(InstructionList& instructions,
            ScratchBuffer& code_buffer,
            u32 code_size)
    {

    TOP:
        int index = 0;
        int depth = 0;
        JumpTargets targets;
        collect_jump_targets(instructions, code_buffer, targets);

        struct LambdaInfo
        {
            int start_ = 0;
        };
        Buffer<LambdaInfo, 15, true> scope_stack;
        scope_stack.push_back({0});

        using namespace instruction;

        while (true) {
            auto inst = instructions[index];
            switch (inst->op_) {
            case Fatal::op():
                return code_size;

            case RetNilIfFalse::op(): {
                if (index > 1 and index < (int)instructions.size() - 1) {
                    auto prev = instructions[index - 1];
                    auto next = instructions[index + 1];
                    static_assert(sizeof(LoadReg) == sizeof(StoreRegKeep));
                    if ((prev->op_ == LoadReg::op() or
                         prev->op_ == StoreRegKeep::op()) and
                        next->op_ == LoadReg::op() and
                        ((LoadReg*)prev)->reg_ == ((LoadReg*)next)->reg_ and
                        not is_jump_target(inst, code_buffer, targets) and
                        not is_jump_target(next, code_buffer, targets)) {
                        inst->op_ = RetNilIfFalseKeep::op();
                        remove(instructions,
                               code_buffer,
                               (LoadReg*)next,
                               code_size);
                        goto TOP;
                    } else if ((prev->op_ == LoadVarS::op() and
                                next->op_ == LoadVarS::op()) and
                               ((LoadVarS*)prev)->symtab_index_.get() ==
                                   ((LoadVarS*)next)->symtab_index_.get() and
                               not is_jump_target(
                                   inst, code_buffer, targets) and
                               not is_jump_target(next, code_buffer, targets)) {
                        inst->op_ = RetNilIfFalseKeep::op();
                        remove(instructions,
                               code_buffer,
                               (LoadVarS*)next,
                               code_size);
                        goto TOP;
                    }
                }
                ++index;
                break;
            }

            case RetNil::op(): {
                auto prev = instructions[index - 1];
                if (prev->op_ == EarlyRet::op()) {
                    // NOTE: after a few passes of optmizations, EarlyRet
                    // followed by RetNil is a common scenario, and often,
                    // RetNil has no branches to it, because branches to RetNil
                    // have been replaced by RetNilIfFalse.
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        prev->op_ = Ret::op();
                        remove(instructions,
                               code_buffer,
                               (RetNil*)inst,
                               code_size);
                        goto TOP;
                    }
                } else if (prev->op_ == EarlyRetNil::op()) {
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        prev->op_ = RetNil::op();
                        remove(instructions,
                               code_buffer,
                               (EarlyRetNil*)inst,
                               code_size);
                        goto TOP;
                    }
                }
                scope_stack.pop_back();
                ++index;
                if (depth-- == 0) {
                    goto CLEANUP;
                }
                break;
            }

            case Ret::op():
                scope_stack.pop_back();
                ++index;
                if (depth-- == 0) {
                    goto CLEANUP;
                }
                break;

            case PushLambda::op(): {
                auto inst_offset = (int)((u8*)inst - (u8*)code_buffer.data_);
                int lambda_start = inst_offset + sizeof(PushLambda);
                scope_stack.push_back({lambda_start});
                ++index;
                ++depth;
                break;
            }

            case PushSmallInteger::op(): {
                if (index > 0) {
                    auto prev = instructions[index - 1];
                    if (prev->op_ == PushSmallInteger::op()) {
                        if (((PushSmallInteger*)prev)->value_ ==
                            ((PushSmallInteger*)inst)->value_) {

                            Dup d;
                            d.header_.op_ = Dup::op();
                            replace(instructions,
                                    code_buffer,
                                    *(PushSmallInteger*)inst,
                                    d,
                                    code_size);
                            goto TOP;
                        }
                    } else if (prev->op_ == Dup::op()) {
                        int backtrack = index - 2;
                        while (backtrack > 0) {
                            auto it = instructions[backtrack];
                            if (it->op_ == Dup::op()) {
                                --backtrack;
                            } else if (it->op_ == PushSmallInteger::op()) {
                                if (((PushSmallInteger*)it)->value_ ==
                                    ((PushSmallInteger*)inst)->value_) {

                                    Dup d;
                                    d.header_.op_ = Dup::op();
                                    replace(instructions,
                                            code_buffer,
                                            *(PushSmallInteger*)inst,
                                            d,
                                            code_size);
                                    // code_size -= sizeof(PushSmallInteger) - sizeof(Dup);
                                    goto TOP;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                }
                ++index;
                break;
            }

            case SmallJumpIfFalse::op(): {
                SmallJumpIfFalse* jif = (SmallJumpIfFalse*)inst;
                int abs_target = scope_stack[depth].start_ + jif->offset_;
                if (code_buffer.data_[abs_target] == RetNil::op() or
                    code_buffer.data_[abs_target] == EarlyRetNil::op()) {
                    RetNilIfFalse rnf;
                    rnf.header_.op_ = RetNilIfFalse::op();
                    replace(instructions, code_buffer, *jif, rnf, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case SmallJumpIfTrue::op(): {
                SmallJumpIfTrue* jit = (SmallJumpIfTrue*)inst;
                int abs_target = scope_stack[depth].start_ + jit->offset_;
                if (code_buffer.data_[abs_target] == RetNil::op() or
                    code_buffer.data_[abs_target] == EarlyRetNil::op()) {
                    RetNilIfTrue rnt;
                    rnt.header_.op_ = RetNilIfTrue::op();
                    replace(instructions, code_buffer, *jit, rnt, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case SmallJump::op(): {
                SmallJump* sj = (SmallJump*)inst;
                int abs_target = scope_stack[depth].start_ + sj->offset_;
                if (code_buffer.data_[abs_target] == Ret::op() or
                    code_buffer.data_[abs_target] == EarlyRet::op()) {
                    // A jump to a return instruction can be replaced with a return
                    // instruction.
                    EarlyRet r;
                    r.header_.op_ = EarlyRet::op();
                    replace(instructions, code_buffer, *sj, r, code_size);
                    goto TOP;
                } else if (code_buffer.data_[abs_target] == RetNil::op()) {
                    EarlyRetNil rn;
                    rn.header_.op_ = EarlyRetNil::op();
                    replace(instructions, code_buffer, *sj, rn, code_size);
                    goto TOP;
                } else if (code_buffer.data_[abs_target] == SmallJump::op()) {
                    // A jump to a jump is a bit wasteful... let's collapse it.
                    auto target_jmp =
                        (SmallJump*)(code_buffer.data_ + abs_target);
                    sj->offset_ = target_jmp->offset_;
                    goto TOP;
                } else {
                    ++index;
                }
                break;
            }

            case PushInteger::op(): {
                if (index > 0) {
                    auto prev = instructions[index - 1];
                    if (prev->op_ == PushInteger::op()) {
                        if (((PushInteger*)prev)->value_.get() ==
                            ((PushInteger*)inst)->value_.get()) {

                            Dup d;
                            d.header_.op_ = Dup::op();
                            replace(instructions,
                                    code_buffer,
                                    *(PushInteger*)inst,
                                    d,
                                    code_size);
                            goto TOP;
                        }
                    } else if (prev->op_ == Dup::op()) {
                        int backtrack = index - 2;
                        while (backtrack > 0) {
                            auto it = instructions[backtrack];
                            if (it->op_ == Dup::op()) {
                                --backtrack;
                            } else if (it->op_ == PushInteger::op()) {
                                if (((PushInteger*)it)->value_.get() ==
                                    ((PushInteger*)inst)->value_.get()) {

                                    Dup d;
                                    d.header_.op_ = Dup::op();
                                    replace(instructions,
                                            code_buffer,
                                            *(PushInteger*)inst,
                                            d,
                                            code_size);
                                    goto TOP;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                }
                ++index;
                break;
            }

            case Jump::op(): {
                if (auto o = ((Jump*)inst)->offset_.get(); o > 0 and o < 256) {
                    SmallJump j;
                    j.header_.op_ = SmallJump::op();
                    j.offset_ = ((Jump*)inst)->offset_.get();
                    replace(
                        instructions, code_buffer, *(Jump*)inst, j, code_size);
                    goto TOP;
                }
                Jump* jmp = (Jump*)inst;
                int abs_target = scope_stack[depth].start_ + jmp->offset_.get();
                if (code_buffer.data_[abs_target] == Ret::op() or
                    code_buffer.data_[abs_target] == EarlyRet::op()) {
                    // A jump to a return instruction can be replaced with a return
                    // instruction.
                    EarlyRet r;
                    r.header_.op_ = EarlyRet::op();
                    replace(instructions, code_buffer, *jmp, r, code_size);
                    goto TOP;
                } else if (code_buffer.data_[abs_target] == RetNil::op()) {
                    EarlyRetNil rn;
                    rn.header_.op_ = EarlyRetNil::op();
                    replace(instructions, code_buffer, *jmp, rn, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case JumpIfFalse::op(): {
                if (auto o = ((Jump*)inst)->offset_.get(); o > 0 and o < 256) {
                    SmallJumpIfFalse j;
                    j.header_.op_ = SmallJumpIfFalse::op();
                    j.offset_ = ((JumpIfFalse*)inst)->offset_.get();
                    replace(instructions,
                            code_buffer,
                            *(JumpIfFalse*)inst,
                            j,
                            code_size);
                    goto TOP;
                }
                JumpIfFalse* jif = (JumpIfFalse*)inst;
                int abs_target = scope_stack[depth].start_ + jif->offset_.get();
                if (code_buffer.data_[abs_target] == RetNil::op() or
                    code_buffer.data_[abs_target] == EarlyRetNil::op()) {
                    RetNilIfFalse rnf;
                    rnf.header_.op_ = RetNilIfFalse::op();
                    replace(instructions, code_buffer, *jif, rnf, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case PushNil::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == Ret::op() and
                    not is_jump_target(next, code_buffer, targets)) {

                    ((PushNil*)inst)->header_.op_ = RetNil::op();
                    remove(instructions, code_buffer, (Ret*)next, code_size);
                    goto TOP;
                } else if (next->op_ == EarlyRet::op() and
                           not is_jump_target(next, code_buffer, targets)) {
                    ((PushNil*)inst)->header_.op_ = EarlyRetNil::op();
                    remove(
                        instructions, code_buffer, (EarlyRet*)next, code_size);
                }
                ++index;
                break;
            }

            case LexicalFramePop::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == Ret::op() or next->op_ == RetNil::op()) {
                    remove(instructions,
                           code_buffer,
                           (LexicalFramePop*)inst,
                           code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case Add::op(): {
                if (index > 0) {
                    auto instr = (Add*)inst;
                    auto prev = instructions[index - 1];
                    if (instr->operands_ == 2 and prev->op_ == Push1::op() and
                        not is_jump_target(inst, code_buffer, targets)) {
                        static_assert(sizeof(Push1) == sizeof(Incr));
                        prev->op_ = Incr::op();
                        remove(instructions, code_buffer, instr, code_size);
                        goto TOP;
                    }
                }
                ++index;
                break;
            }

            case Subtract::op(): {
                if (index > 0) {
                    auto instr = (Subtract*)inst;
                    auto prev = instructions[index - 1];
                    if (prev->op_ == Push1::op() and
                        not is_jump_target(inst, code_buffer, targets)) {
                        static_assert(sizeof(Push1) == sizeof(Decr));
                        prev->op_ = Decr::op();
                        remove(instructions, code_buffer, instr, code_size);
                        goto TOP;
                    }
                }
                ++index;
                break;
            }

            case StoreRegKeep::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == SmallJump::op()) {
                    // This warrants some explanation. If we're jumping to the
                    // address of a pop, and we're keeping the value that we're
                    // writing to a register on the stack, we can instead
                    // increment the jump address to skip over the pop
                    // instruction, which, if the pop instruction no longer is a
                    // jump target, may collapse against a different instruction
                    // in a later pass.
                    auto sj = (SmallJump*)next;
                    int abs_target = scope_stack[depth].start_ + sj->offset_;
                    if (sj->offset_ < std::numeric_limits<
                                          decltype(sj->offset_)>::max() and
                        code_buffer.data_[abs_target] == Pop::op() and
                        not is_jump_target(next, code_buffer, targets)) {
                        inst->op_ = StoreReg::op();
                        sj->offset_++;
                        goto TOP;
                    }
                }
                ++index;
                break;
            }

            case Pop::op(): {
                auto prev = instructions[index - 1];
                switch (prev->op_) {
                case PushNil::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        remove(instructions,
                               code_buffer,
                               (PushNil*)prev,
                               code_size);
                        goto TOP;
                    }
                    break;

                case StoreRegKeep::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        prev->op_ = StoreReg::op();
                        goto TOP;
                    }
                    break;

                case LoadCall0::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        prev->op_ = LoadCall0Discard::op();
                        goto TOP;
                    }
                    break;

                case LoadCall1::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        prev->op_ = LoadCall1Discard::op();
                        goto TOP;
                    }
                    break;

                case LoadCall2::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        prev->op_ = LoadCall2Discard::op();
                        goto TOP;
                    }
                    break;

                case LoadCall3::op():
                    if (not is_jump_target(inst, code_buffer, targets)) {
                        remove(
                            instructions, code_buffer, (Pop*)inst, code_size);
                        prev->op_ = LoadCall3Discard::op();
                        goto TOP;
                    }
                    break;
                }
                ++index;
                break;
            }

            case IsEqual::op(): {
                auto instr = (IsEqual*)inst;
                auto next = instructions[index + 1];
                if (next->op_ == SmallJumpIfFalse::op() and
                    not is_jump_target(inst, code_buffer, targets)) {
                    static_assert(sizeof(SmallJumpNotEqual) ==
                                  sizeof(SmallJumpIfFalse));
                    next->op_ = SmallJumpNotEqual::op();
                    remove(instructions, code_buffer, instr, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case Not::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == Not::op() and
                    not is_jump_target(inst, code_buffer, targets) and
                    not is_jump_target(next, code_buffer, targets)) {
                    remove(instructions, code_buffer, (Not*)next, code_size);
                    remove(instructions, code_buffer, (Not*)inst, code_size);
                    goto TOP;
                } else if (next->op_ == SmallJumpIfFalse::op() and
                           not is_jump_target(inst, code_buffer, targets) and
                           not is_jump_target(next, code_buffer, targets)) {
                    // NOTE: this else/if for jump condition inversion runs
                    // after the not/not detection, because eliminating double
                    // not is a bigger priority.
                    static_assert(
                        sizeof(SmallJumpIfTrue) == sizeof(SmallJumpIfFalse) and
                        std::is_base_of<SmallJumpIfFalse, SmallJumpIfTrue>());
                    next->op_ = SmallJumpIfTrue::op();
                    remove(instructions, code_buffer, (Not*)inst, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case Dup::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == StoreReg::op() &&
                    !is_jump_target(next, code_buffer, targets)) {
                    // DUP → STORE_REG(n) → STORE_REG_KEEP(n)
                    next->op_ = StoreRegKeep::op();
                    static_assert(sizeof(StoreRegKeep) == sizeof(StoreReg));
                    remove(instructions, code_buffer, (Dup*)inst, code_size);
                    goto TOP;
                }
                ++index;
                break;
            }

            case StoreReg::op(): {
                auto next = instructions[index + 1];
                if (next->op_ == LoadReg::op()) {
                    if (((LoadReg*)next)->reg_ == ((StoreReg*)inst)->reg_ and
                        not is_jump_target(next, code_buffer, targets)) {
                        // If we're a store reg, and the next instruction is a
                        // load_reg for the same register, we can instead dup
                        // and convert the next instruction to a store reg.
                        static_assert(sizeof(LoadReg) == sizeof(StoreReg));
                        next->op_ = StoreReg::op();
                        Dup d;
                        d.header_.op_ = Dup::op();
                        replace(instructions,
                                code_buffer,
                                *(StoreReg*)inst,
                                d,
                                code_size);
                        goto TOP;
                    }
                } else if (next->op_ == SmallJump::op()) {
                    auto sj = (SmallJump*)next;
                    int abs_target = scope_stack[depth].start_ + sj->offset_;
                    if (sj->offset_ + sizeof(LoadReg) <=
                            std::numeric_limits<
                                decltype(sj->offset_)>::max() and
                        code_buffer.data_[abs_target] == LoadReg::op() and
                        (((LoadReg*)(code_buffer.data_ + abs_target))->reg_ ==
                         ((StoreReg*)inst)->reg_) and
                        not is_jump_target(next, code_buffer, targets)) {
                        // NOTE: we're a store reg opcode followed by a jump to
                        // a loadreg opcode for the same register. If the jump
                        // itself is not a jump target, then we can update the
                        // jump address to skip over the loadreg instruction and
                        // convert the StoreReg opcode to a StoreRegKeep.
                        sj->offset_ += sizeof(LoadReg);
                        inst->op_ = StoreRegKeep::op();
                        goto TOP;
                    }
                }
                ++index;
                break;
            }

            case LoadVarS::op(): {
                if (index + 1 < (int)instructions.size()) {
                    auto next = instructions[index + 1];
                    switch (next->op_) {
                    case TailCall1::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadTCall1::op();
                        remove(instructions,
                               code_buffer,
                               (TailCall1*)next,
                               code_size);
                        goto TOP;

                    case TailCall2::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadTCall2::op();
                        remove(instructions,
                               code_buffer,
                               (TailCall2*)next,
                               code_size);
                        goto TOP;

                    case TailCall3::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadTCall3::op();
                        remove(instructions,
                               code_buffer,
                               (TailCall3*)next,
                               code_size);
                        goto TOP;

                    case Funcall1::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadCall1::op();
                        remove(instructions,
                               code_buffer,
                               (Funcall1*)next,
                               code_size);
                        goto TOP;

                    case Funcall2::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadCall2::op();
                        remove(instructions,
                               code_buffer,
                               (Funcall2*)next,
                               code_size);
                        goto TOP;

                    case Funcall3::op():
                        ((LoadVarS*)inst)->header_.op_ = LoadCall3::op();
                        remove(instructions,
                               code_buffer,
                               (Funcall3*)next,
                               code_size);
                        goto TOP;

                    case Funcall::op(): {
                        auto fc = ((Funcall*)next);
                        if (fc->argc_ == 0) {
                            ((LoadVarS*)inst)->header_.op_ = LoadCall0::op();
                            remove(instructions,
                                   code_buffer,
                                   (Funcall*)next,
                                   code_size);
                            goto TOP;
                        }
                        break;
                    }

                    case MakePair::op(): {
                        auto n2 = instructions[index + 2];
                        if (n2->op_ == SetVar::op()) {
                            if (((LoadVarS*)inst)->symtab_index_.get() ==
                                ((SetVar*)n2)->symtab_index_.get()) {
                                auto cv = (ConsVar*)inst;
                                cv->header_.op_ = ConsVar::op();
                                static_assert(sizeof(SetVar) ==
                                              sizeof(ConsVar));
                                remove(instructions,
                                       code_buffer,
                                       (SetVar*)n2,
                                       code_size);
                                remove(instructions,
                                       code_buffer,
                                       (MakePair*)next,
                                       code_size);
                                goto TOP;
                            }
                        }
                        break;
                    }

                    default:
                        break;
                    }
                }
                ++index;
                break;
            }

            default:
                index++;
                break;
            }
        }

    CLEANUP:
        struct RegisterMap
        {
            u8 reg_[256];
        };
        auto loaded_registers = allocate_small<RegisterMap>({"register-map"});
        for (u32 i = 0; i < instructions.size(); ++i) {
            auto inst = instructions[i];
            switch (inst->op_) {
            case LoadReg::op(): {
                auto instr = (LoadReg*)inst;
                loaded_registers->reg_[instr->reg_] = true;
                switch (instr->reg_) {
                case 0: {
                    LoadReg0 lr0;
                    lr0.header_.op_ = LoadReg0::op();
                    replace(instructions, code_buffer, *instr, lr0, code_size);
                    break;
                }

                case 1: {
                    LoadReg1 lr1;
                    lr1.header_.op_ = LoadReg1::op();
                    replace(instructions, code_buffer, *instr, lr1, code_size);
                    break;
                }

                case 2: {
                    LoadReg2 lr2;
                    lr2.header_.op_ = LoadReg2::op();
                    replace(instructions, code_buffer, *instr, lr2, code_size);
                    break;
                }
                }
                ++index;
                break;
            }
            }
        }
        targets.clear();
        collect_jump_targets(instructions, code_buffer, targets);
        for (u32 i = 0; i < instructions.size(); ++i) {
            auto inst = instructions[i];
            switch (inst->op_) {
            case StoreRegKeep::op(): {
                if (not loaded_registers->reg_[((StoreRegKeep*)inst)->reg_] and
                    not is_jump_target(inst, code_buffer, targets)) {
                    remove(instructions,
                           code_buffer,
                           (StoreRegKeep*)inst,
                           code_size);
                    --i;
                    targets.clear();
                    collect_jump_targets(instructions, code_buffer, targets);
                } else {
                    auto instr = (StoreRegKeep*)inst;
                    switch (instr->reg_) {
                    case 0: {
                        StoreReg0Keep sr0;
                        sr0.header_.op_ = StoreReg0Keep::op();
                        replace(
                            instructions, code_buffer, *instr, sr0, code_size);
                        targets.clear();
                        collect_jump_targets(
                            instructions, code_buffer, targets);
                        break;
                    }

                    case 1: {
                        StoreReg1Keep sr1;
                        sr1.header_.op_ = StoreReg1Keep::op();
                        replace(
                            instructions, code_buffer, *instr, sr1, code_size);
                        targets.clear();
                        collect_jump_targets(
                            instructions, code_buffer, targets);
                        break;
                    }

                    case 2: {
                        StoreReg2Keep sr2;
                        sr2.header_.op_ = StoreReg2Keep::op();
                        replace(
                            instructions, code_buffer, *instr, sr2, code_size);
                        targets.clear();
                        collect_jump_targets(
                            instructions, code_buffer, targets);
                        break;
                    }
                    }
                }
                break;
            }

            case StoreReg::op(): {
                auto instr = (StoreReg*)inst;
                switch (instr->reg_) {
                case 0: {
                    StoreReg0 sr0;
                    sr0.header_.op_ = StoreReg0::op();
                    replace(instructions, code_buffer, *instr, sr0, code_size);
                    targets.clear();
                    collect_jump_targets(instructions, code_buffer, targets);
                    break;
                }

                case 1: {
                    StoreReg1 sr1;
                    sr1.header_.op_ = StoreReg1::op();
                    replace(instructions, code_buffer, *instr, sr1, code_size);
                    targets.clear();
                    collect_jump_targets(instructions, code_buffer, targets);
                    break;
                }

                case 2: {
                    StoreReg2 sr2;
                    sr2.header_.op_ = StoreReg2::op();
                    replace(instructions, code_buffer, *instr, sr2, code_size);
                    targets.clear();
                    collect_jump_targets(instructions, code_buffer, targets);
                    break;
                }
                }
                break;
            }
            }
        }
        return code_size;
    }
};


class StackOptimizer : public Optimizer
{
public:
    u32 run(InstructionList& instructions,
            ScratchBuffer& code_buffer,
            u32 code_size)
    {
        using namespace instruction;

        struct LexicalBinding
        {
            StringBuffer<32> symbol_name_string_;
            u8 frame_id_;
            int depth_;
        };

        Vector<LexicalBinding, SubBufferMemory> defs(
            make_sub_buffer("stack-opt-strs", 0));

        Vector<StringBuffer<32>, SubBufferMemory> captured_syms(
            make_sub_buffer("captured-syms", 0));

        auto is_captured = [&](const char* name) -> bool {
            for (auto& s : captured_syms) {
                if (s == name) {
                    return true;
                }
            }
            return false;
        };

        auto mark_captured = [&](const char* name) {
            if (not is_captured(name)) {
                captured_syms.push_back(name);
            }
        };

        auto check_captured = [&](const char* name, int depth) {
            for (auto& d : defs) {
                if (d.symbol_name_string_ == name && d.depth_ < depth) {
                    mark_captured(name);
                    break;
                }
            }
        };

        // Pre-scan: collect all defs with their depth, and detect captures
        {
            int depth = 0;
            u8 frame_id = 0;
            Buffer<u8, 15> frame_id_stack;

            for (auto& inst : instructions) {
                switch (inst->op_) {
                case PushLambda::op():
                    ++depth;
                    frame_id_stack.push_back(frame_id);
                    frame_id = 0;
                    break;

                case Fatal::op():
                    break;

                case RetNil::op():
                case Ret::op():
                    if (depth > 0) {
                        --depth;
                        frame_id = frame_id_stack.back();
                        frame_id_stack.pop_back();
                    }
                    break;

                case LexicalFramePush::op():
                    ++frame_id;
                    break;

                case LexicalFramePop::op():
                    --frame_id;
                    break;

                case LexicalDef::op():
                    defs.push_back(
                        {load_from_symtab(
                             ((LexicalDef*)inst)->symtab_index_.get() *
                             symtab_stride),
                         frame_id,
                         depth});
                    break;

                case LexicalDefRT::op():
                    defs.push_back(
                        {((LexicalDefRT*)inst)->ptr_.get(), frame_id, depth});
                    break;

                case LexicalDefSmall::op(): {
                    auto lds = ((LexicalDefSmall*)inst);
                    StringBuffer<3> name;
                    for (int i = 0; i < 3; ++i) {
                        name.__push_unsafe(lds->name_[i]);
                    }
                    defs.push_back({name.c_str(), frame_id, depth});
                    break;
                }

                case LoadVarS::op(): {
                    auto name = load_from_symtab(
                        ((LoadVarS*)inst)->symtab_index_.get() * symtab_stride);
                    check_captured(name, depth);
                    break;
                }

                case LoadVarSmall::op(): {
                    auto lvs = ((LoadVarSmall*)inst);
                    StringBuffer<3> name;
                    for (int i = 0; i < 3; ++i) {
                        name.__push_unsafe(lvs->name_[i]);
                    }
                    check_captured(name.c_str(), depth);
                    break;
                }

                case LoadVarRT::op():
                    check_captured(((LoadVarRT*)inst)->ptr_.get(), depth);
                    break;

                case SetVar::op(): {
                    auto name = load_from_symtab(
                        ((SetVar*)inst)->symtab_index_.get() * symtab_stride);
                    check_captured(name, depth);
                    break;
                }

                case SetVarSmall::op(): {
                    auto svs = ((SetVarSmall*)inst);
                    StringBuffer<3> name;
                    for (int i = 0; i < 3; ++i) {
                        name.__push_unsafe(svs->name_[i]);
                    }
                    check_captured(name.c_str(), depth);
                    break;
                }

                case SetVarRT::op():
                    check_captured(((SetVarRT*)inst)->ptr_.get(), depth);
                    break;
                }
            }
        }

        u8 frame_id = 0;
        Buffer<u8, 15, false> frame_id_stack;

        auto find_slot = [&](const char* name) -> Optional<int> {
            if (is_captured(name)) {
                return nullopt();
            }
            for (int i = defs.size() - 1; i > -1; --i) {
                if (defs[i].symbol_name_string_ == name and
                    defs[i].frame_id_ <= frame_id) {
                    return i;
                }
            }
            return nullopt();
        };

    RESTART:
        frame_id = 0;
        frame_id_stack.clear();
        bool dirty = false;

        for (u32 i = 0; i < instructions.size(); ++i) {
            auto inst = instructions[i];

            switch (inst->op_) {
            case Fatal::op():
                break;

            case PushLambda::op():
                frame_id_stack.push_back(frame_id);
                frame_id = 0;
                break;

            case RetNil::op():
            case Ret::op():
                if (frame_id_stack.size() > 0) {
                    frame_id = frame_id_stack.back();
                    frame_id_stack.pop_back();
                }
                break;

            case LexicalFramePush::op():
                ++frame_id;
                break;

            case LexicalFramePop::op():
                --frame_id;
                break;

            case LexicalDef::op(): {
                auto name = load_from_symtab(
                    ((LexicalDef*)inst)->symtab_index_.get() * symtab_stride);
                if (auto sl = find_slot(name)) {
                    StoreReg sr;
                    sr.header_.op_ = StoreReg::op();
                    sr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(LexicalDef*)inst,
                            sr,
                            code_size);
                    dirty = true;
                }
                break;
            }

            case LexicalDefRT::op(): {
                auto name = ((LexicalDefRT*)inst)->ptr_.get();
                if (auto sl = find_slot(name)) {
                    StoreReg sr;
                    sr.header_.op_ = StoreReg::op();
                    sr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(LexicalDefRT*)inst,
                            sr,
                            code_size);
                    dirty = true;
                }
                break;
            }

            case LexicalDefSmall::op(): {
                auto lds = ((LexicalDefSmall*)inst);
                StringBuffer<3> name;
                for (int i = 0; i < 3; ++i) {
                    name.__push_unsafe(lds->name_[i]);
                }
                if (auto sl = find_slot(name.c_str())) {
                    StoreReg sr;
                    sr.header_.op_ = StoreReg::op();
                    sr.reg_ = *sl;
                    replace(instructions, code_buffer, *lds, sr, code_size);
                    dirty = true;
                }
                break;
            }

            case SetVarSmall::op(): {
                auto svs = ((SetVarSmall*)inst);
                StringBuffer<3> name;
                for (int i = 0; i < 3; ++i) {
                    name.__push_unsafe(svs->name_[i]);
                }
                if (auto sl = find_slot(name.c_str())) {
                    StoreRegKeep sr;
                    sr.header_.op_ = StoreRegKeep::op();
                    sr.reg_ = *sl;
                    replace(instructions, code_buffer, *svs, sr, code_size);
                    dirty = true;
                }
                break;
            }

            case SetVar::op(): {
                auto name = load_from_symtab(
                    ((SetVar*)inst)->symtab_index_.get() * symtab_stride);
                if (auto sl = find_slot(name)) {
                    StoreRegKeep sr;
                    sr.header_.op_ = StoreRegKeep::op();
                    sr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(SetVar*)inst,
                            sr,
                            code_size);
                    dirty = true;
                }
                break;
            }

            case SetVarRT::op(): {
                auto name = ((SetVarRT*)inst)->ptr_.get();
                if (auto sl = find_slot(name)) {
                    StoreRegKeep sr;
                    sr.header_.op_ = StoreRegKeep::op();
                    sr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(SetVarRT*)inst,
                            sr,
                            code_size);
                    dirty = true;
                }
                break;
            }

            case LoadVarSmall::op(): {
                auto lvs = ((LoadVarSmall*)inst);
                StringBuffer<3> name;
                for (int i = 0; i < 3; ++i) {
                    name.__push_unsafe(lvs->name_[i]);
                }
                if (auto sl = find_slot(name.c_str())) {
                    LoadReg lr;
                    lr.header_.op_ = LoadReg::op();
                    lr.reg_ = *sl;
                    replace(instructions, code_buffer, *lvs, lr, code_size);
                    dirty = true;
                }
                break;
            }

            case LoadVarS::op(): {
                auto name = load_from_symtab(
                    ((LoadVarS*)inst)->symtab_index_.get() * symtab_stride);
                if (auto sl = find_slot(name)) {
                    LoadReg lr;
                    lr.header_.op_ = LoadReg::op();
                    lr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(LoadVarS*)inst,
                            lr,
                            code_size);
                    dirty = true;
                }
                break;
            }

            case LoadVarRT::op(): {
                auto name = ((LoadVarRT*)inst)->ptr_.get();
                if (auto sl = find_slot(name)) {
                    LoadReg lr;
                    lr.header_.op_ = LoadReg::op();
                    lr.reg_ = *sl;
                    replace(instructions,
                            code_buffer,
                            *(LoadVarRT*)inst,
                            lr,
                            code_size);
                    dirty = true;
                }
                break;
            }
            }
        }

        if (dirty) {
            goto RESTART;
        }

        auto depth_has_captures = [&](int d) -> bool {
            for (auto& def : defs) {
                if (def.depth_ == d &&
                    is_captured(def.symbol_name_string_.c_str())) {
                    return true;
                }
            }
            return false;
        };

        {
            int depth = 0;
            u32 index = 0;
            while (index < instructions.size()) {
                auto inst = instructions[index];
                switch (inst->op_) {
                case PushLambda::op():
                    ++depth;
                    ++index;
                    break;

                case Ret::op():
                case RetNil::op():
                    if (depth > 0) {
                        --depth;
                    }
                    ++index;
                    break;

                case LexicalFramePush::op():
                    if (not depth_has_captures(depth)) {
                        remove(instructions,
                               code_buffer,
                               (LexicalFramePush*)inst,
                               code_size);
                    } else {
                        ++index;
                    }
                    break;

                case LexicalFramePop::op():
                    if (not depth_has_captures(depth)) {
                        remove(instructions,
                               code_buffer,
                               (LexicalFramePop*)inst,
                               code_size);
                    } else {
                        ++index;
                    }
                    break;

                default:
                    ++index;
                    break;
                }
            }
        }

        return code_size;
    }
};


void compile(Value* code, CompileOptions opts)
{
    // We will be rendering all of our compiled code into this buffer.
    push_op(make_databuffer("lisp-bytecode"));
    if (get_op(0)->type() not_eq Value::Type::databuffer) {
        return;
    }

    push_op(make_cons(make_integer(0), get_op(0)));
    if (get_op(0)->type() not_eq Value::Type::cons) {
        return;
    }

    auto fn = make_bytecode_function(get_op(0));
    if (fn->type() not_eq Value::Type::function) {
        pop_op();
        pop_op();
        auto err = fn;
        push_op(err);
        return;
    }

    pop_op();
    auto buffer = get_op(0)->databuffer().value();
    pop_op();
    push_op(fn);

    // #ifndef _WIN32
    //     __builtin_memset(buffer->data_, 0, sizeof buffer->data_);
    // #else
    //     memset(buffer->data_, 0, sizeof buffer->data_);
    // #endif
    int write_pos = 0;

    CompilerContext ctx;
    write_pos = compile_fn(ctx, *buffer, write_pos, code, 0);

    auto& code_buffer =
        *fn->function().bytecode_impl_.databuffer()->databuffer().value();
    InstructionList instructions(make_scratch_buffer("instruction-buffer"));
    parse_instructions(code_buffer, instructions);

    if (opts.stack_optimizer_enabled_) {
        write_pos = StackOptimizer().run(instructions, code_buffer, write_pos);
    }

    if (opts.peephole_optimizer_enabled_) {
        write_pos =
            PeepholeOptimizer().run(instructions, code_buffer, write_pos);
    }

    // std::cout << "compilation finished, bytes used: " << write_pos <<
    // std::endl;

    // OK, so now, we've successfully compiled our function into the scratch
    // buffer. But, what about all the extra space in the buffer!? So we're
    // going to scan over all of the interpreter's allocated memory, and
    // collapse our own bytecode into a previously allocated slab of bytecode,
    // if possible.

    const int bytes_used = write_pos;

    auto existing_buffer = get_bytecode_buffer();
    if (existing_buffer == L_NIL) {
        // There's no existing bytecode buffer to merge our bytecode into.
        get_bytecode_buffer() = fn->function().bytecode_impl_.databuffer();
    } else {
        auto buf = existing_buffer;
        int used = SCRATCH_BUFFER_SIZE - 1;
        for (; used > 0; --used) {
            if ((Opcode)buf->databuffer().value()->data_[used] not_eq
                instruction::Fatal::op()) {
                ++used;
                break;
            }
        }

        const int remaining = SCRATCH_BUFFER_SIZE - used;
        if (remaining >= bytes_used) {
            // std::cout << "found another buffer with remaining space: "
            //           << remaining
            //           << ", copying " << bytes_used
            //           << " bytes to dest buffer offset "
            //           << used;

            auto src_buffer = fn->function().bytecode_impl_.databuffer();
            for (int i = 0; i < bytes_used; ++i) {
                buf->databuffer().value()->data_[used + i] =
                    src_buffer->databuffer().value()->data_[i];
            }

            Protected used_bytes(make_integer(used));

            fn->function().bytecode_impl_.bytecode_ =
                compr(make_cons(used_bytes, buf));
        } else {
            // No more space, replace the full buffer with our own bytecode
            // buffer.
            get_bytecode_buffer() = fn->function().bytecode_impl_.databuffer();
        }
    }
}


} // namespace lisp
