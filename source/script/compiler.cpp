////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#include "bytecode.hpp"
#include "lisp.hpp"
#include "number/endian.hpp"


namespace lisp {


Value* make_bytecode_function(Value* bytecode);


u16 symbol_offset(const char* symbol);


struct CompilerContext {
    Value* local_bindings_ = nullptr;
};


int compile_impl(CompilerContext& ctx,
                 ScratchBuffer& buffer,
                 int write_pos,
                 Value* code,
                 int jump_offset,
                 bool tail_expr);


template <typename Instruction>
static Instruction* append(ScratchBuffer& buffer, int& write_pos)
{
    if (write_pos + sizeof(Instruction) >= SCRATCH_BUFFER_SIZE) {
        // FIXME: propagate error! We should return nullptr, but the caller does
        // not check the return value yet. Now, a lambda that takes up 2kb of
        // bytecode seems highly unlikely in the first place, but you never know
        // I guess...
        while (true)
            ;
    }

    auto result = (Instruction*)(buffer.data_ + write_pos);
    result->header_.op_ = Instruction::op();
    write_pos += sizeof(Instruction);
    return result;
}


int compile_lambda(CompilerContext& ctx,
                   ScratchBuffer& buffer,
                   int write_pos,
                   Value* code,
                   int jump_offset)
{
    bool first = true;

    auto lat = code;
    while (lat not_eq get_nil()) {
        if (lat->type() not_eq Value::Type::cons) {
            // error...
            break;
        }

        if (not first) {
            append<instruction::Pop>(buffer, write_pos);
        } else {
            first = false;
        }

        bool tail_expr = lat->cons().cdr() == get_nil();

        write_pos = compile_impl(
            ctx, buffer, write_pos, lat->cons().car(), jump_offset, tail_expr);

        lat = lat->cons().cdr();
    }

    append<instruction::Ret>(buffer, write_pos);

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
            auto inst = append<instruction::PushSmallSymbol>(buffer, write_pos);
            auto name = code->symbol().name();
            memcpy(inst->name_, name, Symbol::buffer_size);
        } else {
            auto inst = append<instruction::PushSymbol>(buffer, write_pos);
            inst->ptr_.set(code->symbol().name());
        }

    } else if (code->type() == Value::Type::cons) {

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
                // FIXME: raise error!
                while (true)
                    ;
            }
        }

        auto inst = append<instruction::PushList>(buffer, write_pos);
        inst->element_count_ = list_len;
    } else if (code->type() == Value::Type::nil) {
        append<instruction::PushNil>(buffer, write_pos);
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
        append<instruction::LexicalFramePush>(buffer, write_pos);
    }

    auto prev_bindings = ctx.local_bindings_;
    ctx.local_bindings_ = code->cons().car();

    foreach (code->cons().car(), [&](Value* val) {
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
                    auto inst =
                        append<instruction::LexicalDefSmall>(buffer, write_pos);
                    auto name = sym->symbol().name();
                    memcpy(inst->name_, name, Symbol::buffer_size);
                } else {
                    auto inst =
                        append<instruction::LexicalDef>(buffer, write_pos);
                    inst->ptr_.set(sym->symbol().name());
                }
            }
        }
    })
        ;

    code = code->cons().cdr();

    while (code not_eq get_nil()) {

        bool tail = tail_expr and code->cons().cdr() == get_nil();

        write_pos = compile_impl(
            ctx, buffer, write_pos, code->cons().car(), jump_offset, tail);

        code = code->cons().cdr();
    }

    if (binding_count not_eq 0) {
        append<instruction::LexicalFramePop>(buffer, write_pos);
    }

    ctx.local_bindings_ = prev_bindings;

    return write_pos;
}


int compile_impl(CompilerContext& ctx,
                 ScratchBuffer& buffer,
                 int write_pos,
                 Value* code,
                 int jump_offset,
                 bool tail_expr)
{
    if (code->type() == Value::Type::nil) {

        append<instruction::PushNil>(buffer, write_pos);

    } else if (code->type() == Value::Type::integer) {

        if (code->integer().value_ == 0) {
            append<instruction::Push0>(buffer, write_pos);
        } else if (code->integer().value_ == 1) {
            append<instruction::Push1>(buffer, write_pos);
        } else if (code->integer().value_ == 2) {
            append<instruction::Push2>(buffer, write_pos);
        } else if (code->integer().value_ < 127 and
                   code->integer().value_ > -127) {

            append<instruction::PushSmallInteger>(buffer, write_pos)->value_ =
                code->integer().value_;

        } else {
            append<instruction::PushInteger>(buffer, write_pos)
                ->value_.set(code->integer().value_);
        }
    } else if (code->type() == Value::Type::string) {
        const auto str = code->string().value();
        const auto len = str_len(str);
        append<instruction::PushString>(buffer, write_pos)->length_ = len + 1;

        if (write_pos + len + 1 >= SCRATCH_BUFFER_SIZE) {
            while (true)
                ;
        }

        for (u32 i = 0; i < len; ++i) {
            *(buffer.data_ + write_pos++) = str[i];
        }

        *(buffer.data_ + write_pos++) = '\0';

    } else if (code->type() == Value::Type::symbol) {

        if (code->symbol().name()[0] == '$' and
            code->symbol().name()[1] not_eq 'V') {
            s32 argn = 0;
            for (u32 i = 1; code->symbol().name()[i] not_eq '\0'; ++i) {
                argn = argn * 10 + (code->symbol().name()[i] - '0');
            }

            switch (argn) {
            case 0:
                append<instruction::Arg0>(buffer, write_pos);
                break;

            case 1:
                append<instruction::Arg1>(buffer, write_pos);
                break;

            case 2:
                append<instruction::Arg2>(buffer, write_pos);
                break;

            default:
                append<instruction::PushSmallInteger>(buffer, write_pos)
                    ->value_ = argn;
                append<instruction::Arg>(buffer, write_pos);
                break;
            }

        } else {

            std::optional<int> local_slot;
            if (ctx.local_bindings_) {
                int i = 0;
                foreach (ctx.local_bindings_, [&](Value* val) {
                    auto sym = val->cons().car();
                    if (str_eq(sym->symbol().name(), code->symbol().name())) {
                        local_slot = i;
                        return;
                    } else {
                        ++i;
                    }
                })
                    ;
            }

            if (false and local_slot) {
                auto inst = append<instruction::LoadLocal>(buffer, write_pos);
                inst->var_slot_ = *local_slot;
            } else if (code->symbol().hdr_.mode_bits_ ==
                       (u8)Symbol::ModeBits::small) {
                auto inst =
                    append<instruction::LoadVarSmall>(buffer, write_pos);
                auto name = code->symbol().name();
                memcpy(inst->name_, name, Symbol::buffer_size);
            } else {
                append<instruction::LoadVar>(buffer, write_pos)
                    ->ptr_.set(code->symbol().name());
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
                   str_eq(fn->symbol().name(), "if")) {

            lat = lat->cons().cdr();
            if (lat->type() not_eq Value::Type::cons) {
                while (true)
                    ; // TODO: raise error!
            }

            write_pos = compile_impl(
                ctx, buffer, write_pos, lat->cons().car(), jump_offset, false);

            auto jne = append<instruction::JumpIfFalse>(buffer, write_pos);

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

            auto jmp = append<instruction::Jump>(buffer, write_pos);

            jne->offset_.set(write_pos - jump_offset);

            write_pos = compile_impl(
                ctx, buffer, write_pos, false_branch, jump_offset, tail_expr);

            jmp->offset_.set(write_pos - jump_offset);

        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "lambda")) {

            lat = lat->cons().cdr();

            if (lat->type() not_eq Value::Type::cons) {
                while (true)
                    ; // TODO: raise error!
            }

            auto lambda = append<instruction::PushLambda>(buffer, write_pos);

            // TODO: compile multiple nested expressions! FIXME... pretty broken.
            write_pos = compile_impl(ctx,
                                     buffer,
                                     write_pos,
                                     lat->cons().car(),
                                     jump_offset + write_pos,
                                     false);

            append<instruction::Ret>(buffer, write_pos);

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
            auto jne = append<instruction::JumpIfFalse>(buffer, write_pos);
            auto true_branch = lat->cons().cdr();
            bool first = true;
            while (true_branch not_eq get_nil()) {
                if (not first) {
                    append<instruction::Pop>(buffer, write_pos);
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
            append<instruction::Pop>(buffer, write_pos);
            auto jmp = append<instruction::Jump>(buffer, write_pos);
            jmp->offset_.set(loop_top - jump_offset);
            jne->offset_.set(write_pos - jump_offset);
            append<instruction::PushNil>(buffer, write_pos);
        } else if (fn->type() == Value::Type::symbol and
                   str_eq(fn->symbol().name(), "`")) {
            while (true)
                ;
            // TODO: Implement quasiquote for compiled code.
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

                append<instruction::MakePair>(buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "car") and argc == 1) {

                append<instruction::First>(buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "cdr") and argc == 1) {

                append<instruction::Rest>(buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "arg") and argc == 1) {

                append<instruction::Arg>(buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "this") and argc == 0) {
                append<instruction::PushThis>(buffer, write_pos);

            } else if (fn->type() == Value::Type::symbol and
                       str_eq(fn->symbol().name(), "not") and argc == 1) {
                append<instruction::Not>(buffer, write_pos);
            } else {

                write_pos = compile_impl(
                    ctx, buffer, write_pos, fn, jump_offset, false);

                if (tail_expr) {
                    switch (argc) {
                    case 1:
                        append<instruction::TailCall1>(buffer, write_pos);
                        break;

                    case 2:
                        append<instruction::TailCall2>(buffer, write_pos);
                        break;

                    case 3:
                        append<instruction::TailCall3>(buffer, write_pos);
                        break;

                    default:
                        append<instruction::TailCall>(buffer, write_pos)
                            ->argc_ = argc;
                        break;
                    }
                } else {
                    switch (argc) {
                    case 1:
                        append<instruction::Funcall1>(buffer, write_pos);
                        break;

                    case 2:
                        append<instruction::Funcall2>(buffer, write_pos);
                        break;

                    case 3:
                        append<instruction::Funcall3>(buffer, write_pos);
                        break;

                    default:
                        append<instruction::Funcall>(buffer, write_pos)->argc_ =
                            argc;
                        break;
                    }
                }
            }
        }

    } else {
        append<instruction::PushNil>(buffer, write_pos);
    }
    return write_pos;
}


void live_values(::Function<6 * sizeof(void*), void(Value&)> callback);


class PeepholeOptimizer {
public:
    template <typename U, typename T>
    void replace(ScratchBuffer& code_buffer, U& dest, T& source, u32& code_size)
    {
        static_assert(sizeof dest > sizeof source);
        memcpy(&dest, &source, sizeof source);


        auto diff = (sizeof dest) - (sizeof source);
        const int start = ((u8*)&dest - (u8*)code_buffer.data_) + sizeof source;

        for (u32 i = start; i < code_size - diff; ++i) {
            code_buffer.data_[i] = code_buffer.data_[i + diff];
        }

        code_size -= diff;

        fixup_jumps(code_buffer, start, -diff);
    }


    u32 run(ScratchBuffer& code_buffer, u32 code_size)
    {
        int index = 0;
        while (true) {
            using namespace instruction;
            auto inst = load_instruction(code_buffer, index);

            switch (inst->op_) {
            case PushLambda::op():
                // FIXME! We do not support optimization yet when there are
                // nested lambda definitions, we need to carefully adjust the
                // jump instruction coordinates in these cases, as jumps are
                // relative to the beginning of the lambda.
                return code_size;

            case Ret::op():
                goto TOP;

            default:
                ++index;
                break;
            }
        }

    TOP:
        index = 0;

        while (true) {
            using namespace instruction;

            auto inst = load_instruction(code_buffer, index);
            switch (inst->op_) {
            case Ret::op():
                return code_size;

            case PushLambda::op():
                return code_size;

            case PushSmallInteger::op(): {
                if (index > 0) {
                    auto prev = load_instruction(code_buffer, index - 1);
                    if (prev->op_ == PushSmallInteger::op()) {
                        if (((PushSmallInteger*)prev)->value_ ==
                            ((PushSmallInteger*)inst)->value_) {

                            Dup d;
                            d.header_.op_ = Dup::op();
                            replace(code_buffer,
                                    *(PushSmallInteger*)inst,
                                    d,
                                    code_size);
                            goto TOP;
                        }
                    } else if (prev->op_ == Dup::op()) {
                        int backtrack = index - 2;
                        while (backtrack > 0) {
                            auto it = load_instruction(code_buffer, backtrack);
                            if (it->op_ == Dup::op()) {
                                --backtrack;
                            } else if (it->op_ == PushSmallInteger::op()) {
                                if (((PushSmallInteger*)it)->value_ ==
                                    ((PushSmallInteger*)inst)->value_) {

                                    Dup d;
                                    d.header_.op_ = Dup::op();
                                    replace(code_buffer,
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

            case SmallJump::op(): {
                SmallJump* sj = (SmallJump*)inst;
                if (code_buffer.data_[sj->offset_] == Ret::op() or
                    code_buffer.data_[sj->offset_] == EarlyRet::op()) {
                    // A jump to a return instruction can be replaced with a return
                    // instruction.
                    EarlyRet r;
                    r.header_.op_ = EarlyRet::op();
                    replace(code_buffer, *sj, r, code_size);
                    goto TOP;
                } else {
                    ++index;
                }
                break;
            }

            case PushInteger::op(): {
                if (index > 0) {
                    auto prev = load_instruction(code_buffer, index - 1);
                    if (prev->op_ == PushInteger::op()) {
                        if (((PushInteger*)prev)->value_.get() ==
                            ((PushInteger*)inst)->value_.get()) {

                            Dup d;
                            d.header_.op_ = Dup::op();
                            replace(
                                code_buffer, *(PushInteger*)inst, d, code_size);
                            goto TOP;
                        }
                    } else if (prev->op_ == Dup::op()) {
                        int backtrack = index - 2;
                        while (backtrack > 0) {
                            auto it = load_instruction(code_buffer, backtrack);
                            if (it->op_ == Dup::op()) {
                                --backtrack;
                            } else if (it->op_ == PushInteger::op()) {
                                if (((PushInteger*)it)->value_.get() ==
                                    ((PushInteger*)inst)->value_.get()) {

                                    Dup d;
                                    d.header_.op_ = Dup::op();
                                    replace(code_buffer,
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

            case Jump::op():
                if (abs(((Jump*)inst)->offset_.get()) < 127) {
                    SmallJump j;
                    j.header_.op_ = SmallJump::op();
                    j.offset_ = ((Jump*)inst)->offset_.get();
                    replace(code_buffer, *(Jump*)inst, j, code_size);
                    goto TOP;
                }
                ++index;
                break;

            case JumpIfFalse::op():
                if (abs(((JumpIfFalse*)inst)->offset_.get()) < 127) {
                    SmallJumpIfFalse j;
                    j.header_.op_ = SmallJumpIfFalse::op();
                    j.offset_ = ((JumpIfFalse*)inst)->offset_.get();
                    replace(code_buffer, *(JumpIfFalse*)inst, j, code_size);
                    goto TOP;
                }
                ++index;
                break;

            default:
                index++;
                break;
            }
        }
    }


    // We have just inserted/removed an instruction, and now need to scan
    // through the bytecode, and adjust the offsets of local jumps within the
    // lambda definition.
    void
    fixup_jumps(ScratchBuffer& code_buffer, int inflection_point, int size_diff)
    {
        int index = 0;
        int depth = 0;

        struct LambdaInfo {
            int start_ = 0;
        };

        Buffer<LambdaInfo, 15> function_stack;
        function_stack.push_back({0});

        while (true) {
            using namespace instruction;

            auto inst = load_instruction(code_buffer, index);
            switch (inst->op_) {
            case PushLambda::op():
                ++depth;
                if (((PushLambda*)inst)->lambda_end_.get() > inflection_point) {
                    auto end = ((PushLambda*)inst)->lambda_end_.get();
                    ((PushLambda*)inst)->lambda_end_.set(end + size_diff);
                }
                ++index;
                break;

            case Ret::op():
                if (depth == 0) {
                    return;
                }
                --depth;
                ++index;
                break;

            case Jump::op():
                if (((Jump*)inst)->offset_.get() > inflection_point) {
                    auto offset = ((Jump*)inst)->offset_.get();
                    ((Jump*)inst)->offset_.set(offset + size_diff);
                }
                ++index;
                break;

            case JumpIfFalse::op():
                if (((JumpIfFalse*)inst)->offset_.get() > inflection_point) {
                    auto offset = ((JumpIfFalse*)inst)->offset_.get();
                    ((JumpIfFalse*)inst)->offset_.set(offset + size_diff);
                }
                ++index;
                break;

            case SmallJump::op():
                if (((SmallJump*)inst)->offset_ > inflection_point) {
                    auto offset = ((SmallJump*)inst)->offset_;
                    ((SmallJump*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;

            case SmallJumpIfFalse::op():
                if (((SmallJumpIfFalse*)inst)->offset_ > inflection_point) {
                    auto offset = ((SmallJumpIfFalse*)inst)->offset_;
                    ((SmallJumpIfFalse*)inst)->offset_ = offset + size_diff;
                }
                ++index;
                break;

            default:
                ++index;
                break;
            }
        }
    }
};


void compile(Value* code)
{
    // We will be rendering all of our compiled code into this buffer.
    push_op(make_databuffer("lisp-bytecode"));
    if (get_op(0)->type() not_eq Value::Type::data_buffer) {
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
    auto buffer = get_op(0)->data_buffer().value();
    pop_op();
    push_op(fn);

    __builtin_memset(buffer->data_, 0, sizeof buffer->data_);

    int write_pos = 0;

    CompilerContext ctx;
    write_pos = compile_lambda(ctx, *buffer, write_pos, code, 0);

    write_pos = PeepholeOptimizer().run(
        *fn->function().bytecode_impl_.databuffer()->data_buffer().value(),
        write_pos);

    // std::cout << "compilation finished, bytes used: " << write_pos <<
    // std::endl;

    // OK, so now, we've successfully compiled our function into the scratch
    // buffer. But, what about all the extra space in the buffer!? So we're
    // going to scan over all of the interpreter's allocated memory, and
    // collapse our own bytecode into a previously allocated slab of bytecode,
    // if possible.

    const int bytes_used = write_pos;
    bool done = false;

    live_values([fn, &done, bytes_used](Value& val) {
        if (done) {
            return;
        }

        if (fn not_eq &val and val.type() == Value::Type::function and
            val.hdr_.mode_bits_ == Function::ModeBits::lisp_bytecode_function) {

            auto buf = val.function().bytecode_impl_.databuffer();
            int used = SCRATCH_BUFFER_SIZE - 1;
            for (; used > 0; --used) {
                if ((Opcode)buf->data_buffer().value()->data_[used] not_eq
                    instruction::Fatal::op()) {
                    ++used;
                    break;
                }
            }

            const int remaining = SCRATCH_BUFFER_SIZE - used;
            if (remaining >= bytes_used) {
                done = true;

                // std::cout << "found another buffer with remaining space: "
                //           << remaining
                //           << ", copying " << bytes_used
                //           << " bytes to dest buffer offset "
                //           << used;

                auto src_buffer = fn->function().bytecode_impl_.databuffer();
                for (int i = 0; i < bytes_used; ++i) {
                    buf->data_buffer().value()->data_[used + i] =
                        src_buffer->data_buffer().value()->data_[i];
                }

                Protected used_bytes(make_integer(used));

                fn->function().bytecode_impl_.bytecode_ =
                    compr(make_cons(used_bytes, buf));
            }
        }
    });
}


} // namespace lisp
