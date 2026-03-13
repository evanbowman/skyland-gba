////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "vm.hpp"
#include "bytecode.hpp"
#include "lisp.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"
#include "number/endian.hpp"
#include "builtins.hpp"


namespace lisp
{


template <typename Instruction>
Instruction* read(ScratchBuffer& buffer, int& pc)
{
    auto result = (Instruction*)(buffer.data_ + pc);
    pc += sizeof(Instruction);
    return result;
}


bool vm_can_suspend(Value*& agitant)
{
    bool can_suspend = true;
    // NOTE: stacktrace includes frames up-to, but not including, the current
    // executing function.
    auto strace = lisp::stacktrace();
    l_foreach(strace, [&](Value* v) {
        if (v->type() == Value::Type::function) {
            if (v->hdr_.mode_bits_ not_eq Function::ModeBits::lisp_function) {
                agitant = v;
                can_suspend = false;
            }
        }
    });
    return can_suspend;
}


Optional<SuspendedExecutionContext> vm_execute(Value* code_buffer,
                                               int start_offset)
{
    return vm_resume(code_buffer,
                     start_offset,
                     {.program_counter_ = start_offset, .nested_scope_ = 0});
}


Optional<SuspendedExecutionContext>
vm_resume(Value* code_buffer, int start_offset, const ExecutionContext& ctx)
{
    int pc = ctx.program_counter_;

    auto& code = *code_buffer->databuffer().value();

    int nested_scope = ctx.nested_scope_;

    // If we are within a let expression, and we want to optimize out a
    // recursive tail call, we need to unwind all frames of the lexical scope,
    // because we will never return from the optimized out function call and hit
    // the LEXICAL_FRAME_POP instruction after the tailcall instruciton.
    auto unwind_lexical_scope = [&nested_scope] {
        while (nested_scope) {
            lexical_frame_pop();
            --nested_scope;
        }
    };

    using namespace instruction;

    Optional<ListBuilder> registers;

TOP:
    while (true) {

        switch ((Opcode)code.data_[pc]) {
        case LoadReg::op(): {
            if (not registers) {
                registers.emplace();
            }
            auto inst = read<LoadReg>(code, pc);
            auto v = get_list(registers->result(), inst->reg_);
            // info(::format("load % from reg %", v, inst->reg_));
            push_op(v);
            break;
        }

        case StoreReg::op(): {
            if (not registers) {
                registers.emplace();
            }
            auto inst = read<StoreReg>(code, pc);
            while (registers->length() <= inst->reg_) {
                registers->push_back(L_NIL);
            }
            u8 reg = inst->reg_;
            auto iter = registers->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            // info(::format("store % into reg %", get_op0(), inst->reg_));
            iter->cons().set_car(get_op0());
            pop_op();
            break;
        }

        case JumpIfFalse::op(): {
            auto inst = read<JumpIfFalse>(code, pc);
            if (not is_boolean_true(get_op0())) {
                pc = start_offset + inst->offset_.get();
            }
            pop_op();
            break;
        }

        case Jump::op(): {
            auto inst = read<Jump>(code, pc);
            pc = start_offset + inst->offset_.get();
            break;
        }

        case SmallJumpIfFalse::op(): {
            auto inst = read<SmallJumpIfFalse>(code, pc);
            if (not is_boolean_true(get_op0())) {
                pc = start_offset + inst->offset_;
            }
            pop_op();
            break;
        }

        case SmallJump::op(): {
            auto inst = read<SmallJump>(code, pc);
            pc = start_offset + inst->offset_;
            break;
        }


        case Set::op(): {
            read<Set>(code, pc);
            auto sym = get_op1();
            auto v = get_op0();
            pop_op();
            pop_op();
            if (sym->type() not_eq Value::Type::symbol) {
                push_op(make_error(::format<48>("non-symbol % passed to set!",
                                                sym).c_str()));
            } else {
                push_op(set_var(sym, v, false));
            }
            break;
        }


        case LoadSymtab::op(): {
            auto inst = read<LoadSymtab>(code, pc);
            auto index = inst->symtab_index_.get();
            push_op(make_symtab_symbol(index));
            break;
        }


        case Await::op(): {
            read<Await>(code, pc);
            if (get_op0()->type() not_eq lisp::Value::Type::promise) {
                pop_op();
                push_op(make_error("await expects a promise value!"));
            } else {
                Value* agitant = L_NIL;
                if (vm_can_suspend(agitant)) {
                    SuspendedExecutionContext suspend{
                        .program_counter_ = pc, .nested_scope_ = nested_scope};
                    return suspend;
                } else {
                    pop_op(); // the promise value
                    auto err_str = ::format("await failed: compiled caller % "
                                            "cannot call functions that await",
                                            agitant);
                    push_op(make_error(err_str.c_str()));
                }
            }
            break;
        }

        case SetVar::op(): {
            auto inst = read<SetVar>(code, pc);
            auto index = inst->symtab_index_.get();
            auto v = get_op0();
            Protected sym(make_symtab_symbol(index));
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case ConsVar::op(): {
            auto inst = read<SetVar>(code, pc);
            auto index = inst->symtab_index_.get();
            auto v = get_op0();
            Protected sym(make_symtab_symbol(index));
            Protected lat = get_var(sym);
            lat = make_cons(v, lat);
            auto result = set_var(sym, lat, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case SetVarRT::op(): {
            auto inst = read<SetVarRT>(code, pc);
            auto v = get_op0();
            Protected sym(make_symbol(inst->ptr_.get(),
                                      Symbol::ModeBits::stable_pointer));
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case SetVarSmall::op(): {
            auto inst = read<SetVarSmall>(code, pc);
            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }
            Protected sym(
                make_symbol(name.c_str(), Symbol::ModeBits::stable_pointer));
            auto v = get_op0();
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case LoadVarS::op(): {
            auto inst = read<LoadVarS>(code, pc);
            auto index = inst->symtab_index_.get();
            push_op(get_var(make_symtab_symbol(index)));
            break;
        }

        case LoadVarRT::op(): {
            auto inst = read<LoadVarRT>(code, pc);
            push_op(get_var_stable(inst->ptr_.get()));
            break;
        }

        case LoadVarSmall::op(): {
            auto inst = read<LoadVarSmall>(code, pc);
            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }
            push_op(get_var_stable(name.c_str()));
            break;
        }

        case Dup::op(): {
            read<Dup>(code, pc);
            push_op(get_op0());
            break;
        }

        case Not::op(): {
            read<Not>(code, pc);
            auto input = get_op0();
            pop_op();
            push_op(make_integer(not is_boolean_true(input)));
            break;
        }

        case PushNil::op():
            read<PushNil>(code, pc);
            push_op(get_nil());
            break;

        case PushFloat::op(): {
            auto inst = read<PushFloat>(code, pc);
            push_op(make_float(inst->f_.get()));
            break;
        }

        case PushRatio::op(): {
            auto inst = read<PushRatio>(code, pc);
            push_op(make_ratio(inst->num_.get(), inst->div_.get()));
            break;
        }

        case PushInteger::op(): {
            auto inst = read<PushInteger>(code, pc);
            push_op(make_integer(inst->value_.get()));
            break;
        }

        case Push0::op():
            read<Push0>(code, pc);
            push_op(make_integer(0));
            break;

        case Push1::op():
            read<Push1>(code, pc);
            push_op(make_integer(1));
            break;

        case Push2::op():
            read<Push2>(code, pc);
            push_op(make_integer(2));
            break;

        case PushSmallInteger::op(): {
            auto inst = read<PushSmallInteger>(code, pc);
            push_op(make_integer(inst->value_));
            break;
        }

        case PushSmallSymbol::op(): {
            auto inst = read<PushSmallSymbol>(code, pc);
            StringBuffer<3> str;
            auto name = inst->name_;
            for (int i = 0; i < 3; ++i) {
                str.__push_unsafe(name[i]);
            }
            push_op(make_symbol(str.c_str(), Symbol::ModeBits::small));
            break;
        }

        case PushSymbolRT::op(): {
            auto inst = read<PushSymbolRT>(code, pc);
            push_op(make_symbol(inst->ptr_.get(),
                                Symbol::ModeBits::stable_pointer));
            break;
        }

        case PushString::op(): {
            auto inst = read<PushString>(code, pc);
            push_op(make_string(code.data_ + pc));
            pc += inst->length_;
            break;
        }

        case TailCall::op(): {

            Protected fn(get_op0());

            auto argc = read<TailCall>(code, pc)->argc_;


            if (fn == get_this()) {
                pop_op(); // function on stack

                if (get_argc() not_eq argc) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    // Actually...
                    // The isn't really anything preventing a variadic function
                    // from being executed recursively with a different number
                    // of args, right? So maybe shouldn't be isn't an error...
                    while (true)
                        ;
                }

                if (argc == 0) {
                    unwind_lexical_scope();
                    pc = start_offset;
                    registers.reset();
                    goto TOP;
                } else {
                    // TODO: perform TCO for N-arg function
                    funcall(fn, argc);
                }

            } else {

                pop_op();
                funcall(fn, argc);
            }

            break;
        }

        case TailCall1::op(): {
            read<TailCall1>(code, pc);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg = get_op1();

                if (get_argc() not_eq 1) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // argument
                pop_op(); // previous arg

                push_op(arg);

                unwind_lexical_scope();
                pc = start_offset;
                registers.reset();
                goto TOP;

            } else {
                pop_op();
                funcall(fn, 1);
            }
            break;
        }

        case TailCall2::op(): {
            read<TailCall2>(code, pc);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg0 = get_op1();
                auto arg1 = get_op(2);

                if (get_argc() not_eq 2) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                pc = start_offset;
                registers.reset();
                goto TOP;

            } else {
                pop_op();
                funcall(fn, 2);
            }
            break;
        }

        case TailCall3::op(): {
            read<TailCall3>(code, pc);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg0 = get_op1();
                auto arg1 = get_op(2);
                auto arg2 = get_op(3);

                if (get_argc() not_eq 3) {
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg2);
                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                pc = start_offset;
                registers.reset();
                goto TOP;

            } else {
                pop_op();
                funcall(fn, 3);
            }
            break;
        }

        case LoadCall0::op(): {
            auto inst = read<LoadCall0>(code, pc);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            funcall(fn, 0);
            break;
        }

        case LoadCall1::op(): {
            auto inst = read<LoadCall1>(code, pc);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            funcall(fn, 1);
            break;
        }

        case LoadCall2::op(): {
            auto inst = read<LoadCall2>(code, pc);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            funcall(fn, 2);
            break;
        }

        case LoadCall3::op(): {
            auto inst = read<LoadCall3>(code, pc);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            funcall(fn, 3);
            break;
        }

        case Funcall::op(): {
            Protected fn(get_op0());
            auto argc = read<Funcall>(code, pc)->argc_;
            pop_op();
            funcall(fn, argc);
            break;
        }

        case Funcall1::op(): {
            read<Funcall1>(code, pc);
            Protected fn(get_op0());
            pop_op();
            funcall(fn, 1);
            break;
        }

        case Funcall2::op(): {
            read<Funcall2>(code, pc);
            Protected fn(get_op0());
            pop_op();
            funcall(fn, 2);
            break;
        }

        case Funcall3::op(): {
            read<Funcall3>(code, pc);
            Protected fn(get_op0());
            pop_op();
            funcall(fn, 3);
            break;
        }

        case Arg::op(): {
            read<Arg>(code, pc);
            auto arg_num = get_op0();
            auto arg = get_arg(arg_num->integer().value_);
            pop_op();
            push_op(arg);
            break;
        }

        case Arg0::op(): {
            read<Arg0>(code, pc);
            push_op(get_arg(0));
            break;
        }

        case Arg1::op(): {
            read<Arg1>(code, pc);
            push_op(get_arg(1));
            break;
        }

        case Arg2::op(): {
            read<Arg2>(code, pc);
            push_op(get_arg(2));
            break;
        }

        case MakePair::op(): {
            read<MakePair>(code, pc);
            auto car = get_op1();
            auto cdr = get_op0();
            auto cons = make_cons(car, cdr);
            pop_op();
            pop_op();
            push_op(cons);
            break;
        }

        case First::op(): {
            read<First>(code, pc);
            auto arg = get_op0();
            pop_op();
            if (arg->type() == Value::Type::cons) {
                push_op(arg->cons().car());
            } else {
                push_op(make_error(Error::Code::invalid_argument_type, L_NIL));
            }
            break;
        }

        case Rest::op(): {
            read<Rest>(code, pc);
            auto arg = get_op0();
            pop_op();
            if (arg->type() == Value::Type::cons) {
                push_op(arg->cons().cdr());
            } else {
                push_op(make_error(Error::Code::invalid_argument_type, L_NIL));
            }
            break;
        }

        case Pop::op():
            read<Pop>(code, pc);
            pop_op();
            break;

        case EarlyRet::op():
        case Ret::op():
            unwind_lexical_scope();
            return nullopt();

        case PushLambda::op(): {
            auto inst = read<PushLambda>(code, pc);
            auto offset = make_integer(pc);
            if (offset->type() == lisp::Value::Type::integer) {
                auto bytecode = make_cons(offset, code_buffer);
                if (bytecode->type() == lisp::Value::Type::cons) {
                    auto fn = make_bytecode_function(bytecode);
                    push_op(fn);
                } else {
                    push_op(bytecode);
                }
            } else {
                push_op(offset);
            }
            pc = start_offset + inst->lambda_end_.get();
            break;
        }

        case PushList::op(): {
            auto list_size = read<PushList>(code, pc)->element_count_;
            Protected lat(make_list(list_size));
            for (int i = 0; i < list_size; ++i) {
                set_list(lat, i, get_op((list_size - 1) - i));
            }
            for (int i = 0; i < list_size; ++i) {
                pop_op();
            }
            push_op(lat);
            break;
        }

        case PushThis::op(): {
            push_op(get_this());
            read<PushThis>(code, pc);
            break;
        }

        case LexicalDef::op(): {
            auto inst = read<LexicalDef>(code, pc);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalDefRT::op(): {
            auto inst = read<LexicalDefRT>(code, pc);
            Protected sym(make_symbol(inst->ptr_.get(),
                                      Symbol::ModeBits::stable_pointer));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalDefSmall::op(): {
            auto inst = read<LexicalDefSmall>(code, pc);

            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }

            Protected sym(make_symbol(name.c_str(), Symbol::ModeBits::small));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalFramePush::op(): {
            read<LexicalFramePush>(code, pc);
            lexical_frame_push();
            ++nested_scope;
            break;
        }

        case LexicalFramePop::op(): {
            read<LexicalFramePop>(code, pc);
            lexical_frame_pop();
            --nested_scope;
            break;
        }

        case Add::op(): {
            auto add = read<Add>(code, pc);
            auto result = builtin_add(add->operands_);
            for (int i = 0; i < add->operands_; ++i) {
                pop_op();
            }
            push_op(result);
            break;
        }

        case Get::op(): {
            read<Get>(code, pc);
            auto result = builtin_get(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case IsEqual::op(): {
            read<IsEqual>(code, pc);
            auto result = builtin_comp_equal(2);
            pop_op();
            pop_op(); // args
            push_op(result);
            break;
        }

        default:
        case Fatal::op():
            while (true)
                ;
            break;
        }
    }

    return nullopt();
}


} // namespace lisp
