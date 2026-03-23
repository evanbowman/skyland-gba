////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "bytecode.hpp"
#include "lisp.hpp"
#include "lisp_internal.hpp"
#include "localization.hpp"
#include "string.hpp"


namespace lisp::instruction
{


void disassemble(Value* fn,
                 ::Function<2 * sizeof(void*), void(const char*)> callback)
{
    if (fn->hdr_.mode_bits_ not_eq Function::ModeBits::lisp_bytecode_function) {
        return;
    }

    auto buffer = fn->function().bytecode_impl_.databuffer();
    auto data = buffer->databuffer().value();

    auto start_offset =
        fn->function().bytecode_impl_.bytecode_offset()->integer().value_;

    disassemble(&*data, start_offset, callback);
}



void disassemble(ScratchBuffer* data,
                 s32 offset,
                 ::Function<2 * sizeof(void*), void(const char*)> callback)
{
    u8 depth = 0;

    Buffer<s32, 16> start_offsets;

    start_offsets.push_back(offset);

    auto start_offset = [&] { return start_offsets.back(); };

    for (int i = start_offset(); i < SCRATCH_BUFFER_SIZE;) {

        StringBuffer<64> out;

        for (int i = 0; i < depth; ++i) {
            out += "| ";
        }

        const auto offset = to_string<10>(i - start_offset());
        if (offset.length() < 3) {
            for (u32 i = 0; i < 3 - offset.length(); ++i) {
                out.push_back('0');
            }
        }

        out += offset;
        out += ":";

        using namespace instruction;

        switch ((Opcode)(*data).data_[i]) {
        case Fatal::op():
            return;

        case Get::op():
            out += "GET";
            i += sizeof(Get);
            break;

        case Set::op():
            out += "SET";
            i += sizeof(Set);
            break;

        case Add::op():
            out += Add::name();
            out += "(";
            out += stringify(((Add*)(data->data_ + i))->operands_);
            out += ")";
            i += sizeof(Add);
            break;

        case Multiply::op():
            out += Multiply::name();
            out += "(";
            out += stringify(((Multiply*)(data->data_ + i))->operands_);
            out += ")";
            i += sizeof(Multiply);
            break;

        case Subtract::op():
            out += Subtract::name();
            i += sizeof(Subtract);
            break;

        case Divide::op():
            out += Divide::name();
            i += sizeof(Divide);
            break;

        case Incr::op():
            out += Incr::name();
            i += sizeof(Incr);
            break;

        case Decr::op():
            out += Decr::name();
            i += sizeof(Decr);
            break;

        case LoadVarRT::op():
            out += LoadVarRT::name();
            out += "(";
            out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
            out += ")";
            i += sizeof(LoadVarRT);
            break;

        case SetVarRT::op():
            out += SetVarRT::name();
            out += "(";
            out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
            out += ")";
            i += sizeof(SetVarRT);
            break;

        case LoadVarSmall::op(): {
            i += 1;
            out += "LOAD_VAR_SMALL(";
            StringBuffer<3> name;
            for (int j = 0; j < 3; ++j) {
                name.push_back(*(data->data_ + i + j));
            }
            out += name.c_str();
            out += ")";
            i += 3;
            break;
        }

        case SetVarSmall::op(): {
            i += 1;
            out += "SET_VAR_SMALL(";
            StringBuffer<3> name;
            for (int j = 0; j < 3; ++j) {
                name.push_back(*(data->data_ + i + j));
            }
            out += name.c_str();
            out += ")";
            i += 3;
            break;
        }

        case PushSmallSymbol::op(): {
            i += 1;
            out += "PUSH_SMALL_SYMBOL(";
            StringBuffer<3> name;
            for (int j = 0; j < 3; ++j) {
                name.push_back(*(data->data_ + i + j));
            }
            out += name.c_str();
            out += ")";
            i += 3;
            break;
        }

        case LoadReg0::op(): {
            out += LoadReg0::name();
            i += sizeof(LoadReg0);
            break;
        }

        case LoadReg1::op(): {
            out += LoadReg1::name();
            i += sizeof(LoadReg1);
            break;
        }

        case LoadReg2::op(): {
            out += LoadReg2::name();
            i += sizeof(LoadReg2);
            break;
        }

        case StoreReg0::op(): {
            out += StoreReg0::name();
            i += sizeof(StoreReg0);
            break;
        }

        case StoreReg1::op(): {
            out += StoreReg1::name();
            i += sizeof(StoreReg1);
            break;
        }

        case StoreReg2::op(): {
            out += StoreReg2::name();
            i += sizeof(StoreReg2);
            break;
        }

        case StoreReg0Keep::op(): {
            out += StoreReg0Keep::name();
            i += sizeof(StoreReg0Keep);
            break;
        }

        case StoreReg1Keep::op(): {
            out += StoreReg1Keep::name();
            i += sizeof(StoreReg1Keep);
            break;
        }

        case StoreReg2Keep::op(): {
            out += StoreReg2Keep::name();
            i += sizeof(StoreReg2Keep);
            break;
        }

        case CmpLess::op(): {
            out += CmpLess::name();
            i += sizeof(CmpLess);
            break;
        }

        case CmpGreater::op(): {
            out += CmpGreater::name();
            i += sizeof(CmpGreater);
            break;
        }

        case LoadReg::op(): {
            out += LoadReg::name();
            out += "(";
            out += stringify(((LoadReg*)(data->data_ + i))->reg_);
            out += ")";
            i += sizeof(LoadReg);
            break;
        }

        case StoreRegKeep::op(): {
            out += StoreRegKeep::name();
            out += "(";
            out += stringify(((StoreRegKeep*)(data->data_ + i))->reg_);
            out += ")";
            i += sizeof(StoreRegKeep);
            break;
        }

        case StoreReg::op(): {
            out += StoreReg::name();
            out += "(";
            out += stringify(((StoreReg*)(data->data_ + i))->reg_);
            out += ")";
            i += sizeof(StoreReg);
            break;
        }

        case LexicalDef::op(): {
            out += LexicalDef::name();
            out += "(";
            auto off = ((LexicalDef*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LexicalDef);
            break;
        }

        case LoadSymtab::op(): {
            out += LoadSymtab::name();
            out += "(";
            auto off = ((LoadSymtab*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadSymtab);
            break;
        }

        case SetVar::op(): {
            out += SetVar::name();
            out += "(";
            auto off = ((SetVar*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(SetVar);
            break;
        }

        case ConsVar::op(): {
            out += ConsVar::name();
            out += "(";
            auto off = ((ConsVar*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(ConsVar);
            break;
        }

        case LoadVarS::op(): {
            out += LoadVarS::name();
            out += "(";
            auto off = ((LoadVarS*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadVarS);
            break;
        }

        case LoadTCall1::op(): {
            out += LoadTCall1::name();
            out += "(";
            auto off = ((LoadTCall1*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadTCall1);
            break;
        }

        case LoadTCall2::op(): {
            out += LoadTCall2::name();
            out += "(";
            auto off = ((LoadTCall2*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadTCall2);
            break;
        }

        case LoadTCall3::op(): {
            out += LoadTCall3::name();
            out += "(";
            auto off = ((LoadTCall3*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadTCall3);
            break;
        }

        case LoadCall0Discard::op(): {
            auto inst = (LoadCall0Discard*)(data->data_ + i);
            auto off = inst->symtab_index_.get() * symtab_stride;
            out += inst->name();
            out += "(";
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(*inst);
            break;
        }

        case LoadCall1Discard::op(): {
            auto inst = (LoadCall1Discard*)(data->data_ + i);
            auto off = inst->symtab_index_.get() * symtab_stride;
            out += inst->name();
            out += "(";
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(*inst);
            break;
        }

        case LoadCall2Discard::op(): {
            auto inst = (LoadCall2Discard*)(data->data_ + i);
            auto off = inst->symtab_index_.get() * symtab_stride;
            out += inst->name();
            out += "(";
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(*inst);
            break;
        }

        case LoadCall3Discard::op(): {
            auto inst = (LoadCall3Discard*)(data->data_ + i);
            auto off = inst->symtab_index_.get() * symtab_stride;
            out += inst->name();
            out += "(";
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(*inst);
            break;
        }

        case LoadCall0::op(): {
            out += LoadCall0::name();
            out += "(";
            auto off = ((LoadCall0*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadCall0);
            break;
        }

        case LoadCall1::op(): {
            out += LoadCall1::name();
            out += "(";
            auto off = ((LoadCall1*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadCall1);
            break;
        }

        case LoadCall2::op(): {
            out += LoadCall2::name();
            out += "(";
            auto off = ((LoadCall2*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadCall2);
            break;
        }

        case LoadCall3::op(): {
            out += LoadCall3::name();
            out += "(";
            auto off = ((LoadCall3*)(data->data_ + i))->symtab_index_.get() *
                       symtab_stride;
            out += load_from_symtab(off);
            out += ")";
            i += sizeof(LoadCall3);
            break;
        }

        case PushSymbolRT::op():
            out += "PUSH_SYMBOL_RT(";
            out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
            out += ")";
            i += sizeof(PushSymbolRT);
            break;

        case PushString::op(): {
            i += 1;
            out += PushString::name();
            out += "(\"";
            u8 len = *(data->data_ + (i++));
            out += data->data_ + i;
            out += "\")";
            i += len;
            break;
        }

        case PushNil::op():
            out += "PUSH_NIL";
            i += 1;
            break;

        case Push0::op():
            i += 1;
            out += "PUSH_0";
            break;

        case Push1::op():
            i += 1;
            out += "PUSH_1";
            break;

        case Push2::op():
            i += 1;
            out += "PUSH_2";
            break;

        case PushRatio::op():
            i += 1;
            out += "PUSH_RATIO(";
            out += to_string<32>(((HostInteger<s32>*)(data->data_ + i))->get());
            out += "/";
            out += to_string<32>(
                ((HostInteger<s32>*)(data->data_ + i + 4))->get());
            out += ")";
            i += 8;
            break;

        case PushInteger::op():
            i += 1;
            out += "PUSH_INTEGER(";
            out += to_string<32>(((HostInteger<s32>*)(data->data_ + i))->get());
            out += ")";
            i += 4;
            break;

        case PushSmallInteger::op():
            out += "PUSH_SMALL_INTEGER(";
            out += to_string<32>(*(data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case PushFloat::op(): {
            out += "PUSH_FLOAT(";
            char buffer[32];
            auto f = ((PackedFloat*)(data->data_ + i + 1))->get();
            float_to_string(f, 32, buffer);
            out += buffer;
            out += ")";
            i += sizeof(PushFloat);
            break;
        }

        case JumpIfFalse::op():
            out += "JUMP_IF_FALSE(";
            out += to_string<32>(
                ((HostInteger<u16>*)(data->data_ + i + 1))->get());
            out += ")";
            i += 3;
            break;

        case SmallJumpNotEqual::op(): {
            auto inst = (SmallJumpNotEqual*)(data->data_ + i);
            out += inst->name();
            out += "(";
            out += stringify(inst->offset_);
            out += ")";
            i += sizeof *inst;
            break;
        }

        case RetNilIfTrue::op():
            out += RetNilIfTrue::name();
            i += sizeof(RetNilIfTrue);
            break;

        case RetNilIfFalse::op():
            out += RetNilIfFalse::name();
            i += sizeof(RetNilIfFalse);
            break;

        case RetNilIfFalseKeep::op():
            out += RetNilIfFalseKeep::name();
            i += sizeof(RetNilIfFalseKeep);
            break;

        case Jump::op():
            out += "JUMP(";
            out += to_string<32>(
                ((HostInteger<u16>*)(data->data_ + i + 1))->get());
            out += ")";
            i += 3;
            break;

        case SmallJumpIfFalse::op():
            out += "SMALL_JUMP_IF_FALSE(";
            out += to_string<32>((u8) * (data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case SmallJumpIfTrue::op():
            out += "SMALL_JUMP_IF_TRUE(";
            out += to_string<32>((u8) * (data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case SmallJump::op():
            out += "SMALL_JUMP(";
            out += to_string<32>((u8) * (data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case PushLambda::op(): {
            out += "PUSH_LAMBDA(";
            out += to_string<32>(
                ((HostInteger<u16>*)(data->data_ + i + 1))->get());
            out += ")";
            i += 3;
            start_offsets.push_back(i);
            ++depth;
            break;
        }

        case PushThis::op():
            out += PushThis::name();
            i += sizeof(PushThis);
            break;

        case Arg::op():
            out += Arg::name();
            i += sizeof(Arg);
            break;

        case Arg0::op():
            out += Arg0::name();
            i += sizeof(Arg0);
            break;

        case Arg1::op():
            out += Arg1::name();
            i += sizeof(Arg1);
            break;

        case Arg2::op():
            out += Arg2::name();
            i += sizeof(Arg2);
            break;

        case TailCall::op():
            out += TailCall::name();
            out += "(";
            out += to_string<32>(*(data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case TailCall1::op():
            out += TailCall1::name();
            ++i;
            break;

        case TailCall2::op():
            out += TailCall2::name();
            ++i;
            break;

        case TailCall3::op():
            out += TailCall3::name();
            ++i;
            break;

        case Funcall::op():
            out += "FUNCALL(";
            out += to_string<32>(*(data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case PushList::op():
            out += "PUSH_LIST(";
            out += to_string<32>(*(data->data_ + i + 1));
            out += ")";
            i += 2;
            break;

        case Funcall1::op():
            out += "FUNCALL_1";
            i += 1;
            break;

        case Funcall2::op():
            out += "FUNCALL_2";
            i += 1;
            break;

        case Funcall3::op():
            out += "FUNCALL_3";
            i += 1;
            break;

        case Pop::op():
            out += "POP";
            i += 1;
            break;

        case MakePair::op():
            out += "MAKE_PAIR";
            i += 1;
            break;

        case Not::op():
            out += Not::name();
            i += sizeof(Not);
            break;

        case First::op():
            out += First::name();
            i += sizeof(First);
            break;

        case Rest::op():
            out += Rest::name();
            i += sizeof(Rest);
            break;

        case Length::op():
            out += Length::name();
            i += sizeof(Length);
            break;

        case Dup::op():
            out += Dup::name();
            i += 1;
            break;

        case EarlyRet::op():
            out += EarlyRet::name();
            i += sizeof(EarlyRet);
            break;

        case EarlyRetNil::op():
            out += EarlyRetNil::name();
            i += sizeof(EarlyRetNil);
            break;

        case LexicalDefRT::op():
            out += LexicalDefRT::name();
            out += "(";
            out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
            out += ")";
            i += sizeof(LexicalDefRT);
            break;

        case LexicalDefSmall::op(): {
            out += LexicalDefSmall::name();
            out += "(";
            i += 1;
            StringBuffer<3> name;
            for (int j = 0; j < 3; ++j) {
                name.push_back(*(data->data_ + i + j));
            }
            out += name.c_str();
            out += ")";
            i += 3;
            break;
        }

        case LexicalFramePush::op():
            out += LexicalFramePush::name();
            i += sizeof(LexicalFramePush);
            break;

        case LexicalFramePop::op():
            out += LexicalFramePop::name();
            i += sizeof(LexicalFramePop);
            break;

        case Await::op(): {
            out += Await::name();
            i += sizeof(Await);
            break;
        }

        case Resume::op(): {
            out += Resume::name();
            i += sizeof(Resume);
            break;
        }

        case IsEqual::op(): {
            out += IsEqual::name();
            i += sizeof(IsEqual);
            break;
        }

        case RetNil::op():
        case Ret::op(): {
            if (depth == 0) {
                if ((Opcode)(*data).data_[i] == Ret::op()) {
                    out += Ret::name();
                } else {
                    out += RetNil::name();
                }
                callback(out.c_str());
                return;
            } else {
                --depth;
                if ((Opcode)(*data).data_[i] == Ret::op()) {
                    out += Ret::name();
                } else {
                    out += RetNil::name();
                }
                i += 1;
            }
            start_offsets.pop_back();
            break;
        }

        default:
            return;
        }
        callback(out.c_str());
    }
}


void parse_instructions(ScratchBuffer& buffer,
                        InstructionList& list,
                        int offset)
{
    using namespace instruction;

    int depth = 0;
    while (true) {
        auto* inst = (instruction::Header*)(buffer.data_ + offset);
        list.push_back(inst);
        switch (inst->op_) {
        case Fatal::op():
            return;
        case PushString::op():
            offset += sizeof(PushString) + ((PushString*)inst)->length_;
            break;

        case RetNil::op():
        case Ret::op():
            if (depth == 0) {
                return;
            }
            --depth;
            static_assert(sizeof(Ret) == sizeof(RetNil));
            offset += sizeof(Ret);
            break;

        case PushLambda::op():
            ++depth;
            offset += sizeof(PushLambda);
            break;

        default:
            offset += instruction_size(*inst);
            break;
        }
    }
}


u32 instruction_size(Header header)
{
    switch (header.op_) {
#define MATCH(NAME)                                                            \
    case NAME::op():                                                           \
        return sizeof(NAME);                                                   \
        break;
        MATCH(PushString)
        MATCH(RetNil)
        MATCH(Ret)
        MATCH(EarlyRet)
        MATCH(PushLambda)
        MATCH(LoadVarRT)
        MATCH(LoadVarS)
        MATCH(PushSymbolRT)
        MATCH(LoadSymtab)
        MATCH(PushNil)
        MATCH(Push0)
        MATCH(Push1)
        MATCH(Push2)
        MATCH(PushInteger)
        MATCH(PushSmallInteger)
        MATCH(JumpIfFalse)
        MATCH(Jump)
        MATCH(SmallJumpIfFalse)
        MATCH(SmallJumpIfTrue)
        MATCH(SmallJump)
        MATCH(TailCall)
        MATCH(TailCall1)
        MATCH(TailCall2)
        MATCH(TailCall3)
        MATCH(Funcall)
        MATCH(Funcall1)
        MATCH(Funcall2)
        MATCH(Funcall3)
        MATCH(PushList)
        MATCH(Pop)
        MATCH(Dup)
        MATCH(MakePair)
        MATCH(First)
        MATCH(Rest)
        MATCH(Arg)
        MATCH(Arg0)
        MATCH(Arg1)
        MATCH(Arg2)
        MATCH(PushThis)
        MATCH(Not)
        MATCH(LexicalDef)
        MATCH(LexicalDefRT)
        MATCH(LexicalFramePush)
        MATCH(LexicalFramePop)
        MATCH(PushSmallSymbol)
        MATCH(LexicalDefSmall)
        MATCH(LoadVarSmall)
        MATCH(Set)
        MATCH(PushFloat)
        MATCH(PushRatio)
        MATCH(Await)
        MATCH(IsEqual)
        MATCH(CmpLess)
        MATCH(CmpGreater)
        MATCH(LoadCall0)
        MATCH(LoadCall1)
        MATCH(LoadCall2)
        MATCH(LoadCall3)
        MATCH(LoadCall0Discard)
        MATCH(LoadCall1Discard)
        MATCH(LoadCall2Discard)
        MATCH(LoadCall3Discard)
        MATCH(LoadTCall1)
        MATCH(LoadTCall2)
        MATCH(LoadTCall3)
        MATCH(SetVarSmall)
        MATCH(SetVar)
        MATCH(SetVarRT)
        MATCH(LoadReg)
        MATCH(StoreReg)
        MATCH(StoreRegKeep)
        MATCH(ConsVar)
        MATCH(Get)
        MATCH(Add)
        MATCH(Subtract)
        MATCH(Multiply)
        MATCH(Divide)
        MATCH(Incr)
        MATCH(Decr)
        MATCH(Resume)
        MATCH(LoadReg0)
        MATCH(LoadReg1)
        MATCH(LoadReg2)
        MATCH(StoreReg0)
        MATCH(StoreReg1)
        MATCH(StoreReg2)
        MATCH(StoreReg0Keep)
        MATCH(StoreReg1Keep)
        MATCH(StoreReg2Keep)
        MATCH(EarlyRetNil)
        MATCH(RetNilIfTrue)
        MATCH(RetNilIfFalse)
        MATCH(RetNilIfFalseKeep)
        MATCH(SmallJumpNotEqual)
        MATCH(Length)
    }
    return 0;
}


} // namespace lisp::instruction
