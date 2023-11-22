////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#if defined(__GBA__) or defined(__NDS__)
// If defined, the system will use fixed pools, and will never call malloc.
#define UNHOSTED
#endif
#define UNHOSTED


#if defined(__GBA__) or defined(__NDS__)
#define USE_COMPRESSED_PTRS
#endif


#ifndef UNHOSTED
#define POOL_USE_HEAP
#endif


#include "containers/vector.hpp"
#include "function.hpp"
#include "module.hpp"
#include "number/numeric.hpp"
#include "platform/scratch_buffer.hpp"
#include "string.hpp"
#include "unicode.hpp"
#include "value.hpp"


class Platform;


namespace lisp {


// Call this function to initialize the interpreter, must be done at startup,
// prior to calling any of the library routines below.
void init();


struct Value;


struct ValueHeader {
    enum Type {
        // When a Value is deallocated, it is converted into a HeapNode, and
        // inserted into a freelist. Therefore, we need no extra space to
        // maintain the freelist.
        heap_node,

        nil,
        integer,
        cons,
        function,
        error,
        symbol,
        user_data,
        data_buffer,
        string,
        character,
        __reserved,
        count,
    };

    u8 type_ : 4;

    // For identifying which cells in the heap represent live objects.
    bool alive_ : 1;

    // Used by the garbage collector.
    bool mark_bit_ : 1;

    // NOTE:
    // I had two leftover bits in the value header, which I did not dedicate to
    // anything specific. Some datatypes use the spare bits for storing flags or
    // enumerated properties.
    u8 mode_bits_ : 2;

    Type type() const
    {
        return (Type)type_;
    }
};


struct Nil {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::nil;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


const char* intern(const char* string);


struct Symbol {
    ValueHeader hdr_;

    static constexpr const u32 buffer_size = 4;

    union Data {
        // NOTE: We want to pack data into a six byte space, but buffer size + 1
        // (5) bumps up the aligned size of the union to eight bytes, so we
        // store the pointer value as bytes and memcpy it. Really annoying that
        // I have to do this in the first place, as the data is always going to
        // be aligned in practice, and I don't want the union to be padded. But
        // it's out of my control.
        char intern_name_[sizeof(const char*)];
        // Small size optimized internal buffer. Symbols fewer than four bytes
        // will be stored within the symbol object itself.
        char small_name_[buffer_size + 1]; // +1 for null term
    } data_;


    const char* name() const
    {
        // NOTE: intern name is aliased to a small sized optimized array in the
        // same position. This returns a pointer to either the internd string or
        // the internal buffer.
        if (hdr_.mode_bits_ == (u8)Symbol::ModeBits::small) {
            return data_.small_name_;
        }
        return get_intern_name();
    }


    const char* get_intern_name() const
    {
        const char* intern_name;
        memcpy(&intern_name, data_.intern_name_, sizeof(const char*));
        return intern_name;
    }


    void set_intern_name(const char* value)
    {
        memcpy(data_.intern_name_, &value, sizeof(const char*));
    }


    void set_name(const char* name)
    {
        switch ((ModeBits)hdr_.mode_bits_) {
        case ModeBits::requires_intern:
            set_intern_name(intern(name));
            break;

        case ModeBits::stable_pointer:
            set_intern_name(name);
            break;

        case ModeBits::small:
            set_intern_name(0);
            memset(data_.small_name_, '\0', sizeof data_.small_name_);
            for (u32 i = 0; i < buffer_size; ++i) {
                if (*name not_eq '\0') {
                    data_.small_name_[i] = *(name++);
                }
            }
            break;
        }
    }


    const char* unique_id()
    {
        if (hdr_.mode_bits_ == (u8)ModeBits::small) {
            const char* result = 0;
            memcpy(&result, data_.small_name_, buffer_size);
            return result;
        } else {
            return get_intern_name();
        }
    }


    static ValueHeader::Type type()
    {
        return ValueHeader::Type::symbol;
    }

    enum class ModeBits {
        requires_intern,
        // If you create a symbol, while promising that the string pointer is
        // stable, the interpreter assumes that the string was previously
        // inserted into the string intern table. It will not perform the
        // slow lookup into the string intern memory region.
        stable_pointer,
        small,
    };

    static constexpr void finalizer(Value*)
    {
    }
};


struct Integer {
    ValueHeader hdr_;
    s32 value_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::integer;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct Character {
    ValueHeader hdr_;
    utf8::Codepoint cp_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::character;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct CompressedPtr {
#ifdef USE_COMPRESSED_PTRS
    u16 offset_;
#else
    void* ptr_;
#endif // USE_COMPRESSED_PTRS
};


CompressedPtr compr(Value* value);
Value* dcompr(CompressedPtr ptr);


struct Cons {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::cons;
    }

    inline Value* car()
    {
        return dcompr(car_);
    }

    inline Value* cdr()
    {
        return cdr_;
    }

    void set_car(Value* val)
    {
        car_ = compr(val);
    }

    void set_cdr(Value* val)
    {
        cdr_ = val;
    }

    static constexpr void finalizer(Value*)
    {
    }

private:
    // NOTE: we want all values in our interpreter to take up eight bytes or
    // less on a 32-bit system. We compress the car pointer, and use a full
    // 32-bit pointer for the cdr. If we need to compress one of the pointers,
    // we might as well optimize the cdr, as there are cases where you just want
    // to iterate to a certain point in a list and do not care what the car is.
    CompressedPtr car_;
    Value* cdr_;
};


struct Function {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::function;
    }

    using CPP_Impl = Value* (*)(int);

    struct Bytecode {
        CompressedPtr bytecode_; // (integeroffset . databuffer)
        CompressedPtr lexical_bindings_;

        Value* bytecode_offset() const;
        Value* databuffer() const;
    };

    struct LispFunction {
        CompressedPtr code_;
        CompressedPtr lexical_bindings_;
    };

    union {
        CPP_Impl cpp_impl_;
        LispFunction lisp_impl_;
        Bytecode bytecode_impl_;
    };

    enum ModeBits {
        cpp_function,
        lisp_function,
        lisp_bytecode_function,
    };

    static constexpr void finalizer(Value*)
    {
    }
};


struct DataBuffer {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::data_buffer;
    }

    alignas(ScratchBufferPtr) u8 sbr_mem_[sizeof(ScratchBufferPtr)];

    ScratchBufferPtr value()
    {
        return *reinterpret_cast<ScratchBufferPtr*>(sbr_mem_);
    }

    static void finalizer(Value* buffer);
};


struct String {

    ValueHeader hdr_;
    bool is_literal_;

    struct MemoryString {
        CompressedPtr data_buffer_;
        u16 offset_;
    };

    struct LiteralString {
        const char* value_;
    };


    union Data {
        MemoryString memory_;
        LiteralString literal_;
    } data_;


    static ValueHeader::Type type()
    {
        return ValueHeader::Type::string;
    }

    const char* value();

    static constexpr void finalizer(Value*)
    {
    }
};


struct Error {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::error;
    }

    enum class Code : u8 {
        value_not_callable,
        invalid_argc,
        symbol_table_exhausted,
        undefined_variable_access,
        invalid_argument_type,
        out_of_memory,
        set_in_expression_context,
        mismatched_parentheses,
        invalid_syntax,
    } code_;

    CompressedPtr context_;

    static const char* get_string(Code c)
    {
        switch (c) {
        case Code::value_not_callable:
            return "Value not callable";
        case Code::invalid_argc:
            return "Wrong number of arguments passed to function";
        case Code::symbol_table_exhausted:
            return "No more room in symbol table";
        case Code::undefined_variable_access:
            return "Access to undefined variable";
        case Code::invalid_argument_type:
            return "Invalid argument type";
        case Code::out_of_memory:
            return "Out of memory";
        case Code::set_in_expression_context:
            return "\'set\' in expr context";
        case Code::mismatched_parentheses:
            return "mismatched parentheses";
        case Code::invalid_syntax:
            return "invalid syntax";
        }
        return "Unknown error";
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct UserData {
    ValueHeader hdr_;
    void* obj_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::user_data;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct __Reserved {
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::__reserved;
    }

    // Reserved for future use

    static constexpr void finalizer(Value*)
    {
    }
};


struct HeapNode {
    ValueHeader hdr_;
    Value* next_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::heap_node;
    }

    static constexpr void finalizer(Value*)
    {
        // Should be unreachable.
        while (true)
            ;
    }
};


struct Value {
    ValueHeader hdr_;

    using Type = ValueHeader::Type;

    Type type() const
    {
        return hdr_.type();
    }

    HeapNode& heap_node()
    {
        return *reinterpret_cast<HeapNode*>(this);
    }

    Nil& nil()
    {
        return *reinterpret_cast<Nil*>(this);
    }

    Integer& integer()
    {
        return *reinterpret_cast<Integer*>(this);
    }

    Cons& cons()
    {
        return *reinterpret_cast<Cons*>(this);
    }

    Function& function()
    {
        return *reinterpret_cast<Function*>(this);
    }

    Error& error()
    {
        return *reinterpret_cast<Error*>(this);
    }

    Symbol& symbol()
    {
        return *reinterpret_cast<Symbol*>(this);
    }

    UserData& user_data()
    {
        return *reinterpret_cast<UserData*>(this);
    }

    DataBuffer& data_buffer()
    {
        return *reinterpret_cast<DataBuffer*>(this);
    }

    String& string()
    {
        return *reinterpret_cast<String*>(this);
    }

    Character& character()
    {
        return *reinterpret_cast<Character*>(this);
    }

    template <typename T> T& expect()
    {
        if (this->type() == T::type()) {
            return *reinterpret_cast<T*>(this);
        }

        while (true)
            ;
    }
};


Value* make_function(Function::CPP_Impl impl);
Value* make_cons(Value* car, Value* cdr);
Value* make_integer(s32 value);
Value* make_list(u32 length);
Value* make_error(Error::Code error_code, Value* context);
Value* make_userdata(void* obj);
Value* make_symbol(const char* name,
                   Symbol::ModeBits mode = Symbol::ModeBits::requires_intern);
Value* make_databuffer(const char* sbr_tag = "");
Value* make_string(const char* str);
Value* make_character(utf8::Codepoint cp);


Value* gensym();


// NOTE: Argument MUST be a C string literal.
Value* make_string_from_literal(const char* str);


Value* make_cons_safe(Value* car, Value* cdr);


#define L_CONS(CAR, CDR) lisp::make_cons_safe(CAR, CDR)

#define L_INT(VALUE) lisp::make_integer(VALUE)

#define L_SYM(NAME) lisp::make_symbol(NAME)

#define L_LOAD_INT(STACK_OFFSET) lisp::get_op(STACK_OFFSET)->integer().value_

#define L_LOAD_U8(STACK_OFFSET)                                                \
    ((u8)lisp::get_op(STACK_OFFSET)->integer().value_)

#define L_LOAD_STRING(STACK_OFFSET) lisp::get_op(STACK_OFFSET)->string().value()


struct Binding {
    const char* name_;
    Value* (*function_)(int argc);
};


// A convenience method for efficiently binding C functions to lisp global
// variables.
void bind_functions(const Binding* bindings, int count);


void get_interns(::Function<6 * sizeof(void*), void(const char*)> callback);
void get_env(::Function<6 * sizeof(void*), void(const char*)> callback);


Value* get_nil();
#define L_NIL lisp::get_nil()


void set_list(Value* list, u32 position, Value* value);
Value* get_list(Value* list, u32 position);
int length(Value* lat);

bool is_list(Value* maybe_lat); // slow


// For passing parameter to functions. Operands should be pushed in forward
// order, and read in REVERSE ORDER.
void push_op(Value* operand);
Value* get_op(u32 operand_number);
Value* get_op0();
Value* get_op1();
void pop_op();
Value* get_arg(u16 arg);


Value* get_this();
u8 get_argc();


void gc();


// Arguments should be pushed onto the operand stack prior to the function
// call. The interpreter will consume the arguments, leaving the result on top
// of the operand stack. i.e., use get_op(0) to read the result. Remember to
// call pop_op() when you are done with the result, otherwise, the result will
// remain on the operand stack, and possibly break the interpreter.
//
// You also need to indicate, in the argc parameter, the number of arguments
// that you pushed onto the operand stack.
void funcall(Value* fn, u8 argc);


Value* set_var(Value* sym, Value* value);
Value* get_var(Value* sym);


// Provided for convenience.
inline Value* set_var(const char* name, Value* value)
{
    auto var_sym = make_symbol(name);
    if (var_sym->type() not_eq Value::Type::symbol) {
        return var_sym;
    }

    return set_var(var_sym, value);
}


inline Value* get_var(const char* name)
{
    auto var_sym = make_symbol(name);
    if (var_sym->type() not_eq Value::Type::symbol) {
        return var_sym;
    }

    return get_var(var_sym);
}


class CharSequence {
public:
    virtual ~CharSequence()
    {
    }

    virtual char operator[](int index) = 0;
};


class BasicCharSequence : public CharSequence {
public:
    BasicCharSequence(const char* ptr) : ptr_(ptr), len_(str_len(ptr))
    {
    }

    char operator[](int index) override
    {
        if (index < 0 or index >= (int)len_) {
            return '\0';
        }
        return ptr_[index];
    }

private:
    const char* ptr_;
    const u32 len_;
};


class VectorCharSequence : public CharSequence {
public:
    VectorCharSequence(Vector<char>& v) : v_(v)
    {
    }

    char operator[](int index) override
    {
        if (index < 0 or index >= (int)v_.size()) {
            return '\0';
        }
        return v_[index];
    }

private:
    Vector<char>& v_;
};


// Read an S-expression, leaving the result at the top of the operand stack.
u32 read(CharSequence& code, int offset = 0);


// Result on operand stack.
void eval(Value* code);


// Parameter should be a function. Result on operand stack.
void compile(Value* code);


// Load code from a portable bytecode module. Result on operand stack.
void load_module(Module* module);


// Returns the result of the last expression in the string.
Value* dostring(CharSequence& code,
                ::Function<4 * sizeof(void*), void(Value&)> on_error);
Value* dostring(const char* code);


bool is_executing();


#define L_EXPECT_OP(OFFSET, TYPE)                                              \
    if (lisp::Value::Type::TYPE not_eq lisp::Value::Type::error and            \
        lisp::get_op((OFFSET))->type() == lisp::Value::Type::error) {          \
        return lisp::get_op((OFFSET));                                         \
    } else if (lisp::get_op((OFFSET))->type() not_eq                           \
               lisp::Value::Type::TYPE) {                                      \
        if (lisp::get_op((OFFSET)) == L_NIL) {                                 \
            return lisp::get_op((OFFSET));                                     \
        } else {                                                               \
            return lisp::make_error(lisp::Error::Code::invalid_argument_type,  \
                                    L_NIL);                                    \
        }                                                                      \
    }


#define L_EXPECT_ARGC(ARGC, EXPECTED)                                          \
    if (ARGC not_eq EXPECTED)                                                  \
        return lisp::make_error(lisp::Error::Code::invalid_argc, L_NIL);


class Printer {
public:
    virtual void put_str(const char* c) = 0;
    virtual ~Printer()
    {
    }
};


template <typename Container> class _Printer : public Printer {
public:
    void put_str(const char* str) override
    {
        while (*str not_eq '\0') {
            data_.push_back(*(str++));
        }
    }

    Container data_;
};


using DefaultPrinter = _Printer<StringBuffer<1024>>;


void format(Value* value, Printer& p);


// Protected objects will not be collected until the Protected wrapper goes out
// of scope.
class ProtectedBase {
public:
    ProtectedBase();

    ProtectedBase(const ProtectedBase&) = delete;

    ProtectedBase(ProtectedBase&&) = delete;

    virtual ~ProtectedBase();

    virtual void gc_mark() = 0;

    ProtectedBase* next() const
    {
        return next_;
    }

    ProtectedBase* prev() const
    {
        return prev_;
    }

protected:
    ProtectedBase* prev_;
    ProtectedBase* next_;
};


class Protected : public ProtectedBase {
public:
    Protected(Value* val) : val_(val)
    {
    }

    void gc_mark() override;

    Protected& operator=(Value* val)
    {
        val_ = val;
        return *this;
    }

    void set(Value* val)
    {
        val_ = val;
    }

    operator Value*()
    {
        return val_;
    }

    Value* get() const
    {
        return val_;
    }

    Value* operator->()
    {
        return val_;
    }

protected:
    Value* val_;
};


template <typename F> void foreach (Value* list, F && fn)
{
    Protected p(list);

    while (true) {

        if (list->type() not_eq Value::Type::cons) {
            break;
        } else {
            fn(list->cons().car());
        }

        list = list->cons().cdr();
    }
}


void safecall(Value* fn, u8 argc);


} // namespace lisp
