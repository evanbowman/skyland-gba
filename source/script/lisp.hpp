////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
#include "protected.hpp"
#include "string.hpp"
#include "unicode.hpp"
#include "value.hpp"


class Platform;


namespace lisp
{


// Call this function to initialize the interpreter, must be done at startup,
// prior to calling any of the library routines below.
void init(Optional<std::pair<const char*, u32>> external_symtab);


struct Value;


struct ValueHeader
{
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
        databuffer,
        string,
        fp,
        wrapped,
        __reserved_3,
        __reserved_2,
        __reserved_1,
        __reserved_0,
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


struct Nil
{
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::nil;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct Symbol
{
    ValueHeader hdr_;

    static constexpr const u32 buffer_size = 4;

    union Data
    {
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


    void set_name(const char* name);


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


struct Integer
{
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


struct Float
{
    ValueHeader hdr_;

    using ValueType = float;
    ValueType value_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::fp;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct CompressedPtr
{
#ifdef USE_COMPRESSED_PTRS
    u16 offset_;
#else
    void* ptr_;
#endif // USE_COMPRESSED_PTRS
};


CompressedPtr compr(Value* value);
Value* dcompr(CompressedPtr ptr);



// The purpose of a 'wrapped' object is to provide for the creation of custom
// value types. When printed, a wrapped object will use the attached decorator
// function or decorator symbol to render the wrapped instance.
struct Wrapped
{
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::wrapped;
    }

    static constexpr void finalizer(Value*)
    {
    }

    CompressedPtr data_;
    CompressedPtr type_sym_;
};



struct Cons
{
    ValueHeader hdr_;

    u8 is_definitely_list_ : 1;
    u8 unused_ : 7;

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
        is_definitely_list_ = false;
        __set_cdr(val);
    }

    void __set_cdr(Value* val)
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


struct Function
{
    ValueHeader hdr_;
    struct Signature
    {
        u8 required_args_ : 4;
        u8 arg0_type_ : 4;
        u8 arg1_type_ : 4;
        u8 arg2_type_ : 4;
        u8 arg3_type_ : 4;
        u8 ret_type_ : 4;

        void reset();

    } sig_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::function;
    }

    using CPP_Impl = Value* (*)(int);

    struct Bytecode
    {
        CompressedPtr bytecode_; // (integeroffset . databuffer)
        CompressedPtr lexical_bindings_;

        Value* bytecode_offset() const;
        Value* databuffer() const;
    };

    struct LispFunction
    {
        CompressedPtr code_;
        CompressedPtr lexical_bindings_;
    };

    union
    {
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


struct DataBuffer
{
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::databuffer;
    }

    alignas(ScratchBufferPtr) u8 sbr_mem_[sizeof(ScratchBufferPtr)];

    ScratchBufferPtr value()
    {
        return *reinterpret_cast<ScratchBufferPtr*>(sbr_mem_);
    }

    static void finalizer(Value* buffer);
};


struct String
{
    ValueHeader hdr_;

    enum ModeBits {
        memory_string,
        literal_string,
        small_string,
    };

    struct MemoryString
    {
        CompressedPtr databuffer_;
        u16 offset_;
    };

    struct LiteralString
    {
        const char* value_;
    };

    struct SmallString
    {
        char data_[4];
    };

    union Data
    {
        MemoryString memory_;
        LiteralString literal_;
        SmallString small_string_;
    } data_;

    ModeBits variant() const;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::string;
    }

    const char* value();

    static constexpr void finalizer(Value*)
    {
    }
};


struct Error
{
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
        custom,
    } code_;

    CompressedPtr context_;
    CompressedPtr stacktrace_;


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
        case Code::custom:
            return "";
        }
        return "Unknown error";
    }

    static constexpr void finalizer(Value*)
    {
    }
};


struct UserData
{
    ValueHeader hdr_;
    u16 tag_;
    void* obj_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::user_data;
    }

    static constexpr void finalizer(Value*)
    {
    }
};


template <ValueHeader::Type T> struct __Reserved
{
    ValueHeader hdr_;

    static ValueHeader::Type type()
    {
        return T;
    }

    // Reserved for future use

    static constexpr void finalizer(Value*)
    {
    }
};


struct HeapNode
{
    ValueHeader hdr_;
    Value* next_;

    static ValueHeader::Type type()
    {
        return ValueHeader::Type::heap_node;
    }

    static constexpr void finalizer(Value*)
    {
        // Should be unreachable.
    }
};


struct Value
{
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

    Float& fp()
    {
        return *reinterpret_cast<Float*>(this);
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

    DataBuffer& databuffer()
    {
        return *reinterpret_cast<DataBuffer*>(this);
    }

    String& string()
    {
        return *reinterpret_cast<String*>(this);
    }

    Wrapped& wrapped()
    {
        return *reinterpret_cast<Wrapped*>(this);
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


bool is_boolean_true(Value* val);


Value* wrap(Value* input, Value* type_sym);
Value* make_boolean(bool is_true);
Value* make_function(Function::CPP_Impl impl);
Value* make_cons(Value* car, Value* cdr);
Value* make_integer(s32 value);
Value* make_list(u32 length);
Value* make_error(Error::Code error_code, Value* context);
Value* make_error(const char* message);
Value* make_userdata(void* obj, u16 tag);
Value* make_symbol(const char* name,
                   Symbol::ModeBits mode = Symbol::ModeBits::requires_intern);
Value* make_databuffer(const char* sbr_tag = "");
Value* make_string(const char* str);
Value* make_float(Float::ValueType v);


void apropos(const char* match, Vector<const char*>& out);


Value* stacktrace();


Value* gensym();


// NOTE: Argument MUST be a C string literal.
Value* make_string_from_literal(const char* str);


Value* make_cons_safe(Value* car, Value* cdr);


#define L_CONS(CAR, CDR)                                                       \
    ([&] {                                                                     \
        lisp::Protected v1(static_cast<lisp::Value*>(CAR));                    \
        lisp::Protected v2(static_cast<lisp::Value*>(CDR));                    \
        return make_cons(v1, v2);                                              \
    }())

#define L_INT(VALUE) lisp::make_integer(VALUE)

#define L_FP(VALUE) lisp::make_float(VALUE)

#define L_SYM(NAME) lisp::make_symbol(NAME)

#define L_LOAD_INT(STACK_OFFSET) lisp::get_op(STACK_OFFSET)->integer().value_

#define L_LOAD_U8(STACK_OFFSET)                                                \
    ((u8)lisp::get_op(STACK_OFFSET)->integer().value_)

#define L_LOAD_STRING(STACK_OFFSET) lisp::get_op(STACK_OFFSET)->string().value()

#define L_LOAD_FP(STACK_OFFSET) lisp::get_op(STACK_OFFSET)->fp().value_


using SymbolCallback = ::Function<6 * sizeof(void*), void(const char*)>;


struct NativeInterface
{
    NativeInterface();

    using Function = lisp::Function::CPP_Impl;
    using RequiredArgs = int;

    using LookupResult = std::pair<lisp::Function::Signature, Function>;

    // Given a string function name, should return a C++ lisp function
    // implementation.
    LookupResult (*lookup_function_)(const char* function_name);

    // If a stable, internalized symbol exists in the interface, return
    // it. Allows the interpreter to avoid internalizing strings for builtin
    // functions provided by the interface.
    const char* (*resolve_intern_sym_)(const char* name);

    // Find a function's name, given its address.
    const char* (*lookup_name_)(Function fn);

    // Provides names of all registered functions.
    void (*get_symbols_)(SymbolCallback callback);
};


// Allows library users to register a set of lisp functions written as native
// C++ functions. Technically, you could call make_function() instead for each
// function that you want to register. But using a NativeInterface allows the
// interpreter to lazily pull functions into the environment as needed, reducing
// memory usage. NOTE: only one library of native functions may be bound at a
// time. But an application using this interpreter shouldn't need to bind
// multiple sets of native functions...
void register_native_interface(NativeInterface ni);


void get_interns(SymbolCallback callback);
void get_env(SymbolCallback callback);


// Number of variables in the toplevel environment, for logging purposes.
int toplevel_count();


using ValuePoolUsed = u32;
using ValuePoolFree = u32;
std::pair<ValuePoolUsed, ValuePoolFree> value_pool_info();


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


Value* set_var(Value* sym, Value* value, bool define_var);
Value* get_var(Value* sym);


// Provided for convenience.
inline Value* set_var(const char* name, Value* value)
{
    auto var_sym = make_symbol(name);
    if (var_sym->type() not_eq Value::Type::symbol) {
        return var_sym;
    }

    return set_var(var_sym, value, true);
}


inline Value* get_var(const char* name)
{
    auto var_sym = make_symbol(name);
    if (var_sym->type() not_eq Value::Type::symbol) {
        return var_sym;
    }

    return get_var(var_sym);
}


class CharSequence
{
public:
    virtual ~CharSequence()
    {
    }

    virtual char operator[](int index) = 0;
};


class BasicCharSequence : public CharSequence
{
public:
    BasicCharSequence(const char* ptr) : ptr_(ptr), len_(strlen(ptr))
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


class VectorCharSequence : public CharSequence
{
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

Value* lint_code(CharSequence& code);


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


class Printer
{
public:
    virtual void put_str(const char* c) = 0;
    virtual ~Printer()
    {
    }
};


template <typename Container> class _Printer : public Printer
{
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


template <int len> StringBuffer<len> val_to_string(Value* value)
{
    auto p = allocate_dynamic<DefaultPrinter>("...");
    format(value, *p);

    StringBuffer<len> out = p->data_.c_str();
    return out;
}


template <typename F> void l_foreach(Value* list, F&& fn)
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



#define SIG0(RET)                                                              \
    (lisp::Function::Signature{                                                \
        .required_args_ = 0,                                                   \
        .arg0_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg1_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg2_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg3_type_ = lisp::ValueHeader::Type::nil,                            \
        .ret_type_ = lisp::ValueHeader::Type::RET,                             \
    })

#define SIG1(RET, TP0)                                                         \
    (lisp::Function::Signature{                                                \
        .required_args_ = 1,                                                   \
        .arg0_type_ = lisp::ValueHeader::Type::TP0,                            \
        .arg1_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg2_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg3_type_ = lisp::ValueHeader::Type::nil,                            \
        .ret_type_ = lisp::ValueHeader::Type::RET,                             \
    })

#define SIG2(RET, TP0, TP1)                                                    \
    (lisp::Function::Signature{                                                \
        .required_args_ = 2,                                                   \
        .arg0_type_ = lisp::ValueHeader::Type::TP0,                            \
        .arg1_type_ = lisp::ValueHeader::Type::TP1,                            \
        .arg2_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg3_type_ = lisp::ValueHeader::Type::nil,                            \
        .ret_type_ = lisp::ValueHeader::Type::nil,                             \
    })

#define SIG3(RET, TP0, TP1, TP2)                                               \
    (lisp::Function::Signature{                                                \
        .required_args_ = 3,                                                   \
        .arg0_type_ = lisp::ValueHeader::Type::TP0,                            \
        .arg1_type_ = lisp::ValueHeader::Type::TP1,                            \
        .arg2_type_ = lisp::ValueHeader::Type::TP2,                            \
        .arg3_type_ = lisp::ValueHeader::Type::nil,                            \
        .ret_type_ = lisp::ValueHeader::Type::RET,                             \
    })

#define SIG4(RET, TP0, TP1, TP2, TP3)                                          \
    (lisp::Function::Signature{                                                \
        .required_args_ = 4,                                                   \
        .arg0_type_ = lisp::ValueHeader::Type::TP0,                            \
        .arg1_type_ = lisp::ValueHeader::Type::TP1,                            \
        .arg2_type_ = lisp::ValueHeader::Type::TP2,                            \
        .arg3_type_ = lisp::ValueHeader::Type::TP3,                            \
        .ret_type_ = lisp::ValueHeader::Type::RET,                             \
    })

#define EMPTY_SIG(ARGC)                                                        \
    (lisp::Function::Signature{                                                \
        .required_args_ = ARGC,                                                \
        .arg0_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg1_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg2_type_ = lisp::ValueHeader::Type::nil,                            \
        .arg3_type_ = lisp::ValueHeader::Type::nil,                            \
        .ret_type_ = lisp::ValueHeader::Type::nil,                             \
    })



} // namespace lisp
