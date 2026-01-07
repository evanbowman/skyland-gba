////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "lisp.hpp"
#include "allocator.hpp"
#include "bytecode.hpp"
#include "eternal/eternal.hpp"
#include "ext_workram_data.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"
#include "localization.hpp"
#include "memory/buffer.hpp"
#include "memory/pool.hpp"
#include "number/random.hpp"
#include "platform/libc.hpp"
#include "rot13.hpp"

#if not MAPBOX_ETERNAL_IS_CONSTEXPR
#error "NON-Constexpr lookup table!"
#endif


namespace lisp
{


static int run_gc();


static const u32 string_intern_table_size = 2000;


#if defined(__NDS__)
#define VALUE_POOL_SIZE 20000
#elif defined(__GBA__)
#define VALUE_POOL_SIZE 10000
#else
#define VALUE_POOL_SIZE 200000
#endif

union ValueMemory
{
    Value value_;
    HeapNode heap_node_;
    Nil nil_;
    Integer integer_;
    Cons cons_;
    Function function_;
    Error error_;
    Symbol symbol_;
    UserData user_data_;
    DataBuffer databuffer_;
    String string_;
    Wrapped wrapped_;
    __Reserved<Value::Type::__reserved_3> __reserved_3;
    __Reserved<Value::Type::__reserved_2> __reserved_2;
    __Reserved<Value::Type::__reserved_1> __reserved_1;
    __Reserved<Value::Type::__reserved_0> __reserved_0;
};


#if defined(__GBA__) or defined(__NDS__)
static_assert(sizeof(ValueMemory) == 8);
#endif


static EXT_WORKRAM_DATA int value_remaining_count;
static EXT_WORKRAM_DATA int early_gc_threshold = value_remaining_count;
static EXT_WORKRAM_DATA ValueMemory value_pool_data[VALUE_POOL_SIZE];
static Value* value_pool = nullptr;


struct StringInternTable
{
    char data_[string_intern_table_size];
};


const char* intern(const char* string);


void Function::Signature::reset()
{
    required_args_ = 0;
    arg0_type_ = ValueHeader::Type::nil;
    arg1_type_ = ValueHeader::Type::nil;
    arg2_type_ = ValueHeader::Type::nil;
    arg3_type_ = ValueHeader::Type::nil;
    ret_type_ = ValueHeader::Type::nil;
}


std::pair<ValuePoolUsed, ValuePoolFree> value_pool_info()
{
    int values_remaining = 0;
    Value* current = value_pool;
    while (current) {
        ++values_remaining;
        current = current->heap_node().next_;
    }

    return {VALUE_POOL_SIZE - values_remaining, values_remaining};
}


void value_pool_init()
{
    for (int i = 0; i < VALUE_POOL_SIZE; ++i) {
        auto v = (Value*)(value_pool_data + i);

        v->hdr_.alive_ = false;
        v->hdr_.mark_bit_ = false;
        v->hdr_.type_ = Value::Type::heap_node;

        v->heap_node().next_ = value_pool;
        value_pool = v;
    }
    value_remaining_count = VALUE_POOL_SIZE;
}


Value* value_pool_alloc()
{
    if (value_pool) {
        auto ret = value_pool;
        value_pool = ret->heap_node().next_;
        --value_remaining_count;
        return (Value*)ret;
    }
    return nullptr;
}


void value_pool_free(Value* value)
{
    value->hdr_.type_ = Value::Type::heap_node;
    value->hdr_.alive_ = false;
    value->hdr_.mark_bit_ = false;
    ++value_remaining_count;

    value->heap_node().next_ = value_pool;
    value_pool = value;
}


using Finalizer = void (*)(Value*);

struct FinalizerTableEntry
{
    constexpr FinalizerTableEntry(Finalizer fn) : fn_(fn)
    {
    }

    Finalizer fn_;
};


constexpr const std::array<FinalizerTableEntry, Value::Type::count> fin_table =
    {
        HeapNode::finalizer,
        Nil::finalizer,
        Integer::finalizer,
        Cons::finalizer,
        Function::finalizer,
        Error::finalizer,
        Symbol::finalizer,
        UserData::finalizer,
        DataBuffer::finalizer,
        String::finalizer,
        Float::finalizer,
        Wrapped::finalizer,
        __Reserved<Value::Type::__reserved_3>::finalizer,
        __Reserved<Value::Type::__reserved_2>::finalizer,
        __Reserved<Value::Type::__reserved_1>::finalizer,
        __Reserved<Value::Type::__reserved_0>::finalizer,
};


#define MAX_NAMED_ARGUMENTS 5
#define RECENT_ALLOC_CACHE_SIZE 8
#define __USE_RECENT_ALLOC_CACHE__


struct Context
{
    using OperandStack = Buffer<Value*, 497>;


    Context()
        : operand_stack_(allocate_dynamic<OperandStack>("lisp-operand-stack"))
    {
        if (not operand_stack_) {
            PLATFORM.fatal("pointer compression test failed");
        }
    }

    DynamicMemory<OperandStack> operand_stack_;

    // If the game was built with a correctly formatted symbol lookup table,
    // then this in-memory table should never be needed...
    Optional<DynamicMemory<StringInternTable>> string_intern_table_;

    Value* nil_ = nullptr;
    Value* string_buffer_ = nullptr;
    Value* globals_tree_ = nullptr;
    Value* tree_nullnode_ = nullptr;

    Value* lexical_bindings_ = nullptr;
    Value* macros_ = nullptr;

    Value* callstack_ = nullptr;

    // contains symbol values $0, $1, $2, etc. used for argument substitution.
    // storing the symbols here significantly simplifies some edge cases
    // involving garbage collection that crop up during argument substitution.
    Value* argument_symbols_[MAX_NAMED_ARGUMENTS];

#ifdef __USE_RECENT_ALLOC_CACHE__
    // Ok, so this array exists mainly for the sake of my own sanity. Allocating
    // lisp values within C++ code is extremely fragile; it is very easy to
    // forget to protect something from the GC, resulting in dangling pointer
    // issues if the gc runs unexpectedly and collects something that's actually
    // in use. The recent_alloc_cache stores N recently allocated values, and
    // the GC will not collect these N vals.
    //
    // For example: If someone calls make_cons(make_symbol("..."), L_NIL); Then
    // there's a potential gc bug, where allocation of the cons value within
    // make cons triggers a gc sweep which collects the input symbol argument,
    // which is not protected from the GC. The existence of the
    // recent_alloc_cache simplifies things for the programmer, by protecting
    // values from the gc in cases of programmer error, where the developer may
    // have made an honest mistake and forgot to gc protect something.
    Value* recent_alloc_cache_[RECENT_ALLOC_CACHE_SIZE];
#endif

    const char* external_symtab_contents_ = nullptr;
    u32 external_symtab_size_;

    const char* external_constant_tab_ = nullptr;
    u32 external_constant_tab_size_;

    NativeInterface native_interface_;

    int string_intern_pos_ = 0;
    int eval_depth_ = 0;
    int interp_entry_count_ = 0;
    u32 alloc_highwater_ = 0;

    u16 string_buffer_remaining_ = 0;
    u16 arguments_break_loc_;
    u8 current_fn_argc_ = 0;
    u8 recent_alloc_cache_index_ = 0;
    bool strict_ : 1 = false;
    bool callstack_untouched_ : 1 = true;
    bool critical_gc_alert_ : 1 = false;

    struct GensymState
    {
        u8 char_1_ : 6;
        u8 char_2_ : 6;
        u8 char_3_ : 6;

        GensymState() : char_1_(0), char_2_(0), char_3_(0)
        {
        }

    } gensym_state_;
};


static Optional<Context> bound_context;


static void invoke_finalizer(Value* value);
void value_pool_free(Value* value);


void __unsafe_discard_list(Value* list)
{
    // NOTE: this function manually deallocates a list, and does not check to
    // make sure that the value passed is actually of list type. This function
    // may only be called in places within the interpreter implementation where
    // we are throwing out some intermediary datastructures and know that the
    // data could not possibly be referenced by anything.

    while (list not_eq L_NIL) {
        auto next = list->cons().cdr();
        invoke_finalizer(list);
        value_pool_free(list);
        list = next;
    }
}


static void push_callstack(Value* function)
{
    push_op(function); // GC protect
    bound_context->callstack_ = make_cons(function, bound_context->callstack_);
    pop_op(); // gc unprotect
}


static void pop_callstack()
{
    auto old = bound_context->callstack_;

    bound_context->callstack_ = bound_context->callstack_->cons().cdr();

    if (bound_context->callstack_untouched_) {
        // If nothing referenced the callstack by calling stacktrace(), then we
        // can deallocate the memory reserved for the stack frame right away.
        invoke_finalizer(old);
        value_pool_free(old);
    }

    if (length(bound_context->callstack_) == 1) {
        // If we're returning back to the toplevel, invoke the GC sometimes if
        // it looks like we're running low on lisp values...
        //
        // Returning to the toplevel should be a very safe place to run the
        // collector--we aren't in the middle of running any interesting code.

        if (value_remaining_count < early_gc_threshold) {
            run_gc();
        }
    }
}


const char* native_interface_resolve_intern_default(const char*)
{
    return nullptr;
}


NativeInterface::LookupResult native_interface_fn_lookup_default(const char*)
{
    return {EMPTY_SIG(0), nullptr};
}


void native_interface_fn_name_getter_default(SymbolCallback)
{
}


const char* native_interface_fn_name_lookup_default(NativeInterface::Function)
{
    return nullptr;
}


NativeInterface::NativeInterface()
    : lookup_function_(native_interface_fn_lookup_default),
      resolve_intern_sym_(native_interface_resolve_intern_default),
      lookup_name_(native_interface_fn_name_lookup_default),
      get_symbols_(native_interface_fn_name_getter_default)
{
}


void register_native_interface(NativeInterface ni)
{
    if (bound_context->native_interface_.lookup_function_ not_eq
        native_interface_fn_lookup_default) {
        PLATFORM.fatal("only one NativeInterface may be registered at a time!");
    }
    bound_context->native_interface_ = ni;
}


void register_external_symtab(const char* data, u32 len)
{
    bound_context->external_symtab_contents_ = data;
    bound_context->external_symtab_size_ = len;
}


// Globals tree node:
// ((key . value) . (left-child . right-child))
//
// i.e.: Each global variable binding uses three cons cells.


using GlobalsTreeVisitor = ::Function<6 * sizeof(void*), void(Value&, Value&)>;


static Value* left_subtree(Value* tree)
{
    return tree->cons().cdr()->cons().car();
}


static Value* right_subtree(Value* tree)
{
    return tree->cons().cdr()->cons().cdr();
}


static void set_right_subtree(Value* tree, Value* value)
{
    tree->cons().cdr()->cons().set_cdr(value);
}


static void set_left_subtree(Value* tree, Value* value)
{
    tree->cons().cdr()->cons().set_car(value);
}


// Abbreviations, for the sake of my own sanity.
#define RST(T) right_subtree(T)
#define LST(T) left_subtree(T)
#define SRST(T, V) set_right_subtree(T, V)
#define SLST(T, V) set_left_subtree(T, V);
#define TKEY(T) T->cons().car()->cons().car()->symbol().unique_id()


Value* globals_tree_splay(Value* t, Value* key)
{
    Value *L, *R, *Y;
    if (t == get_nil()) {
        return t;
    }

    // Top-down traversal requires one proxy object, which we'll manually
    // deallocate later.
    Value* temp = bound_context->tree_nullnode_;

    L = R = temp;

    auto inp_key = key->symbol().unique_id();

    for (;;) {
        if (inp_key < TKEY(t)) {
            if (LST(t) == get_nil())
                break;
            if (inp_key < TKEY(LST(t))) {
                Y = LST(t); /* rotate right */
                SLST(t, RST(Y));
                SRST(Y, t);
                t = Y;
                if (LST(t) == get_nil())
                    break;
            }
            SLST(R, t); /* link right */
            R = t;
            t = LST(t);
        } else if (inp_key > TKEY(t)) {
            if (RST(t) == get_nil())
                break;
            if (inp_key > TKEY(RST(t))) {
                Y = RST(t); /* rotate left */
                SRST(t, LST(Y));
                SLST(Y, t);
                t = Y;
                if (RST(t) == get_nil())
                    break;
            }
            SRST(L, t); /* link left */
            L = t;
            t = RST(t);
        } else {
            break;
        }
    }
    SRST(L, LST(t)); /* assemble */
    SLST(R, RST(t));
    SLST(t, RST(temp));
    SRST(t, LST(temp));

    return t;
}


static bool globals_tree_insert(Value* key, Value* value, bool define_var)
{
    auto& ctx = *bound_context;

    if (ctx.globals_tree_ == get_nil()) {

        if (not define_var) {
            return false;
        }

        Protected new_kvp(make_cons(key, value));

        // The empty set of left/right children
        push_op(make_cons(get_nil(), get_nil()));

        auto new_tree = make_cons(new_kvp, get_op0());
        pop_op();

        ctx.globals_tree_ = new_tree;

        return true;

    } else {
        auto& ctx = *bound_context;
        auto pt = globals_tree_splay(ctx.globals_tree_, key);

        if (key->symbol().unique_id() < TKEY(pt)) {
            Protected new_kvp(make_cons(key, value));
            auto node = make_cons(new_kvp, make_cons(get_nil(), get_nil()));
            SLST(node, LST(pt));
            SRST(node, pt);
            SLST(pt, get_nil());
            ctx.globals_tree_ = node;
            if (not define_var) {
                return false;
            }
        } else if (key->symbol().unique_id() > TKEY(pt)) {
            Protected new_kvp(make_cons(key, value));
            auto node = make_cons(new_kvp, make_cons(get_nil(), get_nil()));
            SRST(node, RST(pt));
            SLST(node, pt);
            SRST(pt, get_nil());
            ctx.globals_tree_ = node;
            if (not define_var) {
                return false;
            }
        } else {
            pt->cons().car()->cons().set_cdr(value);
            ctx.globals_tree_ = pt;
        }
        return true;
    }
}


// Invokes callback with (key . value) for each global var definition.
// In place traversal, using Morris algorithm.
static void globals_tree_traverse(Value* root, GlobalsTreeVisitor callback)
{
    if (root == get_nil()) {
        return;
    }

    auto current = root;
    auto prev = get_nil();

    while (current not_eq get_nil()) {

        if (left_subtree(current) == get_nil()) {
            callback(*current->cons().car(), *current);
            current = right_subtree(current);
        } else {
            prev = left_subtree(current);

            while (right_subtree(prev) not_eq get_nil() and
                   right_subtree(prev) not_eq current) {
                prev = right_subtree(prev);
            }

            if (right_subtree(prev) == get_nil()) {
                set_right_subtree(prev, current);
                current = left_subtree(current);
            } else {
                set_right_subtree(prev, get_nil());
                callback(*current->cons().car(), *current);
                current = right_subtree(current);
            }
        }
    }
}


static void globals_tree_erase(Value* key)
{
    auto& ctx = *bound_context;

    if (ctx.globals_tree_ == get_nil()) {
        return;
    }

    auto current = ctx.globals_tree_;
    auto prev = current;
    bool erase_left = true;

    while (current not_eq get_nil()) {

        auto current_key = current->cons().car()->cons().car();

        if (current_key->symbol().unique_id() == key->symbol().unique_id()) {

            Protected erased(current);

            if (current == prev) {
                ctx.globals_tree_ = get_nil();
            } else {
                if (erase_left) {
                    prev->cons().cdr()->cons().set_car(get_nil());
                } else {
                    prev->cons().cdr()->cons().set_cdr(get_nil());
                }
            }

            auto reattach_child = [](Value& kvp, Value&) {
                globals_tree_insert(kvp.cons().car(), kvp.cons().cdr(), true);
            };

            auto left_child = erased->cons().cdr()->cons().car();
            if (left_child not_eq get_nil()) {
                globals_tree_traverse(left_child, reattach_child);
            }

            auto right_child = erased->cons().cdr()->cons().cdr();
            if (right_child not_eq get_nil()) {
                globals_tree_traverse(right_child, reattach_child);
            }

            return;
        }

        prev = current;

        if (current_key->symbol().unique_id() < key->symbol().unique_id()) {
            erase_left = true;
            current = current->cons().cdr()->cons().car();
        } else {
            erase_left = false;
            current = current->cons().cdr()->cons().cdr();
        }
    }
}


static Value* globals_tree_find(Value* key)
{
    auto& ctx = *bound_context;

    if (ctx.globals_tree_ == get_nil()) {
        return nullptr;
    }

    auto pt = globals_tree_splay(ctx.globals_tree_, key);
    ctx.globals_tree_ = pt;
    if (key->symbol().unique_id() ==
        pt->cons().car()->cons().car()->symbol().unique_id()) {
        return pt->cons().car()->cons().cdr();
    }

    return nullptr;
}


static bool is_error(Value* val)
{
    return val->type() == Value::Type::error;
}


static int is_list_slowpath(Value* val)
{
    // If we're iterating over a whole list, we might as well calculate the
    // length too.
    int len = 0;

    while (val not_eq get_nil()) {
        if (val->type() not_eq Value::Type::cons) {
            return 0;
        }
        ++len;
        val = val->cons().cdr();
    }
    return len;
}


bool is_list(Value* val)
{
    if (val->type() == Value::Type::cons and val->cons().is_definitely_list_) {
        return true;
    }

    if (val == get_nil()) {
        return true;
    }

    if (is_list_slowpath(val)) {
        if (val->type() == Value::Type::cons) {
            val->cons().is_definitely_list_ = true;
        }
        return true;
    }

    return false;
}


Value* get_nil()
{
    return bound_context->nil_;
}


void get_interns(::Function<6 * sizeof(void*), void(const char*)> callback)
{
    auto& ctx = bound_context;

    if (ctx->string_intern_table_) {
        const char* search = (*ctx->string_intern_table_)->data_;
        for (int i = 0; i < ctx->string_intern_pos_;) {
            callback(search + i);
            while (search[i] not_eq '\0') {
                ++i;
            }
            ++i;
        }
    }
}


Value* get_arg(u16 n)
{
    auto br = bound_context->arguments_break_loc_;
    auto argc = bound_context->current_fn_argc_;
    if (br >= ((argc - 1) - n)) {
        return (*bound_context->operand_stack_)[br - ((argc - 1) - n)];
    } else {
        return get_nil();
    }
}


void gc_symbols()
{
    // TODO:

    // For each symbol in the string intern table, check if any live symbol
    // objects point to the intern memory. If not, shift everything over, and,
    // fix all pointers by subtracting the intern pointers in symbols by the
    // freed offset if the intern pointer address is higher than the freed
    // address.
    //
    // NOTE: after performing short string optimizations for symbols, I put off
    // gc for the string intern table.
    //
    // NOTE: after pregenerating a symbol table for lisp scripts, I see almost
    // no reason to bother with collecting symbols. We never hit the intern
    // table anyway unless we are creating symbols at runtime, which only really
    // happens if we're declaring variables with the developer lisp repl
    // console.
}


CompressedPtr compr(Value* val)
{
    CompressedPtr result;

#ifdef USE_COMPRESSED_PTRS
    static_assert(sizeof(ValueMemory) % 2 == 0);
    result.offset_ = ((u8*)val - (u8*)value_pool_data) / sizeof(ValueMemory);
#else
    result.ptr_ = val;
#endif

    return result;
}


Value* dcompr(CompressedPtr ptr)
{
#ifdef USE_COMPRESSED_PTRS
    auto ret =
        (Value*)(((ptr.offset_ * sizeof(ValueMemory)) + (u8*)value_pool_data));
    return ret;
#else
    return (Value*)ptr.ptr_;
#endif // USE_COMPRESSED_PTRS
}


int length(Value* lat)
{
    if (lat->type() == Value::Type::nil) {
        return 0;
    }

    int len = 0;
    while (true) {
        ++len;
        lat = lat->cons().cdr();
        if (lat->type() not_eq Value::Type::cons) {
            if (lat not_eq get_nil()) {
                return 0; // not a well-formed list
            }
            break;
        }
    }

    return len;
}


Value* Function::Bytecode::bytecode_offset() const
{
    return dcompr(bytecode_)->cons().car();
}


Value* Function::Bytecode::databuffer() const
{
    return dcompr(bytecode_)->cons().cdr();
}


constexpr bool is_powerof2(int v)
{
    return v && ((v & (v - 1)) == 0);
}


static Value* alloc_value()
{
    auto init_val = [](Value* val) {
        val->hdr_.mark_bit_ = false;
        val->hdr_.alive_ = true;
        return val;
    };

    if (auto val = value_pool_alloc()) {
#ifdef __USE_RECENT_ALLOC_CACHE__
        auto& i = bound_context->recent_alloc_cache_index_;
        bound_context->recent_alloc_cache_[i++] = val;

        static_assert(is_powerof2(RECENT_ALLOC_CACHE_SIZE));
        i %= RECENT_ALLOC_CACHE_SIZE;
#endif

        return init_val(val);
    }

    if (bound_context->critical_gc_alert_) {
        PLATFORM.fatal("unexpected gc run!");
    }

    // To be honest, this is a bit of a precarious place to run the GC... lots
    // of things call alloc_value, meaning there's lots of code to comb through
    // for gc bugs. We do preemptively invoke the gc when we're running low in
    // some places to avoid having to do this...
    run_gc();

    // Hopefully, we've freed up enough memory...
    if (auto val = value_pool_alloc()) {
        return init_val(val);
    }

    Platform::fatal("LISP out of memory");

    return nullptr;
}


Value* wrap(Value* input, Value* type_sym)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::wrapped;
    val->wrapped().data_ = compr(input);
    val->wrapped().type_sym_ = compr(type_sym);
    return val;
}


Value* make_function(Function::CPP_Impl impl)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::function;
    val->function().cpp_impl_ = impl;
    val->function().sig_.reset();
    val->hdr_.mode_bits_ = Function::ModeBits::cpp_function;
    return val;
}


static Value* make_lisp_function(Value* impl)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::function;
    val->function().lisp_impl_.code_ = compr(impl);
    val->function().sig_.reset();
    val->function().lisp_impl_.lexical_bindings_ =
        compr(bound_context->lexical_bindings_);
    val->hdr_.mode_bits_ = Function::ModeBits::lisp_function;
    return val;
}


Value* clone(Value* value)
{
    // TODO!!!!!
    return value;
}


static int examine_argument_list(Value* function_impl)
{
    auto arg_lat = function_impl->cons().car();
    if (not is_list(arg_lat)) {
        PLATFORM.fatal("incorrectly formatted argument list!");
    }

    int argc = 0;
    l_foreach(arg_lat, [&](Value* val) {
        ++argc;

        Value* sym = L_NIL;

        if (val->type() not_eq Value::Type::symbol) {
            if (val->type() == Value::Type::cons) {
                if (not(val->cons().car()->type() == Value::Type::symbol and
                        val->cons().cdr()->type() == Value::Type::symbol)) {
                    PLATFORM.fatal(::format("invalid type declaration: %",
                                            val_to_string<96>(val).c_str()));
                } else {
                    sym = val->cons().car();
                }
            } else {
                PLATFORM.fatal(::format(
                    "value \'%\' in argument list \'%\' is non-symbol!",
                    val_to_string<64>(val).c_str(),
                    val_to_string<128>(arg_lat).c_str()));
            }
        } else {
            sym = val;
        }

        if (not bound_context->external_symtab_contents_ and
            sym->hdr_.mode_bits_ not_eq (u8) Symbol::ModeBits::small) {
            PLATFORM.fatal(::format(
                "symbol name \'%\' in argument list \'%\' is too long! "
                "(4 char limit)",
                sym->symbol().name(),
                val_to_string<128>(arg_lat).c_str()));
        }
    });

    return argc;
}


struct ArgBinding
{
    Symbol* sym_; // This symbol should be safe to store a pointer to... it's in
                  // the input source code list, so it's a proper gc root...

    u8 replacement_;
    u8 type_;
    bool referenced_in_closure_;
};


struct ArgBindings
{
    Buffer<ArgBinding, MAX_NAMED_ARGUMENTS> bindings_;
    ArgBindings* parent_ = nullptr;
};


ArgBindings make_arg_bindings(Value* arg_lat, ArgBindings* parent)
{
    ArgBindings b;
    b.parent_ = parent;

    int arg = 0;
    l_foreach(arg_lat, [&](Value* val) {
        Value* sym;
        u8 type = Value::Type::nil;
        if (val->type() == Value::Type::cons) {
            sym = val->cons().car();
            auto& type_symbol = val->cons().cdr()->symbol();
            if (str_eq(type_symbol.name(), "int")) {
                type = Value::Type::integer;
            } else if (str_eq(type_symbol.name(), "string")) {
                type = Value::Type::string;
            } else if (str_eq(type_symbol.name(), "pair")) {
                type = Value::Type::cons;
            } else if (str_eq(type_symbol.name(), "symbol")) {
                type = Value::Type::symbol;
            } else if (str_eq(type_symbol.name(), "float")) {
                type = Value::Type::fp;
            } else if (str_eq(type_symbol.name(), "error")) {
                type = Value::Type::error;
            } else if (str_eq(type_symbol.name(), "userdata")) {
                type = Value::Type::user_data;
            } else if (str_eq(type_symbol.name(), "databuffer")) {
                type = Value::Type::databuffer;
            } else if (str_eq(type_symbol.name(), "nil")) {
                type = Value::Type::nil;
            } else if (str_eq(type_symbol.name(), "lambda")) {
                type = Value::Type::function;
            } else if (str_eq(type_symbol.name(), "wrapped")) {
                type = Value::Type::wrapped;
            } else {
                PLATFORM.fatal(
                    ::format("invalid type symbol %", type_symbol.name()));
            }
        } else {
            sym = val;
        }
        if (not b.bindings_.push_back(ArgBinding{&sym->symbol(), (u8)arg++, type, false})) {
            PLATFORM.fatal("too many named arguments for function! Max 5");
        }
    });

    return b;
}


static void arg_substitution_impl(Value* impl, ArgBindings& bindings)
{
    auto& ctx = bound_context;

    while (true) {
        if (impl->type() not_eq Value::Type::cons) {
            break;
        } else {
            auto val = impl->cons().car();

            if (is_list(val)) {
                auto first = val->cons().car();
                if (first->type() == Value::Type::symbol) {
                    if (str_eq(first->symbol().name(), "let")) {
                        auto let_bindings = val->cons().cdr()->cons().car();

                        l_foreach(let_bindings, [&](Value* val) {
                            auto sym = val->cons().car();
                            for (auto& binding : bindings.bindings_) {
                                if (sym->symbol().unique_id() ==
                                    binding.sym_->unique_id()) {

                                    PLATFORM.fatal(::format(
                                        "let binding % shadows argument %",
                                        binding.sym_->name(),
                                        sym->symbol().name()));
                                }
                            }

                            auto current = bindings.parent_;
                            while (current) {
                                for (auto& b : current->bindings_) {
                                    if (b.sym_->unique_id() ==
                                        sym->symbol().unique_id()) {
                                        PLATFORM.fatal(
                                            ::format("let binding shadows "
                                                     "captured parent arg '%' ",
                                                     sym->symbol().name()));
                                    }
                                }
                                current = current->parent_;
                            }
                        });

                        arg_substitution_impl(let_bindings, bindings);
                        arg_substitution_impl(val, bindings);
                    } else if (str_eq(first->symbol().name(), "lambda")) {
                        // Now this one's a bit tricky. We need to switch
                        // context and fetch this nested lambda's argument list,
                        // and continue recursion with that list instead.

                        auto lambda = val->cons().cdr();

                        auto arg_lat = lambda->cons().car();
                        auto fn_impl = lambda->cons().cdr();

                        auto nested_bindings =
                            make_arg_bindings(arg_lat, &bindings);

                        arg_substitution_impl(fn_impl, nested_bindings);

                        val->cons().set_car(make_symbol("fn")); //
                        val->cons().set_cdr(fn_impl);           // very naughty!

                        ListBuilder closure;
                        bool arg_closure_exists = false;
                        for (auto& binding : bindings.bindings_) {
                            if (binding.referenced_in_closure_) {
                                ListBuilder bind;
                                bind.push_back((lisp::Value*)binding.sym_);
                                bind.push_back(ctx->argument_symbols_[binding.replacement_]);
                                closure.push_back(bind.result());
                                arg_closure_exists = true;
                            }
                        }

                        if (arg_closure_exists) {
                            // Very naughty indeed... we need to inject
                            // arguments into closures, but, we're implementing
                            // function argument reference by replacing argument
                            // names with stack slots. To preserve access into
                            // stack slots in captured lambdas (funarg problem),
                            // we're wrapping the enclosed function in a
                            // synthetic let binding, which binds the values of
                            // function arguments from stack slots to actual
                            // variable names.
                            val->cons().set_car(make_symbol("let"));
                            val->cons().set_cdr(L_CONS(closure.result(),
                                                       L_CONS(L_CONS(make_symbol("fn"),
                                                                     fn_impl),
                                                              L_NIL)));
                        }

                    } else if (str_eq(first->symbol().name(), "fn")) {
                        // Do nothing... cannot substitute a function argument
                        // within a lambda implementation.
                    } else {
                        arg_substitution_impl(val, bindings);
                    }
                } else {
                    // Most lists begin with a symbol, but other things, like a
                    // function call result, can begin a list.
                    arg_substitution_impl(val, bindings);
                }
            } else if (val->type() == Value::Type::symbol) {
                bool replaced = false;
                for (auto& binding : bindings.bindings_) {
                    if (binding.sym_->unique_id() ==
                        val->symbol().unique_id()) {

                        auto old_val = impl->cons().car();

                        // We're performing a destructive operation on a
                        // cons. We can throw out the old value...  This is
                        // actually safe.
                        invoke_finalizer(old_val);
                        value_pool_free(old_val);

                        auto sym = ctx->argument_symbols_[binding.replacement_];

                        impl->cons().set_car(sym);
                        replaced = true;
                    }
                }

                auto current = bindings.parent_;
                while (current and not replaced) {
                    for (auto& b : current->bindings_) {
                        if (b.sym_->unique_id() == val->symbol().unique_id()) {
                            b.referenced_in_closure_ = true;
                            break;
                        }
                    }
                    current = current->parent_;
                }
            }
        }

        impl = impl->cons().cdr();
    }
}


ArgBindings perform_argument_substitution(Value* impl)
{
    Protected gc_root(impl);

    auto arg_lat = impl->cons().car();
    auto fn_impl = impl->cons().cdr();

    auto arguments = make_arg_bindings(arg_lat, nullptr);

    if (arguments.bindings_.size() > 0) { // Don't bother with no-arg functions
        arg_substitution_impl(fn_impl, arguments);
    }

    return arguments;
}


static Value* make_lisp_argumented_function(Value* impl)
{
    int argc = examine_argument_list(impl);

    if (argc > MAX_NAMED_ARGUMENTS) {

        const char* error_fmt = "no more than % named args allowed in function";

        Protected error_str(L_NIL);
        error_str =
            make_string(::format(error_fmt, MAX_NAMED_ARGUMENTS).c_str());

        return make_error(Error::Code::invalid_syntax, error_str);
    }

    auto val = alloc_value();

    // Now, the complicated part...
    // We need to clone the existing source code input, then we will alter
    // the contents to substitute the input arguments with positional
    // values.
    // If an enclosed lambda captures an input argument, then we need to
    // raise an error.
    Protected new_impl(impl);
    // new_impl = clone(impl); TODO... can anything bad happen if we mutate
    // a list representing source code? I'm not certain.

    auto bind = perform_argument_substitution(new_impl);

    val->hdr_.type_ = Value::Type::function;
    val->function().lisp_impl_.code_ = compr(new_impl->cons().cdr());
    val->function().sig_.reset();
    val->function().sig_.required_args_ = argc;
    val->function().lisp_impl_.lexical_bindings_ =
        compr(bound_context->lexical_bindings_);

    if (bind.bindings_.size() > 0) {
        val->function().sig_.arg0_type_ = bind.bindings_[0].type_;
    }
    if (bind.bindings_.size() > 1) {
        val->function().sig_.arg1_type_ = bind.bindings_[1].type_;
    }
    if (bind.bindings_.size() > 2) {
        val->function().sig_.arg2_type_ = bind.bindings_[2].type_;
    }
    if (bind.bindings_.size() > 3) {
        val->function().sig_.arg3_type_ = bind.bindings_[3].type_;
    }

    val->hdr_.mode_bits_ = Function::ModeBits::lisp_function;

    return val;
}


Value* make_bytecode_function(Value* bytecode)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::function;
    val->function().bytecode_impl_.lexical_bindings_ =
        compr(bound_context->lexical_bindings_);
    val->function().sig_.reset();

    val->function().bytecode_impl_.bytecode_ = compr(bytecode);
    val->hdr_.mode_bits_ = Function::ModeBits::lisp_bytecode_function;
    return val;
}


Value* make_cons_safe(Value* car, Value* cdr)
{
    push_op(car);
    push_op(cdr);

    auto result = make_cons(car, cdr);

    pop_op();
    pop_op();

    return result;
}


Value* make_cons(Value* car, Value* cdr)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::cons;
    val->cons().set_car(car);
    val->cons().__set_cdr(cdr);
    val->cons().is_definitely_list_ = false;
    return val;
}


Value* make_boolean(bool is_true)
{
    if (is_true) {
        return L_INT(1);
    } else {
        return L_NIL;
    }
}


Value* make_integer(s32 value)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::integer;
    val->integer().value_ = value;
    return val;
}


Value* make_float(Float::ValueType value)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::fp;
    val->fp().value_ = value;
    return val;
}


Value* make_list(u32 length)
{
    if (length == 0) {
        return get_nil();
    }
    auto head = make_cons(get_nil(), get_nil());
    while (--length) {
        push_op(head); // To keep head from being collected, in case make_cons()
                       // triggers the gc.
        auto cell = make_cons(get_nil(), head);
        pop_op(); // head

        head = cell;
    }
    return head;
}


Value* make_error(const char* message)
{
    Protected str(make_string(message));

    return make_error(Error::Code::custom, str);
}


Value* make_error(Error::Code error_code, Value* context)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::error;
    val->error().code_ = error_code;
    val->error().context_ = compr(context);
    push_op(val); // gc protect
    val->error().stacktrace_ = compr(stacktrace());
    pop_op(); // unprotect
    return val;
}


Value* make_symbol(const char* name, Symbol::ModeBits mode)
{
    if (mode == Symbol::ModeBits::small and
        strlen(name) > Symbol::buffer_size) {
        Platform::fatal("Symbol ModeBits small with len > internal buffer");
    }

    if (strlen(name) <= Symbol::buffer_size) {
        mode = Symbol::ModeBits::small;
    }

    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::symbol;
    val->hdr_.mode_bits_ = (u8)mode;
    val->symbol().set_name(name);
    return val;
}


static Value* intern_to_symbol(const char* already_interned_str)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::symbol;
    val->hdr_.mode_bits_ = (u8)Symbol::ModeBits::stable_pointer;
    val->symbol().set_name(already_interned_str);
    return val;
}


Value* make_userdata(void* obj, u16 tag)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::user_data;
    val->user_data().obj_ = obj;
    val->user_data().tag_ = tag;
    return val;
}


Value* make_databuffer(const char* sbr_tag)
{
    if (not scratch_buffers_remaining()) {
        // Collect any data buffers that may be lying around.
        run_gc();
    }

    if (strlen(sbr_tag) == 0) {
        sbr_tag = "lisp-databuffer";
    }

    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::databuffer;
    new ((ScratchBufferPtr*)val->databuffer().sbr_mem_)
        ScratchBufferPtr(make_zeroed_sbr(sbr_tag));
    return val;
}


void live_values(::Function<6 * sizeof(void*), void(Value&)> callback);


Value* make_string_from_literal(const char* str)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::string;
    val->string().data_.literal_.value_ = str;
    val->string().hdr_.mode_bits_ = String::literal_string;
    return val;
}


std::pair<Value*, int> store_string(const char* string, u32 len)
{
    Value* existing_buffer = nullptr;
    auto free = bound_context->string_buffer_remaining_;

    if (bound_context->string_buffer_ not_eq L_NIL) {
        if (free > len + 1) { // +1 for null term, > for other null term
            existing_buffer = bound_context->string_buffer_;
            bound_context->string_buffer_remaining_ -= len + 1;
        } else {
            bound_context->string_buffer_ = L_NIL;
            bound_context->string_buffer_remaining_ = 0;
        }
    }

    if (existing_buffer) {
        const auto offset = (SCRATCH_BUFFER_SIZE - free) + 1;

        auto write_ptr = existing_buffer->databuffer().value()->data_ + offset;

        memcpy(write_ptr, string, len);

        return {existing_buffer, offset};

    } else {

        // Because we're allocating a fresh buffer, as the prior one was full.
        bound_context->string_buffer_remaining_ =
            SCRATCH_BUFFER_SIZE - (len + 1);

        auto buffer = make_databuffer("lisp-string-bulk-allocator");

        Protected p(buffer);
        bound_context->string_buffer_ = buffer;

        for (int i = 0; i < SCRATCH_BUFFER_SIZE; ++i) {
            buffer->databuffer().value()->data_[i] = '\0';
        }
        auto write_ptr = buffer->databuffer().value()->data_;

        while (*string) {
            *write_ptr++ = *string++;
        }

        return {buffer, 0};
    }
}


Value* make_string(const char* string)
{
    auto len = strlen(string);

    if (len == 0) {
        return make_string_from_literal("");
    }

    // NOTE: for really small strings, three bytes or less, we do an internal
    // buffer optmization. For this reason, we don't have a separate datatype
    // for character literals.
    if (len < 4) {

        auto val = alloc_value();
        val->hdr_.type_ = Value::Type::string;
        val->string().hdr_.mode_bits_ = String::small_string;

        memset(val->string().data_.small_string_.data_, 0, 4);
        memcpy(val->string().data_.small_string_.data_, string, len);

        return val;
    }

    auto [buffer, offset] = store_string(string, len);

    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::string;
    val->string().data_.memory_.databuffer_ = compr(buffer);
    val->string().data_.memory_.offset_ = offset;
    val->string().hdr_.mode_bits_ = String::memory_string;
    return val;
}


void set_list(Value* list, u32 position, Value* value)
{
    while (position--) {
        if (list->type() not_eq Value::Type::cons) {
            // TODO: raise error
            return;
        }
        list = list->cons().cdr();
    }

    if (list->type() not_eq Value::Type::cons) {
        // TODO: raise error
        return;
    }

    list->cons().set_car(value);
}


Value* get_list(Value* list, u32 position)
{
    while (position--) {
        if (list->type() not_eq Value::Type::cons) {
            // TODO: raise error
            return get_nil();
        }
        list = list->cons().cdr();
    }

    if (list->type() not_eq Value::Type::cons) {
        // TODO: raise error
        return get_nil();
    }

    return list->cons().car();
}


void pop_op()
{
    bound_context->operand_stack_->pop_back();
}


void push_op(Value* operand)
{
    bound_context->operand_stack_->push_back(operand, nullptr, [](void*) {
        Platform::fatal("LISP stack overflow.");
    });
}


void insert_op(u32 offset, Value* operand)
{
    auto& stack = bound_context->operand_stack_;
    auto pos = stack->end() - offset;
    stack->insert(pos, operand);
}


Value* get_op0()
{
    auto& stack = bound_context->operand_stack_;
    return stack->back();
}


Value* get_op1()
{
    auto& stack = bound_context->operand_stack_;
    return *(stack->end() - 2);
}


Value* get_op(u32 offset)
{
    auto& stack = bound_context->operand_stack_;
    if (offset >= stack->size()) {
        return get_nil(); // TODO: raise error
    }

    return (*stack)[(stack.obj_->size() - 1) - offset];
}


void lexical_frame_push()
{
    bound_context->lexical_bindings_ =
        make_cons(get_nil(), bound_context->lexical_bindings_);
}


void lexical_frame_pop()
{
    bound_context->lexical_bindings_ =
        bound_context->lexical_bindings_->cons().cdr();
}


void lexical_frame_store(Value* kvp)
{
    bound_context->lexical_bindings_->cons().set_car(
        make_cons(kvp, bound_context->lexical_bindings_->cons().car()));
}


void vm_execute(Value* code, int start_offset);


const char* type_to_string(ValueHeader::Type tp)
{
    switch (tp) {
    case Value::Type::count:
    case Value::Type::__reserved_3:
    case Value::Type::__reserved_2:
    case Value::Type::__reserved_1:
    case Value::Type::__reserved_0:
    case Value::Type::nil:
        return "?";
    case Value::Type::heap_node:
        return "?";
    case Value::Type::integer:
        return "int";
    case Value::Type::cons:
        return "pair";
    case Value::Type::function:
        return "lambda";
    case Value::Type::error:
        return "error";
    case Value::Type::symbol:
        return "symbol";
    case Value::Type::user_data:
        return "userdata";
    case Value::Type::databuffer:
        return "databuffer";
    case Value::Type::string:
        return "string";
    case Value::Type::fp:
        return "float";
    case Value::Type::wrapped:
        return "wrapped";
    }
    return "?";
}


// The function arguments should be sitting at the top of the operand stack
// prior to calling funcall. The arguments will be consumed, and replaced with
// the result of the function call.
void funcall(Value* obj, u8 argc)
{
    auto pop_args = [&argc] {
        for (int i = 0; i < argc; ++i) {
            bound_context->operand_stack_->pop_back();
        }
    };

    // NOTE: The callee must be somewhere on the operand stack, so it's safe
    // to store this unprotected var here.
    Value* prev_bindings = bound_context->lexical_bindings_;

    auto& ctx = *bound_context;
    auto prev_arguments_break_loc = ctx.arguments_break_loc_;
    auto prev_argc = ctx.current_fn_argc_;

    push_callstack(obj);

    switch (obj->type()) {
    case Value::Type::function: {
        if (obj->function().sig_.required_args_ > argc) {
            pop_args();
            push_op(make_error(Error::Code::invalid_argc, obj));
            break;
        }

        auto arg_error = [&](auto exp_type, auto got_type) {
            return ::format<256>("invalid arg type for %! expected %, got %",
                                 val_to_string<64>(obj).c_str(),
                                 type_to_string((ValueHeader::Type)exp_type),
                                 type_to_string((ValueHeader::Type)got_type));
        };

#define CHECK_ARG_TYPE(ARG, FIELD)                                             \
    if (obj->function().sig_.FIELD not_eq Value::Type::nil and                 \
        get_arg(ARG)->type() not_eq obj->function().sig_.FIELD and             \
        not(get_arg(ARG)->type() == Value::Type::nil and                       \
            obj->function().sig_.FIELD == Value::Type::cons)) {                \
        pop_args();                                                            \
        push_op(make_error(                                                    \
            arg_error(obj->function().sig_.FIELD, get_arg(ARG)->type())        \
                .c_str()));                                                    \
        break;                                                                 \
    }

#define CHECK_ARG_TYPES()                                                      \
    if (argc > 3) {                                                            \
        CHECK_ARG_TYPE(0, arg0_type_);                                         \
        CHECK_ARG_TYPE(1, arg1_type_);                                         \
        CHECK_ARG_TYPE(2, arg2_type_);                                         \
        CHECK_ARG_TYPE(3, arg3_type_);                                         \
    } else if (argc > 2) {                                                     \
        CHECK_ARG_TYPE(0, arg0_type_);                                         \
        CHECK_ARG_TYPE(1, arg1_type_);                                         \
        CHECK_ARG_TYPE(2, arg2_type_);                                         \
    } else if (argc > 1) {                                                     \
        CHECK_ARG_TYPE(0, arg0_type_);                                         \
        CHECK_ARG_TYPE(1, arg1_type_);                                         \
    } else if (argc > 0) {                                                     \
        CHECK_ARG_TYPE(0, arg0_type_);                                         \
    }

        switch (obj->hdr_.mode_bits_) {
        case Function::ModeBits::cpp_function: {
            auto result = obj->function().cpp_impl_(argc);
            pop_args();
            push_op(result);
            break;
        }

        case Function::ModeBits::lisp_function: {
            PLATFORM_EXTENSION(stack_check);

            auto& ctx = *bound_context;
            ctx.lexical_bindings_ =
                dcompr(obj->function().lisp_impl_.lexical_bindings_);
            const auto break_loc = ctx.operand_stack_->size() - 1;
            ctx.arguments_break_loc_ = break_loc;
            ctx.current_fn_argc_ = argc;
            if (bound_context->strict_) {
                CHECK_ARG_TYPES();
            }

            auto expression_list = dcompr(obj->function().lisp_impl_.code_);
            auto result = get_nil();
            push_op(result);
            while (expression_list not_eq get_nil()) {
                if (expression_list->type() not_eq Value::Type::cons) {
                    break;
                }
                pop_op(); // result
                ctx.arguments_break_loc_ = break_loc;
                ctx.current_fn_argc_ = argc;
                eval(expression_list->cons().car()); // new result
                if (is_error(get_op0())) {
                    break;
                }
                expression_list = expression_list->cons().cdr();
            }
            result = get_op0();
            pop_op(); // result
            pop_args();
            push_op(result);
            break;
        }

        case Function::ModeBits::lisp_bytecode_function: {
            PLATFORM_EXTENSION(stack_check);

            auto& ctx = *bound_context;
            const auto break_loc = ctx.operand_stack_->size() - 1;
            ctx.arguments_break_loc_ = break_loc;
            ctx.current_fn_argc_ = argc;
            if (bound_context->strict_) {
                CHECK_ARG_TYPES();
            }

            ctx.lexical_bindings_ =
                dcompr(obj->function().lisp_impl_.lexical_bindings_);

            vm_execute(obj->function().bytecode_impl_.databuffer(),
                       obj->function()
                           .bytecode_impl_.bytecode_offset()
                           ->integer()
                           .value_);

            auto result = get_op0();
            pop_op();
            pop_args();
            push_op(result);
            break;
        }
        }
        break;
    }

    default:
        push_op(make_error(Error::Code::value_not_callable, L_NIL));
        break;
    }

    pop_callstack();
    bound_context->lexical_bindings_ = prev_bindings;
    ctx.arguments_break_loc_ = prev_arguments_break_loc;
    ctx.current_fn_argc_ = prev_argc;
}


void safecall(Value* fn, u8 argc)
{
    if (fn->type() not_eq Value::Type::function) {
        Platform::fatal("attempt to call non-function!");
    }

    if (bound_context->operand_stack_->size() < argc) {
        Platform::fatal("invalid argc for safecall");
    }

    lisp::funcall(fn, argc);
    auto result = lisp::get_op(0);

    if (is_error(result)) {
        const char* tag = "lisp-fmt-buffer";
        auto p = allocate_dynamic<DefaultPrinter>(tag);
        format(result, *p);
        Platform::fatal(p->data_.c_str());
    }
}


u8 get_argc()
{
    return bound_context->current_fn_argc_;
}


Value* get_this()
{
    return bound_context->callstack_->cons().car();
}


Value* get_var_stable(const char* intern_str)
{
    return get_var(make_symbol(intern_str, Symbol::ModeBits::stable_pointer));
}


bool is_boolean_true(Value* val)
{
    switch (val->type()) {
    case Value::Type::integer:
        return val->integer().value_ not_eq 0;

    default:
        break;
    }

    return val not_eq get_nil();
}


static constexpr const long hextable[] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0,  1,  2,  3,  4,  5,  6,  7,  8,
    9,  -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1};


long hexdec(unsigned const char* hex)
{
    long ret = 0;
    while (*hex && ret >= 0) {
        ret = (ret << 4) | hextable[*hex++];
    }
    return ret;
}


bool is_executing()
{
    if (bound_context) {
        return bound_context->interp_entry_count_;
    }

    return false;
}


// Checks for undefined variable access, checks to make sure enough function
// params are supplied, checks the proper structure of special forms, etc...
void lint(Value* expr, Value* variable_list)
{
    bool is_special_form = false;

    switch (expr->type()) {
    case Value::Type::cons:
        if (is_list(expr)) {
            auto fn_sym = get_list(expr, 0);
            if (fn_sym->type() == Value::Type::cons and is_list(fn_sym)) {
                lint(fn_sym, variable_list);
                if (get_op0()->type() == Value::Type::error) {
                    return;
                }
                pop_op();
            } else if (fn_sym->type() == Value::Type::symbol) {
                auto name = fn_sym->symbol().name();

                if (str_eq(name, "'") or str_eq(name, "`")) {
                    push_op(L_NIL);
                    return; // quoted list
                } else if (str_eq(name, "macro")) {
                    // We're linting macro-expanded code, which should be enough
                    // to verify that a macro is well-formed. We don't need to
                    // lint a macro definition as well, which would be difficult
                    // to do correctly.
                    push_op(L_NIL);
                    return;
                } else if (str_eq(name, "if")) {
                    // (if cond true-branch [false-branch])
                    if (length(expr) < 3 or length(expr) > 4) {
                        push_op(make_error("malformed if expr!"));
                        return;
                    }
                    is_special_form = true;
                } else if (str_eq(name, "let")) {
                    // (let ([(sym val)...]) [body])
                    if (length(expr) < 2) {
                        push_op(make_error("malformed let expr!"));
                        return;
                    }
                    auto bindings = get_list(expr, 1);
                    while (bindings not_eq L_NIL) {
                        if (bindings->type() not_eq Value::Type::cons) {
                            push_op(make_error("invalid let binding list"));
                            return;
                        }
                        auto binding = bindings->cons().car();
                        if (not is_list(binding)) {
                            push_op(make_error("invalid let binding list"));
                            return;
                        }
                        auto sym = get_list(binding, 0);
                        if (sym->type() not_eq Value::Type::symbol) {
                            push_op(make_error("let binding missing symbol"));
                            return;
                        }
                        variable_list = L_CONS(sym, variable_list);
                        bindings = bindings->cons().cdr();
                    }
                    is_special_form = true;
                } else if (str_eq(name, "fn")) {
                    // TODO: syntax checks for low level function primitive
                    is_special_form = true;
                } else if (str_eq(name, "lambda")) {
                    // (lambda (args...) [body])
                    if (length(expr) < 2) {
                        push_op(make_error("invalid lambda syntax!"));
                        return;
                    }
                    auto args = get_list(expr, 1);
                    if (not is_list(args)) {
                        push_op(make_error(
                            "lambda expects arg list in position 1!"));
                        return;
                    }
                    while (args not_eq L_NIL) {
                        auto arg_name = args->cons().car();
                        auto arg_sym = arg_name;
                        if (arg_name->type() == Value::Type::cons) {
                            arg_sym = arg_name->cons().car();
                            // TODO: arg type is in cdr...
                        }
                        if (arg_sym->type() not_eq Value::Type::symbol) {
                            push_op(
                                make_error("invalid value in lambda arglist!"));
                            return;
                        }
                        variable_list = L_CONS(arg_sym, variable_list);
                        args = args->cons().cdr();
                    }
                    is_special_form = true;
                } else if (str_eq(name, "while")) {
                    // (while cond [body...])
                    if (length(expr) < 2) {
                        push_op(make_error("invalid while syntax!"));
                        return;
                    }
                    is_special_form = true;
                } else if (str_eq(name, "defconstant")) {
                    is_special_form = true;
                }

                auto fn = get_var(fn_sym);
                if (fn->type() == Value::Type::function) {
                    int reqd_args = fn->function().sig_.required_args_;
                    if (length(expr) - 1 < reqd_args) {
                        push_op(
                            make_error(::format("insufficient args for %!",
                                                val_to_string<64>(fn).c_str())
                                           .c_str()));
                        return;
                    }
                    auto& fn_var = fn->function();

                    auto type_check = [&](auto expected_type, int slot) {
                        auto arg = get_list(expr, slot + 1);
                        if (arg->type() == Value::Type::symbol) {
                            // TODO: track types through variable reference.
                            // For example, if the symbol refers to a function
                            // argument of known type...
                            return true;
                        }
                        if (expected_type == Value::Type::symbol) {
                            // Special case: quoted symbol
                            if (arg->type() == Value::Type::cons) {
                                if (str_eq(arg->cons().car()->symbol().name(),
                                           "'") and
                                    arg->cons().cdr()->type() ==
                                        Value::Type::symbol) {
                                    return true;
                                }
                            }
                        }
                        auto detected_type = arg->hdr_.type();
                        if (is_list(arg)) {
                            auto first = get_list(arg, 0);
                            if (first->type() == Value::Type::symbol) {
                                auto name = first->symbol().name();
                                if (str_eq(name, "lambda") or
                                    str_eq(name, "fn")) {
                                    // The first element of the list indicates
                                    // that the list is a lambda function.
                                    detected_type = Value::Type::function;
                                } else {
                                    auto builtin = __load_builtin(name);
                                    if (builtin.second) {
                                        auto rt = builtin.first.ret_type_;
                                        if (rt == Value::Type::nil) {
                                            // The builtin does not have a
                                            // concrete return type. Static
                                            // analysis cannot be performed
                                            // here.
                                            return true;
                                        } else {
                                            detected_type = (Value::Type)rt;
                                        }
                                    } else {
                                        auto v = get_var(first);
                                        if (v->type() ==
                                            Value::Type::function) {
                                            auto& fn = v->function();
                                            if (fn.sig_.ret_type_ ==
                                                Value::Type::nil) {
                                                return true;
                                            } else {
                                                detected_type =
                                                    (Value::Type)
                                                        fn.sig_.ret_type_;
                                            }
                                        } else {
                                            return true;
                                        }
                                    }
                                }
                            } else {
                                // TODO: check return type of function call, if
                                // possible...
                                //
                                // NOTE: this is the specific case where a
                                // function is defined in the first position of
                                // a list, like: ((lambda (x y) ...) args...)
                                // But in this case, we're unlikely to know for
                                // certain what the return type is,
                                // unfortunately.
                                return true;
                            }
                        }
                        return detected_type == expected_type;
                    };

                    auto arg_error = [&](int arg, u8 expected) {
                        auto tp = (ValueHeader::Type)expected;
                        return ::format(
                            "invalid arg % type for %! "
                            "expected %, got %",
                            arg,
                            val_to_string<64>(fn).c_str(),
                            type_to_string(tp),
                            val_to_string<64>(get_list(expr, arg + 1)).c_str());
                    };

                    auto push_arg_error = [&](int arg, u8 exp) {
                        push_op(make_error(arg_error(arg, exp).c_str()));
                    };

                    if (fn_var.sig_.arg0_type_ not_eq ValueHeader::Type::nil) {
                        if (not type_check(fn_var.sig_.arg0_type_, 0)) {
                            push_arg_error(0, fn_var.sig_.arg0_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg1_type_ not_eq ValueHeader::Type::nil) {
                        if (not type_check(fn_var.sig_.arg1_type_, 1)) {
                            push_arg_error(1, fn_var.sig_.arg1_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg2_type_ not_eq ValueHeader::Type::nil) {
                        if (not type_check(fn_var.sig_.arg2_type_, 2)) {
                            push_arg_error(2, fn_var.sig_.arg2_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg3_type_ not_eq ValueHeader::Type::nil) {
                        if (not type_check(fn_var.sig_.arg3_type_, 3)) {
                            push_arg_error(3, fn_var.sig_.arg3_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.ret_type_ not_eq ValueHeader::Type::nil) {
                        // TODO...
                    }
                }
            }

            auto find_variable = [&](Value* cv) {
                if (cv->type() == Value::Type::symbol) {
                    bool found = false;
                    l_foreach(variable_list, [cv, &found](Value* v) {
                        if (cv->symbol().unique_id() ==
                            v->symbol().unique_id()) {
                            found = true;
                        }
                    });

                    if (not found) {
                        auto v = get_var(cv);
                        if (v->type() == Value::Type::error) {
                            return false;
                        }
                    }
                }
                return true;
            };

            if (not is_special_form) {
                auto cv = expr->cons().car();
                if (not find_variable(cv)) {
                    push_op(make_error(::format("invalid variable access: %",
                                                val_to_string<32>(cv).c_str())
                                           .c_str()));
                    return;
                }

                if (cv->type() == Value::Type::symbol) {
                    if (str_eq(cv->symbol().name(), "set")) {
                        if (length(expr) < 3) {
                            push_op(make_error("invalid syntax in set expr"));
                            return;
                        }
                        auto set_sym = get_list(expr, 1);
                        if (set_sym->type() == Value::Type::cons) {
                            // The thing being set should be a quoted pair,
                            // unless a variable is passed to set, which is also
                            // entirely possible (and cannot easily be verified
                            // by the linter).
                            set_sym = set_sym->cons().cdr();
                            if (set_sym->type() not_eq Value::Type::symbol) {
                                push_op(make_error("setq expects symbol!"));
                                return;
                            }

                            if (not find_variable(set_sym)) {
                                push_op(make_error(
                                    ::format("set for unknown variable %",
                                             set_sym->symbol().name())
                                        .c_str()));
                                return;
                            }
                        }
                    }
                }
            }

            auto current = expr->cons().cdr();
            while (current not_eq L_NIL) {

                auto cv = current->cons().car();

                if (not find_variable(cv)) {
                    push_op(make_error(::format("invalid variable access: %",
                                                cv->symbol().name())
                                           .c_str()));
                    return;
                }

                lint(cv, variable_list);

                if (get_op0()->type() == Value::Type::error) {
                    return;
                }
                pop_op();
                current = current->cons().cdr();
            }
        }
        break;

    default:
        break;
    }

    push_op(L_NIL);
}



int error_find_linenum(CharSequence& code, int byte_offset)
{
    int current_line = 1;
    for (int j = 0; j < byte_offset; ++j) {
        if (code[j] == '\n') {
            ++current_line;
        }
    }
    while (code[byte_offset++] == '\n') {
        ++current_line;
    }
    return current_line;
}



void error_append_line_hint(Error& err, int line)
{
    Protected old_ctx = dcompr(err.context_);
    err.context_ = compr(
        L_CONS(make_string(::format("near line:%", line).c_str()), old_ctx));
}



Value* lint_code(CharSequence& code)
{
    int i = 0;
    Protected result(get_nil());

    Protected varlist = L_NIL;

    while (true) {
        const auto last_i = i;
        i += read(code, i);
        auto reader_result = get_op0();
        if (reader_result == get_nil()) {
            pop_op();
            break;
        }
        if (is_list(reader_result)) {
            auto invoke = get_list(reader_result, 0);
            if (invoke->type() == Value::Type::symbol) {
                if (str_eq(invoke->symbol().name(), "global")) {
                    l_foreach(reader_result->cons().cdr(),
                              [&varlist](Value* val) {
                                  if (val->type() == Value::Type::cons) {
                                      auto sym = val->cons().cdr();
                                      if (sym->type() == Value::Type::symbol) {
                                          varlist = L_CONS(sym, varlist);
                                      }
                                  }
                              });
                } else if (str_eq(invoke->symbol().name(), "setfn") or
                           str_eq(invoke->symbol().name(), "set-temp") or
                           str_eq(invoke->symbol().name(), "defconstant")) {
                    auto pair = get_list(reader_result, 1);
                    if (pair->type() == Value::Type::cons) {
                        auto sym = pair->cons().cdr();
                        if (sym->type() == Value::Type::symbol) {
                            varlist = L_CONS(sym, varlist);
                        }
                    }
                }
            }
        }
        lint(reader_result, varlist);
        auto expr_result = get_op0();
        result.set(expr_result);
        pop_op(); // expression result
        pop_op(); // reader result

        if (is_error(expr_result)) {
            const auto current_line = error_find_linenum(code, last_i);
            error_append_line_hint(expr_result->error(), current_line);
            expr_result->error().stacktrace_ = compr(L_NIL);
            return expr_result;
        }
    }

    return result;
}


Value* dostring(const char* code)
{
    BasicCharSequence cs(code);
    return dostring(cs, [](Value& err) {
        Platform::fatal(::format("fatal error in dostring: %",
                                 val_to_string<86>(&err).c_str()));
    });
}


Value* dostring(CharSequence& code,
                ::Function<4 * sizeof(void*), void(Value&)> on_error)
{
    ++bound_context->interp_entry_count_;

    int i = 0;

    Protected result(get_nil());

    auto prev_stk = bound_context->operand_stack_->size();

    while (true) {
        const auto last_i = i;
        i += read(code, i);
        auto reader_result = get_op0();
        if (reader_result == get_nil()) {
            pop_op();
            break;
        }
        eval(reader_result);
        auto expr_result = get_op0();
        result.set(expr_result);
        pop_op(); // expression result
        pop_op(); // reader result

        if (is_error(expr_result)) {
            const auto current_line = error_find_linenum(code, last_i);
            error_append_line_hint(expr_result->error(), current_line);
            push_op(expr_result);
            on_error(*expr_result);
            pop_op();
            break;
        }
    }

    if (bound_context->strict_ and
        bound_context->operand_stack_->size() not_eq prev_stk) {
        PLATFORM.fatal("stack spill!");
    }

    --bound_context->interp_entry_count_;

    return result;
}


const char* nameof(Value* value);


void format_impl(Value* value, Printer& p, int depth, bool skip_quotes = false)
{
    bool prefix_quote = false;

    switch ((lisp::Value::Type)value->type()) {
    case lisp::Value::Type::heap_node:
        // We should never reach here.
        PLATFORM.fatal("direct access to heap node");
        break;

    case lisp::Value::Type::nil:
        if (depth == 0) {
            p.put_str("'()");
        } else {
            p.put_str("()");
        }

        break;

    case lisp::Value::Type::__reserved_3:
    case lisp::Value::Type::__reserved_2:
    case lisp::Value::Type::__reserved_1:
    case lisp::Value::Type::__reserved_0:
        break;

    case lisp::Value::Type::wrapped: {
        auto type = dcompr(value->wrapped().type_sym_);

        auto decorator_fn =
            get_var(::format("-decorate-%", type->symbol().name()).c_str());

        if (decorator_fn->type() == Value::Type::function) {
            push_op(value); // argument
            safecall(decorator_fn, 1);
            format_impl(get_op0(), p, depth + 1, true);
            pop_op(); // result
        } else {
            Platform::fatal(::format("missing decorator function for %",
                                     type->symbol().name()));
        }
        break;
    }

    case lisp::Value::Type::string:
        if (not skip_quotes) {
            p.put_str("\"");
        }
        p.put_str(value->string().value());
        if (not skip_quotes) {
            p.put_str("\"");
        }
        break;

    case lisp::Value::Type::symbol:
        p.put_str(value->symbol().name());
        break;

    case lisp::Value::Type::fp: {
        char buffer[32];
        const char* str = float_to_string(value->fp().value_, 32, buffer);
        if (str) {
            if (value->fp().value_ < 1.f) {
                p.put_str("0");
            }
            p.put_str(str);
            bool has_decimal = false;
            while (*str not_eq '\0') {
                if (*str == '.') {
                    has_decimal = true;
                    break;
                }
                ++str;
            }
            if (not has_decimal) {
                p.put_str(".0");
            }
        }
        break;
    }

    case lisp::Value::Type::integer: {
        p.put_str(to_string<32>(value->integer().value_).c_str());
        break;
    }

    case lisp::Value::Type::cons:
        if (depth == 0 and not prefix_quote) {
            p.put_str("'");
            prefix_quote = true;
        }
        p.put_str("(");
        format_impl(value->cons().car(), p, depth + 1);
        if (value->cons().cdr()->type() == Value::Type::nil) {
            // ...
        } else if (value->cons().cdr()->type() not_eq Value::Type::cons) {
            p.put_str(" . ");
            format_impl(value->cons().cdr(), p, depth + 1);
        } else {
            auto current = value;
            while (true) {
                if (current->cons().cdr()->type() == Value::Type::cons) {
                    p.put_str(" ");
                    format_impl(
                        current->cons().cdr()->cons().car(), p, depth + 1);
                    current = current->cons().cdr();
                } else if (current->cons().cdr() not_eq get_nil()) {
                    p.put_str(" . ");
                    format_impl(current->cons().cdr(), p, depth + 1);
                    break;
                } else {
                    break;
                }
            }
        }
        p.put_str(")");
        break;

    case lisp::Value::Type::function:
        if (auto name = nameof(value)) {
            p.put_str("<fn:");
            p.put_str(name);
        } else {
            p.put_str("<lambda");
        }

        if (value->function().sig_.required_args_) {
            p.put_str(":");
            p.put_str(stringify(value->function().sig_.required_args_).c_str());
        }
        p.put_str(">");
        break;

    case lisp::Value::Type::user_data:
        p.put_str(::format("<userdata:%>", value->user_data().tag_).c_str());
        break;

    case lisp::Value::Type::error:
        p.put_str("[ERR: ");
        p.put_str(lisp::Error::get_string(value->error().code_));
        p.put_str(" : ");
        format_impl(dcompr(value->error().context_), p, 0);
        p.put_str(" ");
        format_impl(dcompr(value->error().stacktrace_), p, 0);
        p.put_str("]");
        break;

    case lisp::Value::Type::databuffer:
        p.put_str("<sbr>");
        break;

    case lisp::Value::Type::count:
        break;
    }
}


const char* String::value()
{
    switch (hdr_.mode_bits_) {
    case String::literal_string:
        return data_.literal_.value_;

    case String::memory_string:
        return dcompr(data_.memory_.databuffer_)->databuffer().value()->data_ +
               data_.memory_.offset_;

    case String::small_string:
        return data_.small_string_.data_;

    default:
        return ""; // Huh?
    }
}


String::ModeBits String::variant() const
{
    return (ModeBits)hdr_.mode_bits_;
}


void Symbol::set_name(const char* name)
{
    switch ((ModeBits)hdr_.mode_bits_) {
    case ModeBits::requires_intern:
        set_intern_name(intern(name));
        break;

    case ModeBits::stable_pointer:
        set_intern_name(name);
        break;

    case ModeBits::small: {
        char* ptr = &small_name_begin_;
        memset(ptr, '\0', buffer_size + 1);
        for (u32 i = 0; i < buffer_size; ++i) {
            if (*name not_eq '\0') {
#ifdef __GBA__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
                ptr[i] = *(name++);
#ifdef __GBA__
#pragma GCC diagnostic pop
#endif
            }
        }
        break;
    }
    }
}


void format(Value* value, Printer& p)
{
    format_impl(value, p, 0, false);
}


// Garbage Collection:
//
// Each object already contains a mark bit. We will need to trace the global
// variable table and the operand stack, and deal with all of the gc
// roots. Then, we'll need to scan through the raw slab of memory allocated
// toward each memory pool used for lisp::Value instances (not the
// freelist!). For any cell in the pool with an unset mark bit, we'll add that
// node back to the pool.


static void gc_mark_value(Value* value)
{
    if (value->hdr_.mark_bit_) {
        return;
    }

    switch (value->type()) {
    case Value::Type::wrapped:
        gc_mark_value(dcompr(value->wrapped().data_));
        gc_mark_value(dcompr(value->wrapped().type_sym_));
        break;

    case Value::Type::function:
        if (value->hdr_.mode_bits_ == Function::ModeBits::lisp_function) {
            gc_mark_value((dcompr(value->function().lisp_impl_.code_)));
            gc_mark_value(
                (dcompr(value->function().lisp_impl_.lexical_bindings_)));
        } else if (value->hdr_.mode_bits_ ==
                   Function::ModeBits::lisp_bytecode_function) {
            gc_mark_value((dcompr(value->function().bytecode_impl_.bytecode_)));
            gc_mark_value(
                (dcompr(value->function().bytecode_impl_.lexical_bindings_)));
        }
        break;

    case Value::Type::string:
        if (value->string().variant() == String::memory_string) {
            gc_mark_value(dcompr(value->string().data_.memory_.databuffer_));
        }
        break;

    case Value::Type::error:
        gc_mark_value(dcompr(value->error().context_));
        break;

    case Value::Type::cons:
        if (value->cons().cdr()->type() == Value::Type::cons) {
            auto current = value;

            while (current->cons().cdr()->type() == Value::Type::cons) {
                gc_mark_value(current->cons().car());
                current = current->cons().cdr();
                current->hdr_.mark_bit_ = true;
            }

            gc_mark_value(current->cons().car());
            gc_mark_value(current->cons().cdr());

        } else {
            gc_mark_value(value->cons().car());
            gc_mark_value(value->cons().cdr());
        }
        break;

    default:
        break;
    }

    value->hdr_.mark_bit_ = true;
}


static ProtectedBase* __protected_values = nullptr;


ProtectedBase::ProtectedBase()
{
    prev_ = nullptr;
    next_ = __protected_values;

    if (__protected_values) {
        __protected_values->prev_ = this;
    }

    __protected_values = this;
}


ProtectedBase::~ProtectedBase()
{
    if (prev_ == nullptr) {
        // We're the list head!
        __protected_values = next_;
    } else {
        prev_->next_ = next_;
    }

    if (next_) {
        next_->prev_ = prev_;
    }
}


void Protected::gc_mark()
{
    gc_mark_value(val_);
}


static void gc_mark()
{
    gc_mark_value(bound_context->nil_);
    gc_mark_value(bound_context->lexical_bindings_);
    gc_mark_value(bound_context->macros_);
    gc_mark_value(bound_context->tree_nullnode_);

    for (auto& sym : bound_context->argument_symbols_) {
        gc_mark_value(sym);
    }

#ifdef __USE_RECENT_ALLOC_CACHE__
    for (auto& v : bound_context->recent_alloc_cache_) {
        if (v) {
            gc_mark_value(v);
        }
    }
#endif

    auto& ctx = bound_context;

    for (auto elem : *ctx->operand_stack_) {
        gc_mark_value(elem);
    }

    globals_tree_traverse(ctx->globals_tree_, [](Value& car, Value& node) {
        node.hdr_.mark_bit_ = true;
        node.cons().cdr()->hdr_.mark_bit_ = true;
        gc_mark_value(&car);
    });

    gc_mark_value(ctx->callstack_);

    auto p_list = __protected_values;
    while (p_list) {
        p_list->gc_mark();
        p_list = p_list->next();
    }
}


static void invoke_finalizer(Value* value)
{
    // NOTE: This ordering should match the Value::Type enum.

    fin_table[value->type()].fn_(value);
}

void DataBuffer::finalizer(Value* buffer)
{
    reinterpret_cast<ScratchBufferPtr*>(buffer->databuffer().sbr_mem_)
        ->~ScratchBufferPtr();
}


int compact_string_memory()
{
    // The interpreter uses bump allocation when allocating strings. This can
    // cause fragmentation, and after collecting lisp objects, we should squeeze
    // the resulting gaps out of the memory region used for storing strings.

    auto& ctx = *bound_context;
    for (auto& v : *ctx.operand_stack_) {
        if (v->type() == Value::Type::string) {
            // It isn't safe to move internal string pointers around when string
            // values are currently on the stack, because a library user could
            // have a raw pointer to the string memory that we want to move.
            return 0;
        }
    }

    auto new_buffer = [] { return make_databuffer("string-memory"); };

    Protected db = new_buffer();
    u32 write_offset = 0;

    Vector<Value*> recovered_buffers;

    for (int i = 0; i < VALUE_POOL_SIZE; ++i) {
        Value* val = (Value*)&value_pool_data[i];

        if (val->hdr_.alive_ and val->type() == Value::Type::string and
            val->string().variant() == String::memory_string) {

            const auto len = strlen(val->string().value()) + 1;

            if (write_offset + len >= SCRATCH_BUFFER_SIZE) {
                write_offset = 0;
                db = new_buffer();
            }

            memcpy(db->databuffer().value()->data_ + write_offset,
                   val->string().value(),
                   len);

            auto old_buffer = dcompr(val->string().data_.memory_.databuffer_);
            if (not contains(recovered_buffers, old_buffer)) {
                recovered_buffers.push_back(old_buffer);
            }

            val->string().data_.memory_.databuffer_ = compr(db);
            val->string().data_.memory_.offset_ = write_offset;

            write_offset += len;
        }
    }

    ctx.string_buffer_ = db;
    ctx.string_buffer_remaining_ = (SCRATCH_BUFFER_SIZE - (write_offset + 1));

    for (auto& b : recovered_buffers) {
        invoke_finalizer(b);
        value_pool_free(b);
    }

    return recovered_buffers.size();

    // info(::format("compacted string memory, % total in use",
    //               string_bytes_total));
}


static int gc_sweep()
{
    if (not bound_context->string_buffer_->hdr_.mark_bit_) {
        bound_context->string_buffer_ = L_NIL;
        bound_context->string_buffer_remaining_ = 0;
    }

    int collect_count = 0;
    u32 used_count = 0;

    for (int i = 0; i < VALUE_POOL_SIZE; ++i) {

        Value* val = (Value*)&value_pool_data[i];

        if (val->hdr_.alive_) {
            if (val->hdr_.mark_bit_) {
                val->hdr_.mark_bit_ = false;
                ++used_count;
            } else {
                invoke_finalizer(val);
                value_pool_free(val);
                ++collect_count;
            }
        }
    }

    bound_context->callstack_untouched_ = true;

    if (used_count > bound_context->alloc_highwater_) {
        bound_context->alloc_highwater_ = used_count;
        info(::format("LISP mem %", used_count));
    }

    collect_count += compact_string_memory();

    return collect_count;
}


void live_values(::Function<6 * sizeof(void*), void(Value&)> callback)
{
    for (int i = 0; i < VALUE_POOL_SIZE; ++i) {

        Value* val = (Value*)&value_pool_data[i];

        if (val->hdr_.alive_) {
            callback(*val);
        }
    }
}


static int run_gc()
{
    return gc_mark(), gc_sweep();
}


using EvalBuffer = StringBuffer<900>;


namespace
{
class EvalPrinter : public Printer
{
public:
    EvalPrinter(EvalBuffer& buffer) : buffer_(buffer)
    {
    }

    void put_str(const char* str) override
    {
        buffer_ += str;
    }

private:
    EvalBuffer& buffer_;
};
} // namespace



template <typename F> void foreach_string_intern(F&& fn)
{
    auto& ctx = bound_context;

    if (ctx->external_symtab_contents_) {
        const char* search = ctx->external_symtab_contents_;
        for (u32 i = 0; i < ctx->external_symtab_size_;) {
            fn(search + i);
            i += 32;
        }
    }

    if (ctx->string_intern_table_) {
        char* const interns = (*ctx->string_intern_table_)->data_;
        char* str = interns;

        while (static_cast<u32>(str - interns) < string_intern_table_size and
               static_cast<s32>(str - interns) < ctx->string_intern_pos_ and
               *str not_eq '\0') {

            fn(str);

            str += strlen(str) + 1;
        }
    }
}



static void
push_reader_error(CharSequence& code, int byte_offset, Error::Code ec)
{
    Protected err = make_error(ec, L_NIL);

    const auto current_line = error_find_linenum(code, byte_offset);
    error_append_line_hint(err->error(), current_line);

    push_op(err);
}



static u32 read_list(CharSequence& code, int offset)
{
    int i = 0;

    auto result = get_nil();
    push_op(get_nil());

    bool dotted_pair = false;

    while (true) {
        switch (code[offset + i]) {
        case '\r':
        case '\n':
        case '\t':
        case '\v':
        case ' ':
            ++i;
            break;

        case '.':
            if (code[offset + i + 1] >= '0' and code[offset + i + 1] <= '9') {
                goto DEFAULT;
            } else {
                i += 1;
                if (dotted_pair or result == get_nil()) {
                    push_reader_error(
                        code, i, Error::Code::mismatched_parentheses);
                    return i;
                } else {
                    dotted_pair = true;
                    i += read(code, offset + i);
                    result->cons().set_cdr(get_op0());
                    pop_op();
                }
            }
            break;

        case ';':
            while (true) {
                if (code[offset + i] == '\0' or code[offset + i] == '\r' or
                    code[offset + i] == '\n') {
                    break;
                } else {
                    ++i;
                }
            }
            break;

        case ']':
        case ')':
            ++i;
            return i;

        case '\0':
            pop_op();
            push_reader_error(code, i, Error::Code::mismatched_parentheses);
            return i;
            break;

        default:
        DEFAULT:
            if (dotted_pair) {
                push_reader_error(code, i, Error::Code::mismatched_parentheses);
                return i;
            }
            i += read(code, offset + i);

            if (result == get_nil()) {
                result = make_cons(get_op0(), get_nil());
                pop_op(); // the result from read()
                pop_op(); // nil
                push_op(result);
            } else {
                auto next = make_cons(get_op0(), get_nil());
                pop_op();
                result->cons().set_cdr(next);
                result = next;
            }
            break;
        }
    }
}


static u32 read_string(CharSequence& code, int offset)
{
    auto temp = make_scratch_buffer("lisp-string-memory");
    auto write = temp->data_;

    int i = 0;
    while (true) {
        const auto current = code[offset + i];

        if (current == '"') {
            break;
        }

        if (current == '\0' or i == SCRATCH_BUFFER_SIZE - 1) {
            // FIXME: correct error code.
            push_reader_error(code, i, Error::Code::mismatched_parentheses);
            return i;
        }

        // UTF-8. We need special parsing, in case a utf-8 sequence contains a "
        // character.
        const Bitvector<8> parsed(current);
        if ((current & 0x80) == 0) {
            *(write++) = code[offset + i++];
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 0) {
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 1 and
                   parsed[4] == 0) {
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
        } else if (parsed[7] == 1 and parsed[6] == 1 and parsed[5] == 1 and
                   parsed[4] == 1 and parsed[3] == 0) {
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
            *(write++) = code[offset + i++];
        }
    }

    if (code[offset + i] == '"') {
        ++i;
    }

    *write = '\0';
    push_op(make_string(temp->data_));

    return i;
}


static u32 read_symbol(CharSequence& code, int offset)
{
    int i = 0;

    StringBuffer<64> symbol;

    if (code[offset] == '\'' or code[offset] == '`' or code[offset] == ',' or
        code[offset] == '@') {
        symbol.push_back(code[offset]);

        auto mode = Symbol::ModeBits::requires_intern;
        if (symbol.length() <= Symbol::buffer_size) {

#ifndef __GBA__
            char* id = 0;
            for (u32 i = 0; i < symbol.length(); ++i) {
                ((u8*)&id)[i] = symbol[i];
            }

            if (bound_context->string_intern_table_ and
                id >= (*bound_context->string_intern_table_)->data_ and
                id < (*bound_context->string_intern_table_)->data_ +
                         string_intern_table_size) {
                // Do not perform small symbol optimization, because the name,
                // when interpreted as a pointer, falls in the range of the
                // symbol intern table.

            } else {
                mode = Symbol::ModeBits::small;
            }
#else
            // __GBA__
            // NOTE: the above code prevents small symbol optimizations from
            // colliding with the address space of the symbol intern table. But
            // on the GBA specifically, EWRAM is known to be mapped to a block
            // of memory starting at 0x03000000. Given that the gba 0x03 is a
            // nonprintable ascii ETX control code, no need to worry about
            // address collisions.
            mode = Symbol::ModeBits::small;
#endif
        }

        push_op(make_symbol(symbol.c_str(), mode));
        return 1;
    }

    while (true) {
        switch (code[offset + i]) {
        case '[':
        case ']':
        case '(':
        case ')':
        case ' ':
        case '\r':
        case '\n':
        case '\t':
        case '\v':
        case '\0':
        case ';':
        case '"':
        case '.':
        case '\'':
            goto FINAL;

        default:
            symbol.push_back(code[offset + i++]);
            break;
        }
    }

FINAL:

    if (symbol == "nil" or symbol == "false") {
        push_op(get_nil());
    } else if (symbol == "true") {
        push_op(make_integer(1));
    } else {
        auto mode = Symbol::ModeBits::requires_intern;
        if (symbol.length() <= Symbol::buffer_size) {
            mode = Symbol::ModeBits::small;
        }
        push_op(make_symbol(symbol.c_str(), mode));
    }

    return i;
}


static u32 read_number(CharSequence& code, int offset)
{
    int i = 0;

    StringBuffer<64> num_str;

    bool is_fp = false;

    while (true) {
        switch (code[offset + i]) {
        case 'x':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            num_str.push_back(code[offset + i++]);
            break;

        case '.': {
            if (is_fp) {
                // Two decimal places don't make sense. Must be a dotted pair...
                goto FINAL;
            }
            auto next = code[offset + i + 1];
            if (next >= '0' and next <= '9') {
                is_fp = true;
                num_str.push_back(code[offset + i++]);
            } else {
                goto FINAL;
            }
            break;
        }

        default:
            goto FINAL;
        }
    }

FINAL:

    if (is_fp) {
        push_op(L_FP(atof(num_str.c_str())));
    } else if (num_str.length() > 1 and num_str[1] == 'x') {
        push_op(make_integer(hexdec((const u8*)num_str.begin() + 2)));
    } else {
        s32 result = 0;
        for (u32 i = 0; i < num_str.length(); ++i) {
            result = result * 10 + (num_str[i] - '0');
        }

        push_op(make_integer(result));
    }

    return i;
}


static void eval_let(Value* code);


static void macroexpand();


// Argument: list on operand stack
// result: list on operand stack
static void macroexpand_macro()
{
    // Ok, so this warrants some explanation: When calling this function, we've
    // just expanded a macro, but the macro expansion itself may contain macros,
    // so we'll want to iterate through the expanded expression and expand any
    // nested macros.

    // FIXME: If the macro has no nested macro expressions, we create a whole
    // bunch of garbage. Not an actual issue, but puts pressure on the gc.

    ListBuilder result;

    // Macroexpand top level (the original list), in case the result of the
    // prior macroexpand introduced a new macro in the first position of the
    // outermost list.
    macroexpand();

    auto lat = get_op0();
    for (; lat not_eq get_nil(); lat = lat->cons().cdr()) {
        auto car_val = lat->cons().car();
        if (is_list(car_val)) {
            push_op(car_val);
            macroexpand_macro();
            macroexpand();
            result.push_back(get_op0());
            pop_op();
        } else {
            result.push_back(car_val);
        }
    }

    pop_op();
    push_op(result.result());
}


// Argument: list on operand stack
// result: list on operand stack
static void macroexpand()
{
    // NOTE: I know this function looks complicated. But it's really not too
    // bad.

    auto lat = get_op0();

    if (lat->cons().car()->type() == Value::Type::symbol) {

        auto macros = bound_context->macros_;
        for (; macros not_eq get_nil(); macros = macros->cons().cdr()) {

            // if Symbol matches?
            if (macros->cons().car()->cons().car()->symbol().unique_id() ==
                lat->cons().car()->symbol().unique_id()) {

                auto supplied_macro_args = lat->cons().cdr();

                auto macro = macros->cons().car()->cons().cdr();
                auto macro_args = macro->cons().car();

                if (length(macro_args) > length(supplied_macro_args)) {
                    pop_op();
                    push_op(make_error(Error::Code::invalid_syntax,
                                       make_string("invalid arguments "
                                                   "passed to macro")));
                    return;
                }

                Protected quote(make_symbol("'", Symbol::ModeBits::small));

                // Ok, so I should explain what's going on here. For code reuse
                // purposes, we basically generate a let expression from the
                // macro parameter list, which binds the quoted macro arguments
                // to the unevaluated macro parameters.
                //
                // So, (macro foo (a b c) ...),
                // instantiated as (foo (+ 1 2) 5 6) becomes:
                //
                // (let ((a '(+ 1 2)) (b '5) (c '(6))) ...)
                //
                // Then, we just eval the let expression.
                // NOTE: The final macro argument will _always_ be a list. We
                // need to allow for variadic arguments in macro expressions,
                // and for the sake of generality, we may as well make the final
                // parameter in a macro a list in all cases, if it will need to
                // be a list in some cases.

                ListBuilder builder;
                while (macro_args not_eq get_nil()) {

                    ListBuilder assoc;

                    if (macro_args->cons().cdr() == get_nil()) {

                        assoc.push_front(make_cons(quote, supplied_macro_args));

                    } else {

                        assoc.push_front(make_cons(
                            quote, supplied_macro_args->cons().car()));
                    }

                    assoc.push_front(macro_args->cons().car());
                    builder.push_back(assoc.result());

                    macro_args = macro_args->cons().cdr();
                    supplied_macro_args = supplied_macro_args->cons().cdr();
                }

                ListBuilder synthetic_let;
                synthetic_let.push_front(macro->cons().cdr()->cons().car());
                synthetic_let.push_front(builder.result());

                eval_let(synthetic_let.result());

                auto result = get_op0();
                pop_op(); // result of eval_let()
                pop_op(); // input list

                push_op(result);

                // OK, so... we want to allow users to recursively instantiate
                // macros, so we aren't done!
                macroexpand_macro();
                return;
            } else {
                // ... no match ...
            }
        }
    }
}


static void negate_number(Value* v)
{
    if (v->type() == Value::Type::fp) {
        v->fp().value_ *= -1;
    } else {
        v->integer().value_ *= -1;
    }
}


u32 read(CharSequence& code, int offset)
{
    int i = 0;

    push_op(get_nil());

    while (true) {
        switch (code[offset + i]) {
        case '\0':
            return i;

        case '[':
        case '(': {
            ++i;
            pop_op(); // nil
            i += read_list(code, offset + i);
            macroexpand();
            // list now at stack top.
            return i;
        }

        case ';':
            while (true) {
                if (code[offset + i] == '\0' or code[offset + i] == '\r' or
                    code[offset + i] == '\n') {
                    break;
                } else {
                    ++i;
                }
            }
            break;

        case '.':
            if (code[offset + i + 1] >= '0' and code[offset + i + 1] <= '9') {
                pop_op(); // nil
                i += read_number(code, offset + i);
                return i;
            } else {
                goto READ_SYMBOL;
            }
            break;

        case '-':
            if (code[offset + i + 1] >= '0' and code[offset + i + 1] <= '9') {
                ++i;
                pop_op(); // nil
                i += read_number(code, offset + i);
                negate_number(get_op0());
                return i;
            } else {
                if (code[offset + i + 1] == '.' and
                    code[offset + i + 2] >= '0' and
                    code[offset + i + 2] <= '9') {
                    ++i;
                    pop_op(); // nil
                    i += read_number(code, offset + i);
                    negate_number(get_op0());
                    return i;
                }
                goto READ_SYMBOL;
            }
            break;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            pop_op(); // nil
            i += read_number(code, offset + i);
            // number now at stack top.
            return i;

        case '\n':
        case '\r':
        case '\v':
        case '\t':
        case ' ':
            ++i;
            break;

        case '"':
            pop_op(); // nil
            i += read_string(code, offset + i + 1);
            return i + 1;


        READ_SYMBOL:
        default:
            pop_op(); // nil
            i += read_symbol(code, offset + i);
            // symbol now at stack top.

            // Ok, so for quoted expressions, we're going to put the value into
            // a cons, where the car holds the quote symbol, and the cdr holds
            // the value. Not sure how else to support top-level quoted
            // values outside of s-expressions.
            if (get_op0()->type() == Value::Type::symbol and
                (str_cmp(get_op0()->symbol().name(), "'") == 0 or
                 str_cmp(get_op0()->symbol().name(), "`") == 0)) {

                auto pair = make_cons(get_op0(), get_nil());
                push_op(pair);
                i += read(code, offset + i);
                pair->cons().set_cdr(get_op0());
                pop_op(); // result of read()
                pop_op(); // pair
                pop_op(); // symbol
                push_op(pair);
            }
            return i;
        }
    }
}


static void eval_while(Value* code)
{
    Protected result(get_nil());

    if (code->type() not_eq Value::Type::cons) {
        push_op(lisp::make_error(Error::Code::mismatched_parentheses, L_NIL));
        return;
    }

    auto cond = code->cons().car();

    while (true) {

        eval(cond);
        auto test = get_op0();
        pop_op();

        if (not is_boolean_true(test)) {
            break;
        }

        l_foreach(code->cons().cdr(), [&](Value* val) {
            eval(val);
            result.set(get_op0());
            pop_op();
        });
    }

    push_op(result);
}


static void eval_let(Value* code)
{
    // Overview:
    // Push the previous values of all of the let binding vars onto the stack.
    // Overwrite the current contents of the global vars. Pop the previous
    // contents off of the operand stack, and re-assign the var to the stashed
    // value.

    if (code->type() not_eq Value::Type::cons) {
        push_op(lisp::make_error(Error::Code::mismatched_parentheses, L_NIL));
        return;
    }

    Value* bindings = code->cons().car();

    Protected result(get_nil());

    bool has_bindings = false;

    {
        ListBuilder binding_list_builder;

        l_foreach(bindings, [&](Value* val) {
            if (result not_eq get_nil()) {
                return;
            }
            if (val->type() == Value::Type::cons) {
                auto sym = val->cons().car();
                auto bind = val->cons().cdr();
                if (sym->type() == Value::Type::symbol and
                    bind->type() == Value::Type::cons) {

                    eval(bind->cons().car());
                    binding_list_builder.push_back(make_cons(sym, get_op0()));

                    has_bindings = true;

                    pop_op();

                } else {
                    result = lisp::make_error(
                        Error::Code::mismatched_parentheses, L_NIL);
                }
            } else {
                result = lisp::make_error(Error::Code::mismatched_parentheses,
                                          L_NIL);
            }
        });

        if (result not_eq get_nil()) {
            push_op(result);
            return;
        }

        if (has_bindings) {
            auto new_binding_list = make_cons(binding_list_builder.result(),
                                              bound_context->lexical_bindings_);

            if (is_error(new_binding_list)) {
                push_op(new_binding_list);
                return;
            } else {
                bound_context->lexical_bindings_ = new_binding_list;
            }
        }
    }

    l_foreach(code->cons().cdr(), [&](Value* val) {
        eval(val);
        result.set(get_op0());
        pop_op();
    });

    if (has_bindings) {
        bound_context->lexical_bindings_ =
            bound_context->lexical_bindings_->cons().cdr();
    }

    push_op(result);
}


static void eval_macro(Value* code)
{
    if (code->cons().car()->type() == Value::Type::symbol) {
        bound_context->macros_ = make_cons(code, bound_context->macros_);
        push_op(get_nil());
    } else {
        // TODO: raise error!
        PLATFORM.fatal("invalid macro format");
    }
}


static void eval_defconstant(Value* code)
{
    if (code->cons().car()->type() not_eq lisp::Value::Type::symbol) {
        // FIXME!
        PLATFORM.fatal("invalid defconstant syntax!");
    }

    if (bound_context->external_constant_tab_) {
        // We will load the value from our constant table, so there's no need to
        // do anything when we encounter a defconstant line.
    } else {
        // We don't have an external constant table defined. Instead, we'll need
        // to load the variable into memory.
        eval(code->cons().cdr()->cons().car()); // The constant value...

        // TODO: load and check for a precomputed constant table, and don't
        // store values in memory.
        set_var(code->cons().car(), get_op0(), true);

        pop_op(); // eval result.
    }

    push_op(get_nil());
}


static void eval_if(Value* code)
{
    if (code->type() not_eq Value::Type::cons) {
        push_op(lisp::make_error(Error::Code::mismatched_parentheses, L_NIL));
        return;
    }

    auto cond = code->cons().car();

    auto true_branch = get_nil();
    auto false_branch = get_nil();

    if (code->cons().cdr()->type() == Value::Type::cons) {
        true_branch = code->cons().cdr()->cons().car();

        if (code->cons().cdr()->cons().cdr()->type() == Value::Type::cons) {
            false_branch = code->cons().cdr()->cons().cdr()->cons().car();
        }
    }

    eval(cond);
    if (is_boolean_true(get_op0())) {
        eval(true_branch);
    } else {
        eval(false_branch);
    }

    auto result = get_op0();
    pop_op(); // result
    pop_op(); // cond
    push_op(result);
}


static void eval_function(Value* code)
{
    push_op(make_lisp_function(code));
}


static void eval_argumented_function(Value* code)
{
    push_op(make_lisp_argumented_function(code));
}


static void eval_quasiquote(Value* code)
{
    ListBuilder builder;

    while (code not_eq get_nil()) {
        if (code->cons().car()->type() == Value::Type::symbol and
            str_cmp(code->cons().car()->symbol().name(), ",") == 0) {

            code = code->cons().cdr();

            if (code == get_nil()) {
                push_op(make_error(Error::Code::invalid_syntax,
                                   make_string("extraneous unquote")));
                return;
            }

            if (code->cons().car()->type() == Value::Type::symbol and
                str_cmp(code->cons().car()->symbol().name(), "@") == 0) {

                code = code->cons().cdr(); // skip over @ symbol

                eval(code->cons().car());
                auto result = get_op0();

                if (is_list(result)) {
                    // Quote splicing
                    while (result not_eq get_nil()) {
                        builder.push_back(result->cons().car());
                        result = result->cons().cdr();
                    }
                } else {
                    builder.push_back(result);
                }

                pop_op(); // result

            } else {
                eval(code->cons().car());
                auto result = get_op0();
                pop_op();

                builder.push_back(result);
            }

        } else {
            if (is_list(code->cons().car())) {
                // NOTE: because we need to expand unquotes in nested lists.
                eval_quasiquote(code->cons().car());
                builder.push_back(get_op0());
                pop_op();
            } else {
                builder.push_back(code->cons().car());
            }
        }

        code = code->cons().cdr();
    }

    push_op(builder.result());
}




#if defined(__GBA__) or defined(__APPLE__)
#define STANDARD_FORMS                                                      \
    MAPBOX_ETERNAL_CONSTEXPR const auto standard_forms =                    \
        mapbox::eternal::hash_map<mapbox::eternal::string, EvalCB>
#else
#define STANDARD_FORMS                                                  \
    const auto standard_forms = std::unordered_map<std::string, EvalCB>
#endif


using EvalCB = void(*)(Value*);
// clang-format off
STANDARD_FORMS({
        {"if", [](Value* code) {
            eval_if(code->cons().cdr());
            auto result = get_op0();
            pop_op(); // result
            pop_op(); // code
            push_op(result);
            --bound_context->interp_entry_count_;
        }},
        {"lambda", [](Value* code) {
            eval_argumented_function(code->cons().cdr());
            auto result = get_op0();
            pop_op(); // result
            pop_op(); // code
            push_op(result);
            --bound_context->interp_entry_count_;
            return;
        }},
        {"fn", [](Value* code) {
            eval_function(code->cons().cdr());
            auto result = get_op0();
            pop_op(); // result
            pop_op(); // code
            push_op(result);
            --bound_context->interp_entry_count_;
        }},
        {"'", [](Value* code) {
            pop_op(); // code
            push_op(code->cons().cdr());
            --bound_context->interp_entry_count_;
        }},
        {"`", [](Value* code) {
            eval_quasiquote(code->cons().cdr());
            auto result = get_op0();
            pop_op(); // result
            pop_op(); // code
            push_op(result);
            --bound_context->interp_entry_count_;
        }},
        {"let", [](Value* code) {
            eval_let(code->cons().cdr());
            auto result = get_op0();
            pop_op();
            pop_op();
            push_op(result);
            --bound_context->interp_entry_count_;
        }},
        {"macro", [](Value* code) {
            eval_macro(code->cons().cdr());
            pop_op();
            // TODO: store macro!
            --bound_context->interp_entry_count_;
        }},
        {"while", [](Value* code) {
            eval_while(code->cons().cdr());
            auto result = get_op0();
            pop_op();
            pop_op();
            push_op(result);
        }},
        {"defconstant", [](Value* code) {
            eval_defconstant(code->cons().cdr());
            auto result = get_op0();
            pop_op();
            pop_op();
            push_op(result);
        }}
    });



void eval(Value* code)
{
    ++bound_context->interp_entry_count_;

    // NOTE: just to protect this from the GC, in case the user didn't bother to
    // do so.
    push_op(code);

    if (code->type() == Value::Type::symbol) {
        pop_op();
        push_op(get_var(code));
    } else if (code->type() == Value::Type::cons) {
        auto form = code->cons().car();
        if (form->type() == Value::Type::symbol) {
            auto std_form = standard_forms.find(form->symbol().name());
            if (std_form not_eq standard_forms.end()) {
                std_form->second(code);
                return;
            }
        }

        eval(code->cons().car());
        auto function = get_op0();
        pop_op();

        int argc = 0;

        auto clear_args = [&] {
            while (argc) {
                pop_op();
                --argc;
            }
        };

        auto arg_list = code->cons().cdr();
        while (true) {
            if (arg_list == get_nil()) {
                break;
            }
            if (arg_list->type() not_eq Value::Type::cons) {
                clear_args();
                pop_op();
                push_op(make_error(Error::Code::value_not_callable, arg_list));
                --bound_context->interp_entry_count_;
                return;
            }

            eval(arg_list->cons().car());
            ++argc;

            arg_list = arg_list->cons().cdr();
        }

        funcall(function, argc);
        auto result = get_op0();
        if (result->type() == Value::Type::error and
            dcompr(result->error().context_) == L_NIL) {
            result->error().context_ = compr(code);
        }
        pop_op(); // result
        pop_op(); // protected expr (see top)
        push_op(result);
        --bound_context->interp_entry_count_;
        return;
    }
}


Platform* interp_get_pfrm()
{
    return &PLATFORM;
}


bool is_equal(Value* lhs, Value* rhs)
{
    if (lhs->type() not_eq rhs->type()) {
        return false;
    }

    switch (lhs->type()) {
    case Value::Type::integer:
        return lhs->integer().value_ == rhs->integer().value_;

    case Value::Type::fp:
        return lhs->fp().value_ == rhs->fp().value_;

    case Value::Type::cons:
        if (is_list(lhs)) {
            if (is_list(rhs)) {
                if (length(lhs) not_eq length(rhs)) {
                    return false;
                }
                auto l = lhs;
                auto r = rhs;
                while (l not_eq L_NIL and r not_eq L_NIL) {
                    if (not is_equal(l->cons().car(), r->cons().car())) {
                        return false;
                    }
                    l = l->cons().cdr();
                    r = r->cons().cdr();
                }
                return true;
            } else {
                return false;
            }
        }
        return is_equal(lhs->cons().car(), rhs->cons().car()) and
               is_equal(lhs->cons().cdr(), rhs->cons().cdr());

    case Value::Type::count:
    case Value::Type::__reserved_3:
    case Value::Type::__reserved_2:
    case Value::Type::__reserved_1:
    case Value::Type::__reserved_0:
    case Value::Type::nil:
    case Value::Type::heap_node:
    case Value::Type::databuffer:
        return lhs == rhs;

    case Value::Type::wrapped: {
        if (not str_eq(dcompr(lhs->wrapped().type_sym_)->symbol().name(),
                       dcompr(rhs->wrapped().type_sym_)->symbol().name())) {
            return false;
        }
        auto type = dcompr(lhs->wrapped().type_sym_);
        auto equal_fn =
            get_var(::format("-equal-%", type->symbol().name()).c_str());

        if (equal_fn->type() == Value::Type::function) {
            push_op(rhs);
            push_op(lhs);
            safecall(equal_fn, 2);
            auto ret = is_boolean_true(get_op0());
            pop_op(); // result
            return ret;
        } else {
            Platform::fatal(::format("missing equal function for %",
                                     type->symbol().name()));
        }
        break;
    }

    case Value::Type::function:
        if (lhs->hdr_.mode_bits_ not_eq rhs->hdr_.mode_bits_) {
            return false;
        }
        switch (lhs->hdr_.mode_bits_) {
        default:
            return lhs == rhs;

        case Function::ModeBits::cpp_function:
            return lhs->function().cpp_impl_ == rhs->function().cpp_impl_;
        }
        break;

    case Value::Type::error:
        break;

    case Value::Type::symbol:
        return lhs->symbol().unique_id() == rhs->symbol().unique_id();

    case Value::Type::user_data:
        return lhs->user_data().obj_ == rhs->user_data().obj_;

    case Value::Type::string:
        return str_cmp(lhs->string().value(), rhs->string().value()) == 0;
    }
    return false;
}


Value* gensym()
{
    auto& ctx = bound_context;

    char gen[5];
    gen[0] = '#';
    gen[4] = '\0';

    const char* alphabet = "abcdefghijklmnopqrstuvwxyz"
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           "1234567890-_";

    // Explanation:
    // We do an internal optimization for four-character strings, so we use a
    // four-char string when generating symbols. Keep a counter and represent
    // each of the three slots in the gensym output with one of the printable
    // ascii characters, prefixed by a # to reduce likelihood of collision with
    // a user-supplied symbol.
    if (ctx->gensym_state_.char_1_ == 63) {
        // Carry
        ctx->gensym_state_.char_1_ = 0;
        if (ctx->gensym_state_.char_2_ == 63) {
            // Carry
            ctx->gensym_state_.char_2_ = 0;
            if (ctx->gensym_state_.char_3_ == 63) {
                // Wrap! But at this point, we've run through quite a large
                // number of generated symbols, i.e. a macro would need to be
                // quite huge for wrapping to cause a collision in gensym
                // output.
                ctx->gensym_state_.char_1_ = 0;
                ctx->gensym_state_.char_2_ = 0;
                ctx->gensym_state_.char_3_ = 0;
            } else {
                ++ctx->gensym_state_.char_3_;
            }
        } else {
            ++ctx->gensym_state_.char_2_;
        }
    } else {
        ++ctx->gensym_state_.char_1_;
    }

    gen[1] = alphabet[ctx->gensym_state_.char_1_];
    gen[2] = alphabet[ctx->gensym_state_.char_2_];
    gen[3] = alphabet[ctx->gensym_state_.char_3_];

    return make_symbol(gen);
}


struct ConstantTabEntryHeader
{
    u8 field_size_;
    u8 value_size_;
};


void apropos(const char* match, Vector<const char*>& completion_strs)
{
    StringBuffer<16> ident(match);

    auto handle_completion = [&ident, &completion_strs](const char* intern) {
        const auto intern_len = strlen(intern);
        if (intern_len <= ident.length()) {
            // I mean, there's no reason to autocomplete
            // to something shorter or the same length...
            return;
        }

        for (u32 i = 0; i < ident.length() and i < intern_len; ++i) {
            if (ident[i] not_eq intern[i]) {
                return;
            }
        }

        for (auto& str : completion_strs) {
            if (str == intern) {
                return;
            }
        }

        completion_strs.push_back(intern);
    };

    get_env(handle_completion);
    get_interns(handle_completion);

    if (bound_context->external_constant_tab_) {
        u32 i = 0;
        for (; i < bound_context->external_constant_tab_size_;) {
            auto ptr = bound_context->external_constant_tab_ + i;
            u8 field_size = ((ConstantTabEntryHeader*)ptr)->field_size_;
            u8 value_size = ((ConstantTabEntryHeader*)ptr)->value_size_;
            const char* name = ptr + sizeof(ConstantTabEntryHeader);
            handle_completion(name);
            i += sizeof(ConstantTabEntryHeader) + field_size + value_size;
        }
    }
}


static bool contains(Value* list, Value* val)
{
    bool result = false;
    l_foreach(list, [&](Value* v2) {
        if (is_equal(val, v2)) {
            result = true;
        }
    });

    return result;
}


const char* nameof(Function::CPP_Impl impl);


Value* stacktrace()
{
    bound_context->callstack_untouched_ = false;
    return bound_context->callstack_->cons().cdr();
}



bool comp_less_than(Value* lhs, Value* rhs)
{
    if (lhs->type() == Value::Type::fp) {
        return lhs->fp().value_ < rhs->fp().value_;
    }
    return lhs->integer().value_ < rhs->integer().value_;
}



Value* l_comp_less_than(int argc)
{
    if (get_op0()->type() == Value::Type::fp) {
        L_EXPECT_OP(1, fp);
        return make_integer(comp_less_than(get_op1(), get_op0()));
    }
    L_EXPECT_OP(0, integer);
    L_EXPECT_OP(1, integer);
    return make_integer(comp_less_than(get_op1(), get_op0()));
}


Value* repr_signature(Function::Signature sig)
{
    ListBuilder list;
    list.push_back(L_SYM(type_to_string((Value::Type)sig.ret_type_)));
    list.push_back(L_SYM(type_to_string((Value::Type)sig.arg0_type_)));
    list.push_back(L_SYM(type_to_string((Value::Type)sig.arg1_type_)));
    list.push_back(L_SYM(type_to_string((Value::Type)sig.arg2_type_)));
    list.push_back(L_SYM(type_to_string((Value::Type)sig.arg3_type_)));
    return list.result();
}


#if defined(__GBA__) or defined(__APPLE__)
#define BUILTIN_TABLE                                                          \
    MAPBOX_ETERNAL_CONSTEXPR const auto builtin_table =                        \
        mapbox::eternal::hash_map<mapbox::eternal::string, Builtin>
#else
#define BUILTIN_TABLE                                                          \
    const auto builtin_table = std::unordered_map<std::string, Builtin>
#endif



using RequiredArgc = int;
using Builtin = std::pair<Function::Signature, lisp::Value* (*)(int)>;
// clang-format off
BUILTIN_TABLE(
    // clang-format on
    {{"set",
      {SIG2(nil, symbol, nil),
       [](int argc) {
           L_EXPECT_OP(1, symbol);

           if (is_error(get_op0())) {
               return get_op0();
           }

           bool define_if_missing = not bound_context->strict_;

           return lisp::set_var(get_op1(), get_op0(), define_if_missing);
       }}},
     {"global",
      {SIG1(nil, symbol),
       [](int argc) {
           for (int i = 0; i < argc; ++i) {
               L_EXPECT_OP(i, symbol);
               if (__find_local(get_op(i)->symbol().name())) {
                   Protected str(make_string(
                       ::format("cannot declare global % due to existing local",
                                get_op(i)->symbol().name())
                           .c_str()));
                   return make_error(Error::Code::invalid_syntax, str);
               }
               if (not globals_tree_find(get_op(i))) {
                   set_var(get_op(i), L_NIL, true);
               }
           }
           return get_op0();
       }}},
     {"strict-mode",
      {SIG1(nil, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           bound_context->strict_ = L_LOAD_INT(0);
           return L_NIL;
       }}},
     {"signature",
      {SIG1(cons, function),
       [](int argc) {
           L_EXPECT_OP(0, function);
           auto sig = get_op0()->function().sig_;
           return repr_signature(sig);
       }}},
     {"require-args",
      {SIG2(function, function, integer),
       [](int argc) {
           L_EXPECT_OP(1, function);
           L_EXPECT_OP(0, integer);

           lisp::Protected result(L_NIL);
           result = alloc_value();
           result->function() = get_op1()->function();
           result->function().sig_.required_args_ = L_LOAD_INT(0);

           return (Value*)result;
       }}},
     {"rot13",
      {SIG1(string, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           auto str = L_LOAD_STRING(0);
           auto rotstr = allocate_dynamic<StringBuffer<1000>>("rot13");
           while (*str not_eq '\0') {
               rotstr->push_back(rot13(*str));
               ++str;
           }
           return lisp::make_string(rotstr->c_str());
       }}},
     {"cons",
      {SIG2(cons, nil, nil),
       [](int argc) {
           auto car = get_op1();
           auto cdr = get_op0();

           if (car->type() == lisp::Value::Type::error) {
               return car;
           }

           if (cdr->type() == lisp::Value::Type::error) {
               return cdr;
           }

           return make_cons(get_op1(), get_op0());
       }}},
     {"car",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().car();
       }}},
     {"first",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().car();
       }}},
     {"identity", {SIG1(nil, nil), [](int argc) { return get_op0(); }}},
     {"cddr",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           auto cdr = get_op0()->cons().cdr();
           if (cdr->type() not_eq Value::Type::cons) {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
           return cdr->cons().cdr();
       }}},
     {"cadr",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           auto cdr = get_op0()->cons().cdr();
           if (cdr->type() not_eq Value::Type::cons) {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
           return cdr->cons().car();
       }}},
     {"cdar",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           auto car = get_op0()->cons().car();
           if (car->type() not_eq Value::Type::cons) {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
           return car->cons().cdr();
       }}},
     {"caar",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           auto car = get_op0()->cons().car();
           if (car->type() not_eq Value::Type::cons) {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
           return car->cons().car();
       }}},
     {"cdr",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"second",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"rest",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"list",
      {SIG0(cons),
       [](int argc) {
           ListBuilder list;
           for (int i = 0; i < argc; ++i) {
               auto val = get_op((argc - 1) - i);
               if (val->type() == Value::Type::error) {
                   return val;
               }
               list.push_back(val);
           }
           return list.result();
       }}},
     {"split",
      {SIG2(list, string, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           L_EXPECT_OP(1, string);

           const char delim = *L_LOAD_STRING(0);
           auto str = L_LOAD_STRING(1);

           ListBuilder b;

           StringBuffer<96> temp;

           while (*str not_eq '\0') {
               if (*str == delim) {
                   b.push_back(make_string(temp.c_str()));
                   temp.clear();
               } else {
                   temp.push_back(*str);
               }
               ++str;
           }

           if (not temp.empty()) {
               b.push_back(make_string(temp.c_str()));
           }

           return b.result();
       }}},
     {"arg",
      {EMPTY_SIG(1),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return get_arg(get_op0()->integer().value_);
       }}},
     {"abs",
      {SIG1(integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return make_integer(abs(L_LOAD_INT(0)));
       }}},
     {"not",
      {EMPTY_SIG(1),
       [](int argc) { return make_integer(not is_boolean_true(get_op0())); }}},
     {"equal",
      {EMPTY_SIG(2),
       [](int argc) { return make_integer(is_equal(get_op0(), get_op1())); }}},
     {"boolean?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean((get_op0()->type() == Value::Type::integer and
                                (get_op0()->integer().value_ == 1 or
                                 get_op0()->integer().value_ == 0)) or
                               get_op0()->type() == Value::Type::nil);
       }}},
     {"list?",
      {EMPTY_SIG(1),
       [](int argc) { return make_boolean(is_list(get_op0())); }}},
     {"nil?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::nil);
       }}},
     {"int?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::integer);
       }}},
     {"float?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::fp);
       }}},
     {"pair?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::cons);
       }}},
     {"lambda?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::function);
       }}},
     {"error?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::error);
       }}},
     {"symbol?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::symbol);
       }}},
     {"userdata-tag",
      {SIG1(integer, user_data),
       [](int argc) {
           L_EXPECT_OP(0, user_data);
           return L_INT(get_op0()->user_data().tag_);
       }}},
     {"type",
      {SIG1(symbol, nil),
       [](int argc) {
           if (get_op0()->type() == Value::Type::wrapped) {
               return dcompr(get_op0()->wrapped().type_sym_);
           }
           return make_symbol(type_to_string(get_op0()->type()));
       }}},
     {"userdata?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::user_data);
       }}},
     {"databuffer?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::user_data);
       }}},
     {"string?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::string);
       }}},
     {"wrapped?",
      {EMPTY_SIG(1),
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::wrapped);
       }}},
     {"wrap",
      {SIG2(wrapped, nil, symbol),
       [](int argc) {
           L_EXPECT_OP(0, symbol);
           return wrap(get_op1(), get_op0());
       }}},
     {"unwrap",
      {SIG1(nil, wrapped),
       [](int argc) {
           L_EXPECT_OP(0, wrapped);
           return dcompr(get_op0()->wrapped().data_);
       }}},
     {"odd?",
      {SIG1(nil, integer),
       [](int argc) {
           if (get_op0()->type() == Value::Type::integer) {
               return make_boolean(L_LOAD_INT(0) % 2);
           }
           return make_boolean(false);
       }}},
     {"string-to-bytes",
      {SIG1(cons, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           ListBuilder result;

           int i = 0;
           while (get_op0()->string().value()[i] not_eq '\0') {
               result.push_back(L_INT(get_op0()->string().value()[i]));
               ++i;
           }
           return result.result();
       }}},
     {"bytes-to-string",
      {SIG1(string, cons),
       [](int argc) {
           if (not is_list(get_op0())) {
               return make_error("bytes-to-string expects a list param!");
           }

           auto temp =
               allocate_dynamic<StringBuffer<2000>>("bytes-to-string-temp");
           l_foreach(get_op0(), [&temp](Value* v) {
               temp->push_back(v->integer().value_);
           });

           return make_string(temp->c_str());
       }}},
     {"int-to-bytes",
      {SIG1(cons, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           ListBuilder result;
           host_s32 value;
           value.set(L_LOAD_INT(0));

           for (int i = 0; i < 4; ++i) {
               result.push_back(L_INT(((u8*)&value)[i]));
           }

           return result.result();
       }}},
     {"bytes-to-int",
      {SIG1(integer, cons),
       [](int argc) {
           if (not is_list(get_op0())) {
               return make_error("bytes-to-int expects a list of four values!");
           }

           host_s32 value;
           int i = 0;

           l_foreach(get_op0(), [&](Value* v) {
               ((u8*)&value)[i++] = v->integer().value_;
           });

           return L_INT(value.get());
       }}},
     {"int",
      {SIG1(integer, nil),
       [](int argc) {
           if (get_op0()->type() == Value::Type::string) {
               auto str = L_LOAD_STRING(0);

               int accum = 0;
               while (*str not_eq '\0') {
                   accum = accum * 10 + (*str - '0');
                   ++str;
               }
               return L_INT(accum);

           } else if (get_op0()->type() == Value::Type::fp) {
               return L_INT(L_LOAD_FP(0));
           } else if (get_op0()->type() == Value::Type::integer) {
               return get_op0();
           } else {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
       }}},
     {"float",
      {SIG1(fp, nil),
       [](int argc) {
           if (get_op0()->type() == Value::Type::string) {
               auto str = L_LOAD_STRING(0);
               return L_FP(atof(str));
           } else if (get_op0()->type() == Value::Type::fp) {
               return get_op0();
           } else if (get_op0()->type() == Value::Type::integer) {
               return L_FP(L_LOAD_INT(0));
           } else {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }
       }}},
     {"error-info",
      {SIG1(nil, error),
       [](int argc) {
           if (get_op0()->type() == Value::Type::error) {
               return dcompr(get_op0()->error().context_);
           }
           return L_NIL;
       }}},
     {"databuffer",
      {SIG0(databuffer),
       [](int argc) { return make_databuffer("lisp-databuffer"); }}},
     {"buffer-write!",
      {SIG3(nil, databuffer, integer, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           L_EXPECT_OP(1, integer);
           L_EXPECT_OP(2, databuffer);

           if (not is_list(get_op0())) {
               return make_boolean(false); // TODO: error
           }

           u16 len = length(get_op(0));
           u16 offset = L_LOAD_INT(1);

           if (offset + len >= SCRATCH_BUFFER_SIZE) {
               return make_boolean(false); // TODO: error
           }

           int i = offset;
           l_foreach(get_op0(), [&](Value* val) {
               u8 byte = val->integer().value_;
               get_op(2)->databuffer().value()->data_[i++] = byte;
           });

           return make_boolean(true);
       }}},
     {"buffer-read",
      {SIG3(cons, databuffer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           L_EXPECT_OP(2, databuffer);

           u16 len = L_LOAD_INT(0);
           u16 offset = L_LOAD_INT(1);

           if (offset + len >= SCRATCH_BUFFER_SIZE) {
               return make_boolean(false);
           }

           auto sbr = get_op(2)->databuffer().value();

           lisp::ListBuilder result;
           for (int i = 0; i < len; ++i) {
               result.push_back(L_INT(sbr->data_[i + offset]));
           }

           return result.result();
       }}},
     {"apropos",
      {SIG1(nil, string),
       [](int argc) {
           L_EXPECT_OP(0, string);

           Vector<const char*> results;
           apropos(L_LOAD_STRING(0), results);

           ListBuilder list;
           for (auto& r : results) {
               list.push_back(make_string(r));
           }

           return list.result();
       }}},
     {"apply",
      {SIG2(nil, function, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           L_EXPECT_OP(1, function);

           auto lat = get_op0();
           auto fn = get_op1();

           int apply_argc = 0;
           while (lat not_eq get_nil()) {
               if (lat->type() not_eq Value::Type::cons) {
                   return make_error(Error::Code::invalid_argument_type, lat);
               }
               ++apply_argc;
               push_op(lat->cons().car());

               lat = lat->cons().cdr();
           }

           funcall(fn, apply_argc);

           auto result = get_op0();
           pop_op();

           return result;
       }}},
     {"fill",
      {SIG2(cons, integer, integer),
       [](int argc) {
           L_EXPECT_OP(1, integer);

           auto result = make_list(get_op1()->integer().value_);
           for (int i = 0; i < get_op1()->integer().value_; ++i) {
               set_list(result, i, get_op0());
           }

           return result;
       }}},
     {"difference",
      {SIG2(cons, cons, cons),
       [](int argc) {
           ListBuilder list;

           auto find_difference = [&](Value* lat1, Value* lat2) {
               l_foreach(lat1, [&](Value* v1) {
                   if (not contains(lat2, v1)) {
                       list.push_back(v1);
                   }
               });
           };
           find_difference(get_op0(), get_op1());
           find_difference(get_op1(), get_op0());

           return list.result();
       }}},
     {"union",
      {SIG2(cons, cons, cons),
       [](int argc) {
           ListBuilder list;

           l_foreach(get_op0(), [&](Value* v) {
               if (not contains(list.result(), v) and contains(get_op1(), v)) {
                   list.push_back(v);
               }
           });

           l_foreach(get_op1(), [&](Value* v) {
               if (not contains(list.result(), v) and contains(get_op0(), v)) {
                   list.push_back(v);
               }
           });

           return list.result();
       }}},
     {"collect",
      {SIG1(cons, function),
       [](int argc) {
           L_EXPECT_OP(0, function);

           ListBuilder res;

           funcall(get_op0(), 0);
           Protected c = get_op0();
           pop_op();

           while (is_boolean_true(c)) {
               res.push_back(c);

               funcall(get_op0(), 0);
               c = get_op0();
               pop_op();
           }

           return res.result();
       }}},
     {"length",
      {SIG1(integer, nil),
       [](int argc) {
           if (get_op0()->type() == Value::Type::nil) {
               return make_integer(0);
           } else if (get_op0()->type() == Value::Type::string) {
               return make_integer(utf8::len(get_op0()->string().value()));
           }

           L_EXPECT_OP(0, cons);

           return make_integer(length(get_op0()));
       }}},
     {"<", {EMPTY_SIG(2), l_comp_less_than}},
     {">",
      {EMPTY_SIG(2),
       [](int argc) {
           if (get_op0()->type() == Value::Type::fp) {
               L_EXPECT_OP(1, fp);
               return make_integer(get_op1()->fp().value_ >
                                   get_op0()->fp().value_);
           }
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return make_integer(get_op1()->integer().value_ >
                               get_op0()->integer().value_);
       }}},
     {"bit-and",
      {SIG2(integer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) & L_LOAD_INT(0));
       }}},
     {"bit-or",
      {SIG2(integer, integer, integer),
       [](int argc) {
           int accum = 0;
           for (int i = 0; i < argc; ++i) {
               L_EXPECT_OP(i, integer);
               accum |= get_op(i)->integer().value_;
           }
           return make_integer(accum);
       }}},
     {"bit-xor",
      {SIG2(integer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) ^ L_LOAD_INT(0));
       }}},
     {"bit-not",
      {SIG1(integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return L_INT(~L_LOAD_INT(0));
       }}},
     {"mod",
      {SIG2(integer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) % L_LOAD_INT(0));
       }}},
     {"bit-shift-right",
      {SIG2(integer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) >> L_LOAD_INT(0));
       }}},
     {"bit-shift-left",
      {SIG2(integer, integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) << L_LOAD_INT(0));
       }}},
     {"hex",
      {SIG1(string, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           const char* hex = "0123456789abcdef";
           auto v = L_LOAD_INT(0);
           Buffer<char, 8> stack;
           while (v) {
               stack.push_back(hex[v & 0xf]);
               v >>= 4;
           }
           StringBuffer<10> result("0x");
           for (char c : reversed(stack)) {
               result.push_back(c);
           }
           return make_string(result.c_str());
       }}},
     {"incr",
      {SIG1(integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return L_INT(L_LOAD_INT(0) + 1);
       }}},
     {"decr",
      {SIG1(integer, integer),
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return L_INT(L_LOAD_INT(0) - 1);
       }}},
     {"+",
      {EMPTY_SIG(0),
       [](int argc) {
           if (argc >= 1 and get_op0()->type() == Value::Type::fp) {
               Float::ValueType accum = 0;
               for (int i = 0; i < argc; ++i) {
                   L_EXPECT_OP(i, fp);
                   accum += get_op(i)->fp().value_;
               }
               return L_FP(accum);
           }
           int accum = 0;
           for (int i = 0; i < argc; ++i) {
               L_EXPECT_OP(i, integer);
               accum += get_op(i)->integer().value_;
           }
           return make_integer(accum);
       }}},
     {"-",
      {EMPTY_SIG(1),
       [](int argc) {
           if (argc == 1) {
               if (get_op0()->type() == Value::Type::fp) {
                   return L_FP(-L_LOAD_FP(0));
               }
               L_EXPECT_OP(0, integer);
               return make_integer(-get_op0()->integer().value_);

           } else {
               L_EXPECT_ARGC(argc, 2);
               if (get_op0()->type() == Value::Type::fp) {
                   L_EXPECT_OP(1, fp);
                   return L_FP(get_op1()->fp().value_ - get_op0()->fp().value_);
               }

               L_EXPECT_OP(1, integer);
               L_EXPECT_OP(0, integer);
               return make_integer(get_op1()->integer().value_ -
                                   get_op0()->integer().value_);
           }
       }}},
     {"*",
      {EMPTY_SIG(0),
       [](int argc) {
           if (argc >= 1 and get_op0()->type() == Value::Type::fp) {
               Float::ValueType accum = 1;
               for (int i = 0; i < argc; ++i) {
                   L_EXPECT_OP(i, fp);
                   accum *= get_op(i)->fp().value_;
               }
               return L_FP(accum);
           }
           int accum = 1;
           for (int i = 0; i < argc; ++i) {
               L_EXPECT_OP(i, integer);
               accum *= get_op(i)->integer().value_;
           }
           return make_integer(accum);
       }}},
     {"/",
      {EMPTY_SIG(2),
       [](int argc) {
           if (get_op0()->type() == Value::Type::fp) {
               L_EXPECT_OP(1, fp);
               return L_FP(get_op1()->fp().value_ / get_op0()->fp().value_);
           }
           L_EXPECT_OP(1, integer);
           L_EXPECT_OP(0, integer);
           return make_integer(get_op1()->integer().value_ /
                               get_op0()->integer().value_);
       }}},
     {"gensym", {SIG0(symbol), [](int) { return gensym(); }}},
     {"lisp-mem-stack-used",
      {SIG0(integer),
       [](int argc) { return L_INT(bound_context->operand_stack_->size()); }}},
     {"lisp-mem-stack-contents",
      {SIG0(cons),
       [](int argc) {
           lisp::ListBuilder b;
           for (auto& v : *bound_context->operand_stack_) {
               b.push_back(v);
           }
           return b.result();
       }}},
     {"lisp-mem-string-storage",
      {SIG0(integer),
       [](int argc) {
           int bytes = 0;
           live_values([&bytes](Value& val) {
               if (val.type() == Value::Type::string) {
                   if (val.string().variant() == String::memory_string) {
                       bytes += strlen(val.string().value()) + 1;
                   }
               }
           });
           return L_INT(bytes);
       }}},
     {"lisp-mem-string-buffers",
      {SIG0(cons),
       [](int argc) {
           ListBuilder buffers;
           live_values([&buffers](Value& val) {
               if (val.type() == Value::Type::string) {
                   if (val.string().variant() == String::memory_string) {
                       auto buf =
                           dcompr(val.string().data_.memory_.databuffer_);
                       bool found = false;
                       l_foreach(buffers.result(), [&](Value* v) {
                           if (v == buf) {
                               found = true;
                           }
                       });
                       if (not found) {
                           buffers.push_back(buf);
                       }
                   }
               }
           });
           return buffers.result();
       }}},
     {"lisp-mem-set-gc-thresh",
      {SIG1(nil, integer),
       [](int) {
           L_EXPECT_OP(0, integer);
           early_gc_threshold = L_LOAD_INT(0);
           return L_NIL;
       }}},
     {"lisp-mem-crit-gc-alert",
      {SIG1(nil, integer),
       [](int) {
           L_EXPECT_OP(0, integer);
           bound_context->critical_gc_alert_ = L_LOAD_INT(0);
           return L_NIL;
       }}},
     {"lisp-mem-vals-remaining",
      {SIG0(integer),
       [](int) {
           int values_remaining = 0;
           Value* current = value_pool;
           while (current) {
               ++values_remaining;
               current = current->heap_node().next_;
           }

           return L_INT(values_remaining);
       }}},
     {"lisp-mem-global-count",
      {SIG0(integer),
       [](int argc) {
           auto& ctx = bound_context;
           int globals_used = 0;
           globals_tree_traverse(
               ctx->globals_tree_,
               [&globals_used](Value&, Value&) { ++globals_used; });

           return L_INT(globals_used);
       }}},
     {"lisp-mem-string-internb",
      {SIG0(integer),
       [](int argc) {
           auto& ctx = bound_context;
           return L_INT(ctx->string_intern_pos_);
       }}},
     {"lisp-mem-sbr-used",
      {SIG0(integer),
       [](int argc) {
           int databuffers = 0;
           for (int i = 0; i < VALUE_POOL_SIZE; ++i) {
               Value* val = (Value*)&value_pool_data[i];
               if (val->hdr_.alive_ and
                   val->hdr_.type_ == Value::Type::databuffer) {
                   ++databuffers;
               }
           }
           return L_INT(databuffers);
       }}},
     {"range",
      {SIG1(cons, integer),
       [](int argc) {
           int start = 0;
           int end = 0;
           int incr = 1;

           if (argc == 1) {

               L_EXPECT_OP(0, integer);

               start = 0;
               end = get_op0()->integer().value_;

           } else if (argc == 2) {

               L_EXPECT_OP(1, integer);
               L_EXPECT_OP(0, integer);

               start = get_op1()->integer().value_;
               end = get_op0()->integer().value_;

           } else if (argc == 3) {

               L_EXPECT_OP(2, integer);
               L_EXPECT_OP(1, integer);
               L_EXPECT_OP(0, integer);

               start = get_op(2)->integer().value_;
               end = get_op1()->integer().value_;
               incr = get_op0()->integer().value_;
           } else {
               return lisp::make_error(lisp::Error::Code::invalid_argc, L_NIL);
           }

           if (incr == 0) {
               return get_nil();
           }

           ListBuilder lat;

           for (int i = start; i < end; i += incr) {
               lat.push_back(make_integer(i));
           }

           return lat.result();
       }}},
     {"unbind",
      {EMPTY_SIG(0),
       [](int argc) {
           for (int i = 0; i < argc; ++i) {
               auto sym = get_op(i);
               if (sym->type() not_eq lisp::Value::Type::symbol) {
                   auto err = lisp::Error::Code::invalid_argument_type;
                   return lisp::make_error(err, L_NIL);
               }
               set_var(sym, L_NIL, true);
               globals_tree_erase(sym);
           }

           return get_nil();
       }}},
     {"symbol",
      {SIG1(symbol, string),
       [](int argc) {
           L_EXPECT_OP(0, string);

           return make_symbol(get_op0()->string().value());
       }}},
     {"format",
      {SIG2(string, string, nil),
       [](int argc) {
           int fmt_arg = argc - 2;

           L_EXPECT_OP(argc - 1, string);
           auto builder = allocate_dynamic<StringBuffer<1800>>("lisp-fmt");

           auto str = get_op(argc - 1)->string().value();

           while (*str not_eq '\0') {
               if (*str == '%') {
                   if (fmt_arg == -1) {
                       return L_NIL;
                   }

                   DefaultPrinter p;
                   format(get_op(fmt_arg), p);
                   *builder += p.data_.c_str();

                   --fmt_arg;
               } else {
                   builder->push_back(*str);
               }
               ++str;
           }

           return make_string(builder->c_str());
       }}},
     {"profile",
      {SIG1(integer, function),
       [](int argc) {
           L_EXPECT_OP(0, function);
           auto start = PLATFORM.delta_clock().sample();
           funcall(get_op0(), 0);
           auto stop = PLATFORM.delta_clock().sample();
           pop_op();
           return L_INT(stop - start);
       }}},
     {"slice",
      {EMPTY_SIG(2),
       [](int argc) {
           int begin = 0;
           int end = 0;

           int seq = 2;
           bool until_end = false;

           if (argc == 2) {
               L_EXPECT_OP(0, integer);
               seq = 1;
               begin = L_LOAD_INT(0);
               end = VALUE_POOL_SIZE;
               until_end = true;
           } else {
               L_EXPECT_OP(0, integer);
               L_EXPECT_OP(1, integer);
               begin = L_LOAD_INT(1);
               end = L_LOAD_INT(0);
           }

           int index = 0;

           if (get_op(seq)->type() == Value::Type::cons) {

               if (until_end) {
                   // In this case, we want nodes up to and including the end
                   // node, therefore, we don't need to copy any of the old
                   // list, we just need to iterate until the begin index and
                   // return the cdr.

                   auto list = get_op(seq);
                   while (true) {
                       if (list->type() not_eq Value::Type::cons) {
                           break;
                       } else {
                           if (index >= begin) {
                               return list;
                           }
                           ++index;
                       }
                       list = list->cons().cdr();
                   }
                   return list;
               }

               ListBuilder result;

               auto list = get_op(seq);

               while (true) {
                   if (list->type() not_eq Value::Type::cons) {
                       break;
                   } else {
                       auto v = list->cons().car();
                       if (index >= begin and index < end) {
                           result.push_back(v);
                       } else if (index == end) {
                           break;
                       }
                       ++index;
                   }

                   list = list->cons().cdr();
               }

               return result.result();

           } else if (get_op(seq)->type() == Value::Type::string) {

               auto inp_str = L_LOAD_STRING(2);
               auto builder = allocate_dynamic<StringBuffer<2000>>("lispslice");

               int index = 0;
               utf8::scan(
                   [&](const utf8::Codepoint&, const char* raw, int) {
                       if (index >= begin and index < end) {
                           (*builder) += raw;
                       }
                       ++index;
                       return true;
                   },
                   inp_str,
                   strlen(inp_str));

               return make_string(builder->c_str());

           } else {
               L_EXPECT_OP(2, cons);
               return L_NIL;
           }
       }}},
     {"string-explode",
      {SIG1(cons, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           ListBuilder list;

           auto str = L_LOAD_STRING(0);
           utf8::scan(
               [&](const utf8::Codepoint& cp, const char*, int) {
                   list.push_back(L_INT(cp));
                   return true;
               },
               str,
               strlen(str));

           return list.result();
       }}},
     {"string-assemble",
      {SIG1(string, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);

           auto str = allocate_dynamic<StringBuffer<2000>>("tempstr");

           l_foreach(get_op0(), [&](Value* val) {
               auto v = val->integer().value_;
               auto cp = (const char*)&v;
               for (int i = 0; i < 4; ++i) {
                   if (cp[i]) {
                       str->push_back(cp[i]);
                   }
               }
           });

           return make_string(str->c_str());
       }}},
     {"string",
      {SIG0(string),
       [](int argc) {
           EvalBuffer b;
           EvalPrinter p(b);

           if (argc == 1 and get_op0()->hdr_.type() == Value::Type::symbol) {
               return make_string_from_literal(get_op0()->symbol().name());
           }

           for (int i = argc - 1; i > -1; --i) {
               auto val = get_op(i);
               if (val->type() == Value::Type::string) {
                   p.put_str(val->string().value());
               } else {
                   format_impl(val, p, 0);
               }
           }

           return make_string(b.c_str());
       }}},
     {"error",
      {SIG1(error, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           return make_error(Error::Code::custom, get_op0());
       }}},
     {"bound?",
      {SIG1(nil, symbol),
       [](int argc) {
           L_EXPECT_OP(0, symbol);

           auto found = globals_tree_find(get_op0());
           return make_integer(found not_eq nullptr);
       }}},
     {"filter",
      {SIG2(cons, function, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           L_EXPECT_OP(1, function);

           auto fn = get_op1();
           Value* result = make_cons(L_NIL, L_NIL);
           auto prev = result;
           auto current = result;

           l_foreach(get_op0(), [&](Value* val) {
               push_op(result); // gc protect

               push_op(val);
               funcall(fn, 1);
               auto funcall_result = get_op0();

               if (is_boolean_true(funcall_result)) {
                   current->cons().set_car(val);
                   auto next = make_cons(L_NIL, L_NIL);
                   current->cons().set_cdr(next);
                   prev = current;
                   current = next;
               }
               pop_op(); // funcall result

               pop_op(); // gc unprotect
           });

           if (current == result) {
               return L_NIL;
           }

           prev->cons().set_cdr(L_NIL);

           return result;
       }}},
     {"nameof",
      {SIG1(nil, nil),
       [](int argc) {
           if (auto name = nameof(get_op0())) {
               return make_symbol(name);
           }
           return L_NIL;
       }}},
     {"foreach",
      {SIG2(nil, function, cons),
       [](int argc) {
           L_EXPECT_OP(1, function);
           L_EXPECT_OP(0, cons);

           auto fn = get_op1();

           l_foreach(get_op0(), [&](Value* val) {
               push_op(val);
               funcall(fn, 1);
               pop_op(); // result
           });

           return L_NIL;
       }}},
     {"map",
      {SIG2(cons, function, cons),
       [](int argc) {
           if (lisp::get_op(argc - 1)->type() not_eq Value::Type::function and
               lisp::get_op(argc - 1)->type() not_eq Value::Type::cons) {
               return lisp::make_error(lisp::Error::Code::invalid_argument_type,
                                       L_NIL);
           }

           // I've never seen map used with so many input lists, but who knows,
           // someone might try to call this with more than six inputs...
           Buffer<Value*, 6> inp_lats;

           if (argc < static_cast<int>(inp_lats.size())) {
               return get_nil(); // TODO: return error
           }

           for (int i = 0; i < argc - 1; ++i) {
               L_EXPECT_OP(i, cons);
               inp_lats.push_back(get_op(i));
           }

           const auto len = length(inp_lats[0]);
           if (len == 0) {
               return get_nil();
           }
           for (auto& l : inp_lats) {
               if (length(l) not_eq len) {
                   return get_nil(); // return error instead!
               }
           }

           auto fn = get_op(argc - 1);

           int index = 0;

           ListBuilder result;

           // Because the length function returned a non-zero value, we've
           // already succesfully scanned the list, so we don't need to do any
           // type checking.

           while (index < len) {

               for (auto& lat : reversed(inp_lats)) {
                   push_op(lat->cons().car());
                   lat = lat->cons().cdr();
               }
               funcall(fn, inp_lats.size());
               auto fc_result = get_op0();

               result.push_back(fc_result);
               pop_op();


               ++index;
           }

           return result.result();
       }}},
     {"find",
      {SIG2(nil, nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);

           Optional<int> index;

           int i = 0;
           l_foreach(get_op0(), [&](Value* v) {
               if (index) {
                   return;
               }
               if (is_equal(get_op1(), v)) {
                   index = i;
               }
               ++i;
           });

           if (index) {
               return L_INT(*index);
           }

           return L_NIL;
       }}},
     {"flatten",
      {SIG1(cons, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);

           auto inp = get_op0();

           lisp::ListBuilder b;

           ::Function<6 * sizeof(void*), void(Value*)> flatten_impl(
               [](Value*) {});
           flatten_impl = [&](Value* val) {
               if (is_list(val)) {
                   l_foreach(val, flatten_impl);
               } else {
                   b.push_back(val);
               }
           };

           l_foreach(inp, flatten_impl);

           return b.result();
       }}},
     {"reverse",
      {SIG1(cons, cons),
       [](int argc) {
           if (get_op0()->type() not_eq lisp::Value::Type::cons) {
               return L_NIL;
           }

           L_EXPECT_OP(0, cons);

           Value* result = get_nil();
           l_foreach(get_op0(), [&](Value* car) {
               push_op(result);
               result = make_cons(car, result);
               pop_op();
           });

           return result;
       }}},
     {"gc", {EMPTY_SIG(0), [](int argc) { return make_integer(run_gc()); }}},
     {"get",
      {SIG2(nil, cons, integer),
       [](int argc) {
           if (get_op0()->type() == lisp::Value::Type::nil) {
               return L_NIL;
           }

           L_EXPECT_OP(0, integer);

           const auto index = L_LOAD_INT(0);

           // if (get_op0()->type() == lisp::Value::Type::string) {
           //     auto str_data = L_LOAD_STRING(0);
           //     auto str_size = strlen(str_data);

           // }

           L_EXPECT_OP(1, cons);

           return get_list(get_op1(), index);
       }}},
     {"read",
      {SIG1(nil, string),
       [](int argc) {
           L_EXPECT_OP(0, string);
           BasicCharSequence seq(get_op0()->string().value());
           read(seq);
           auto result = get_op0();
           pop_op();
           return result;
       }}},
     {"lint",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           if (not is_list(get_op0())) {
               return make_error("lint expects list parameter!");
           }

           lint(get_op0(), L_NIL);
           auto result = get_op0();
           pop_op();
           return result;
       }}},
     {"eval",
      {SIG1(nil, nil),
       [](int argc) {
           eval(get_op0());
           auto result = get_op0();
           pop_op(); // result

           return result;
       }}},
     {"stacktrace", {SIG0(cons), [](int argc) { return stacktrace(); }}},
     {"this",
      {SIG0(function),
       [](int argc) {
           return bound_context->callstack_->cons().cdr()->cons().car();
       }}},
     {"sort",
      {SIG2(cons, cons, function),
       [](int argc) {
           L_EXPECT_OP(0, function);
           L_EXPECT_OP(1, cons);

           auto comp = get_op0();

           auto compare = [comp](Value* lhs, Value* rhs) {
               if (comp->hdr_.mode_bits_ == Function::ModeBits::cpp_function and
                   comp->function().cpp_impl_ == l_comp_less_than) {
                   return comp_less_than(lhs, rhs);
               } else {
                   push_op(lhs);
                   push_op(rhs);
                   funcall(comp, 2);
                   auto result = get_op0();
                   pop_op(); // result
                   return is_boolean_true(result);
               }
           };

           if (not is_list(get_op1())) {
               return make_error("sort parameter must be list!");
           }

           using TempBuffer = Buffer<Value*, 509>;
           auto buf = allocate_dynamic_fast<TempBuffer>("sort-buffer");

           l_foreach(get_op1(), [&buf](Value* v) { buf->push_back(v); });

           std::sort(buf->begin(), buf->end(), compare);

           ListBuilder result;
           for (Value* v : *buf) {
               result.push_back(v);
           }

           return result.result();
       }}},
     {"env",
      {EMPTY_SIG(0),
       [](int argc) {
           Value* result = make_cons(get_nil(), get_nil());
           push_op(result); // protect from the gc

           Value* current = result;

           get_env([&current](const char* str) {
               current->cons().set_car(intern_to_symbol(str));
               auto next = make_cons(get_nil(), get_nil());
               current->cons().set_cdr(next);
               current = next;
           });

           pop_op(); // result

           return result;
       }}},
     {"compile",
      {SIG1(function, function),
       [](int argc) {
           L_EXPECT_OP(0, function);

           if (get_op0()->hdr_.mode_bits_ ==
               Function::ModeBits::lisp_function) {
               auto input = get_op0();
               compile(dcompr(get_op0()->function().lisp_impl_.code_));
               auto ret = get_op0();
               if (ret->type() == Value::Type::function) {
                   ret->function().sig_ = input->function().sig_;
               }
               pop_op();
               return ret;
           } else {
               return get_op0();
           }
       }}},
     {"disassemble",
      {SIG1(nil, function), [](int argc) {
           L_EXPECT_OP(0, function);

           if (get_op0()->hdr_.mode_bits_ ==
               Function::ModeBits::lisp_bytecode_function) {

               Platform::RemoteConsole::Line out;

               u8 depth = 0;

               auto buffer = get_op0()->function().bytecode_impl_.databuffer();

               auto data = buffer->databuffer().value();

               const auto start_offset = get_op0()
                                             ->function()
                                             .bytecode_impl_.bytecode_offset()
                                             ->integer()
                                             .value_;

               for (int i = start_offset; i < SCRATCH_BUFFER_SIZE;) {

                   const auto offset = to_string<10>(i - start_offset);
                   if (offset.length() < 4) {
                       for (u32 i = 0; i < 4 - offset.length(); ++i) {
                           out.push_back('0');
                       }
                   }

                   out += offset;
                   out += ": ";

                   using namespace instruction;

                   switch ((Opcode)(*data).data_[i]) {
                   case Fatal::op():
                       return get_nil();

                   case LoadBuiltin::op(): {
                       out += LoadBuiltin::name();
                       out += "(";
                       auto ptr = ((UnalignedPtr*)(data->data_ + i + 1))->get();
                       out += nameof((Function::CPP_Impl)ptr);
                       out += ":";
                       out += stringify(
                           *(u8*)(data->data_ + i + 1 + sizeof(UnalignedPtr)));
                       out += ")";
                       i += sizeof(LoadBuiltin);
                       break;
                   }

                   case load_var_nonlocal:
                   case LoadVar::op():
                       out += LoadVar::name();
                       out += "(";
                       out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                       out += ")";
                       i += sizeof(LoadVar);
                       break;

                   case load_var_small_nonlocal:
                   case LoadVarSmall::op(): {
                       i += 1;
                       out += "LOAD_VAR_SMALL(";
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 5;
                       break;
                   }

                   case LoadLocalCached::op(): {
                       i += 1;
                       out += "LOAD_LOCAL_CACHED(";
                       out += stringify((int)*(data->data_ + i));
                       i += 1;
                       out += ", ";
                       out += stringify((int)*(data->data_ + i));
                       i += 1;
                       out += ")";
                       i += *(data->data_ + i); // padding
                       i += 1;
                       break;
                   }

                   case LoadVarRelocatable::op():
                       i += 1;
                       out += "LOAD_VAR_RELOCATABLE(";
                       out += to_string<32>(
                           ((HostInteger<s16>*)(data->data_ + i))->get());
                       out += ")";
                       i += 2;
                       break;

                   case PushSmallSymbol::op(): {
                       i += 1;
                       out += "PUSH_SMALL_SYMBOL(";
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 4;
                       break;
                   }

                   case PushSymbol::op():
                       out += "PUSH_SYMBOL(";
                       out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                       out += ")";
                       i += sizeof(PushSymbol);
                       break;

                   case PushSymbolRelocatable::op():
                       i += 1;
                       out += "PUSH_SYMBOL_RELOCATABLE(";
                       out += to_string<32>(
                           ((HostInteger<s16>*)(data->data_ + i))->get());
                       out += ")";
                       i += 2;
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

                   case PushInteger::op():
                       i += 1;
                       out += "PUSH_INTEGER(";
                       out += to_string<32>(
                           ((HostInteger<s32>*)(data->data_ + i))->get());
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

                   case Jump::op():
                       out += "JUMP(";
                       out += to_string<32>(
                           ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                       out += ")";
                       i += 3;
                       break;

                   case SmallJumpIfFalse::op():
                       out += "SMALL_JUMP_IF_FALSE(";
                       out += to_string<32>(*(data->data_ + i + 1));
                       out += ")";
                       i += 2;
                       break;

                   case SmallJump::op():
                       out += "SMALL_JUMP(";
                       out += to_string<32>(*(data->data_ + i + 1));
                       out += ")";
                       i += 2;
                       break;

                   case PushLambda::op():
                       out += "PUSH_LAMBDA(";
                       out += to_string<32>(
                           ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                       out += ")";
                       i += 3;
                       ++depth;
                       break;

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

                   case Dup::op():
                       out += Dup::name();
                       i += 1;
                       break;

                   case EarlyRet::op():
                       out += EarlyRet::name();
                       i += sizeof(EarlyRet);
                       break;

                   case LexicalDef::op():
                       out += LexicalDef::name();
                       out += "(";
                       out += ((UnalignedPtr*)(data->data_ + i + 1))->get();
                       out += ")";
                       i += sizeof(LexicalDef);
                       break;

                   case LexicalDefSmallFromArg0::op(): {
                       out += LexicalDefSmallFromArg0::name();
                       out += "(";
                       i += 1;
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 4;
                       break;
                   }

                   case LexicalDefSmallFromArg1::op(): {
                       out += LexicalDefSmallFromArg1::name();
                       out += "(";
                       i += 1;
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 4;
                       break;
                   }

                   case LexicalDefSmallFromArg2::op(): {
                       out += LexicalDefSmallFromArg2::name();
                       out += "(";
                       i += 1;
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 4;
                       break;
                   }

                   case LexicalDefSmall::op(): {
                       out += LexicalDefSmall::name();
                       out += "(";
                       i += 1;
                       StringBuffer<4> name;
                       for (int j = 0; j < 4; ++j) {
                           name.push_back(*(data->data_ + i + j));
                       }
                       out += name.c_str();
                       out += ")";
                       i += 4;
                       break;
                   }

                   case LexicalDefRelocatable::op():
                       out += LexicalDefRelocatable::name();
                       out += "(";
                       out += to_string<32>(
                           ((HostInteger<s16>*)(data->data_ + i + 1))->get());
                       out += ")";
                       i += sizeof(LexicalDefRelocatable);
                       break;

                   case LexicalFramePush::op():
                       out += LexicalFramePush::name();
                       i += sizeof(LexicalFramePush);
                       break;

                   case LexicalFramePop::op():
                       out += LexicalFramePop::name();
                       i += sizeof(LexicalFramePop);
                       break;

                   case LexicalVarLoad::op():
                       out += LexicalVarLoad::name();
                       i += sizeof(LexicalVarLoad);
                       break;

                   case Ret::op(): {
                       if (depth == 0) {
                           out += "RET\r\n";
                           auto pfrm = interp_get_pfrm();
                           if (pfrm->remote_console().printline(out.c_str(),
                                                                "")) {
                               ((Platform*)pfrm)->sleep(80);
                           } else {
                               info(out.c_str());
                           }
                           return get_nil();
                       } else {
                           --depth;
                           out += "RET";
                           i += 1;
                       }
                       break;
                   }

                   default:
                       interp_get_pfrm()->remote_console().printline(
                           out.c_str(), "");
                       interp_get_pfrm()->sleep(80);
                       return get_nil();
                   }
                   out += "\r\n";
               }
               return get_nil();
           } else if (get_op0()->hdr_.mode_bits_ ==
                      Function::ModeBits::lisp_function) {

               auto expression_list =
                   dcompr(get_op0()->function().lisp_impl_.code_);

               Protected sym(make_symbol("fn"));

               return make_cons(sym, expression_list);

           } else {
               return get_nil();
           }
       }}}});


int toplevel_count()
{
    int count = 0;

    auto& ctx = bound_context;

    globals_tree_traverse(ctx->globals_tree_,
                          [&count](Value& val, Value&) { ++count; });

    ctx->native_interface_.get_symbols_([&count](const char*) { ++count; });
    count += builtin_table.size();

    return count;
}


void get_env(SymbolCallback callback)
{
    auto& ctx = bound_context;

    for (auto& kvp : builtin_table) {
        callback(kvp.first.c_str());
    }

    ctx->native_interface_.get_symbols_(callback);

    globals_tree_traverse(ctx->globals_tree_, [&callback](Value& val, Value&) {
        callback((const char*)val.cons().car()->symbol().name());
    });

    l_foreach(ctx->macros_,
              [&](Value* v) { callback(v->cons().car()->symbol().name()); });
}



const char* intern(const char* string)
{
    const auto len = strlen(string);

    auto& ctx = bound_context;

    auto found_builtin = builtin_table.find(string);
    if (found_builtin not_eq builtin_table.end()) {
        // If the string exists as a constant in the builtin table, then it
        // needn't be copied to the string intern table.
        return found_builtin->first.c_str();
    }

    if (auto ni_sym = ctx->native_interface_.resolve_intern_sym_(string)) {
        return ni_sym;
    }

    if (ctx->external_symtab_contents_) {
        const char* search = ctx->external_symtab_contents_;
        u32 left = 0;
        u32 right = ctx->external_symtab_size_ / 32;

        while (left < right) {
            u32 mid = left + (right - left) / 2;
            const char* candidate = search + (mid * 32);

            int cmp = str_cmp(candidate, string);
            if (cmp == 0) {
                return candidate;
            } else if (cmp < 0) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        return nullptr; // not found
    }

    // Ok, no stable pointer to the string exists anywhere, so we'll have to
    // preserve the string contents in intern memory.

    if (len + 1 >
        string_intern_table_size - bound_context->string_intern_pos_) {

        PLATFORM.fatal("string intern table full");
    }

    if (not ctx->string_intern_table_) {
        ctx->string_intern_table_ =
            allocate_dynamic<StringInternTable>("string-intern-table");
        info(::format("allocating string intern table (due to symbol %)",
                      string));
    }

    const char* search = (*ctx->string_intern_table_)->data_;
    for (int i = 0; i < ctx->string_intern_pos_;) {
        if (str_eq(search + i, string)) {
            return search + i;
        } else {
            while (search[i] not_eq '\0') {
                ++i;
            }
            ++i;
        }
    }

    auto result = (*ctx->string_intern_table_)->data_ + ctx->string_intern_pos_;

    for (u32 i = 0; i < len; ++i) {
        ((*ctx->string_intern_table_)->data_)[ctx->string_intern_pos_++] =
            string[i];
    }
    ((*ctx->string_intern_table_)->data_)[ctx->string_intern_pos_++] = '\0';

    return result;
}


Value* __get_local(LocalVariableOffset off)
{
    auto stack = bound_context->lexical_bindings_;

    while (off.first) {
        stack = stack->cons().cdr();
        --off.first;
    }

    auto bindings = stack->cons().car();
    while (off.second) {
        bindings = bindings->cons().cdr();
        --off.second;
    }

    auto kvp = bindings->cons().car();
    return kvp->cons().cdr();
}


Optional<LocalVariableOffset> __find_local(const char* intern_str)
{
    LocalVariableOffset ret{0, 0};

    auto symbol = make_symbol(intern_str, Symbol::ModeBits::stable_pointer);

    if (bound_context->lexical_bindings_ not_eq get_nil()) {
        auto stack = bound_context->lexical_bindings_;

        while (stack not_eq get_nil()) {

            ret.second = 0;

            auto bindings = stack->cons().car();
            while (bindings not_eq get_nil()) {
                auto kvp = bindings->cons().car();
                if (kvp->cons().car()->symbol().unique_id() ==
                    symbol->symbol().unique_id()) {
                    return ret;
                }

                bindings = bindings->cons().cdr();
                ++ret.second;
            }

            stack = stack->cons().cdr();
            ++ret.first;
        }
    }

    return std::nullopt;
}


NativeInterface::LookupResult __load_builtin(const char* name)
{
    auto found_builtin = builtin_table.find(name);
    if (found_builtin not_eq builtin_table.end()) {
        return found_builtin->second;
    }

    auto found_ni_fn = bound_context->native_interface_.lookup_function_(name);

    if (found_ni_fn.second) {
        return found_ni_fn;
    }

    return {EMPTY_SIG(0), nullptr};
}


Value* get_var(Value* symbol)
{
    if (symbol->symbol().name()[0] == '$') {
        if (symbol->symbol().name()[1] == 'V') {
            // Special case: use '$V' to access arguments as a list.
            ListBuilder lat;
            for (int i = bound_context->current_fn_argc_ - 1; i > -1; --i) {
                lat.push_front(get_arg(i));
            }
            return lat.result();
        } else if (symbol->symbol().name()[1] == 'q') {
            // A shortcut to allow you to refer to the quote symbol.
            return make_symbol("'");
        } else {
            s32 argn = 0;
            for (u32 i = 1; symbol->symbol().name()[i] not_eq '\0'; ++i) {
                argn = argn * 10 + (symbol->symbol().name()[i] - '0');
            }

            return get_arg(argn);
        }
    }

    // First, check to see if any lexical (non-global) bindings exist for a
    // symbol.
    if (bound_context->lexical_bindings_ not_eq get_nil()) {
        auto stack = bound_context->lexical_bindings_;

        while (stack not_eq get_nil()) {

            auto bindings = stack->cons().car();
            while (bindings not_eq get_nil()) {
                auto kvp = bindings->cons().car();
                if (kvp->cons().car()->symbol().unique_id() ==
                    symbol->symbol().unique_id()) {
                    return kvp->cons().cdr();
                }

                bindings = bindings->cons().cdr();
            }

            stack = stack->cons().cdr();
        }
    }

    // After walking all the way up the stack, we didn't find any existing
    // lexical bindings. Now, we want to check the global variable tree to see
    // if any variable exists.
    auto found = globals_tree_find(symbol);

    if (found) {
        return found;
    }

    const char* const symbol_name = symbol->symbol().name();

    // Next, we want to check to see if any builtin functions exist for our
    // symbol name. By keeping builtins out of the globals tree, we decrease the
    // lower bound on the interpreter's memory usage. On the other hand, doing
    // this does put additional pressure on the gc (because the functions need
    // to be boxed as lisp function each time they're accessed).
    //
    auto builtin = __load_builtin(symbol_name);
    if (builtin.second) {
        auto fn = lisp::make_function(builtin.second);
        fn->function().sig_ = builtin.first;
        return fn;
    }

    // Ok, and as a final step, let's look for any builtin constants, if the
    // system is running with a precomputed constant table.
    if (bound_context->external_constant_tab_) {
        u32 i = 0;
        for (; i < bound_context->external_constant_tab_size_;) {
            auto ptr = bound_context->external_constant_tab_ + i;
            u8 name_size = ((ConstantTabEntryHeader*)ptr)->field_size_;
            u8 value_size = ((ConstantTabEntryHeader*)ptr)->value_size_;
            const char* name = ptr + sizeof(ConstantTabEntryHeader);
            if (str_eq(name, symbol->symbol().name())) {
                return dostring(ptr + sizeof(ConstantTabEntryHeader) +
                                name_size);
            }
            i += sizeof(ConstantTabEntryHeader) + name_size + value_size;
        }
    }

    StringBuffer<31> hint("[var: ");
    hint += symbol->symbol().name();
    hint += "]";

    return make_error(Error::Code::undefined_variable_access,
                      make_string(hint.c_str()));
}


Value* set_var(Value* symbol, Value* val, bool define_var)
{
    if (bound_context->lexical_bindings_ not_eq get_nil()) {
        auto stack = bound_context->lexical_bindings_;

        while (stack not_eq get_nil()) {

            auto bindings = stack->cons().car();
            while (bindings not_eq get_nil()) {
                auto kvp = bindings->cons().car();
                if (kvp->cons().car()->symbol().unique_id() ==
                    symbol->symbol().unique_id()) {

                    kvp->cons().set_cdr(val);
                    return get_nil();
                }

                bindings = bindings->cons().cdr();
            }

            stack = stack->cons().cdr();
        }
    }

    if (not globals_tree_insert(symbol, val, define_var)) {
        return make_error(Error::Code::undefined_variable_access, symbol);
    }
    return val;
}


const char* nameof(Function::CPP_Impl impl)
{
    for (auto& entry : builtin_table) {
        if (impl == entry.second.second) {
            return entry.first.c_str();
        }
    }

    if (auto n = bound_context->native_interface_.lookup_name_(impl)) {
        return n;
    }

    return nullptr;
}


const char* nameof(Value* value)
{
    const char* name = nullptr;
    globals_tree_traverse(bound_context->globals_tree_,
                          [&](Value& car, Value& node) {
                              auto sym = car.cons().car();
                              auto v = car.cons().cdr();
                              if (value == v) {
                                  name = sym->symbol().name();
                              }
                          });

    if (name) {
        return name;
    }

    bool cpp_fn =
        value->type() == Value::Type::function and
        value->function().hdr_.mode_bits_ == Function::ModeBits::cpp_function;

    if (cpp_fn) {
        auto impl = value->function().cpp_impl_;
        return nameof(impl);
    }

    return nullptr;
}


void gc()
{
    run_gc();
}


void init(Optional<std::pair<const char*, u32>> external_symtab,
          Optional<std::pair<const char*, u32>> external_constant_tab)
{
    if (bound_context) {
        return;
    }

    bound_context.emplace();

#ifdef __USE_RECENT_ALLOC_CACHE__
    for (auto& v : bound_context->recent_alloc_cache_) {
        v = nullptr;
    }
#endif

    if (external_symtab and external_symtab->second) {
        bound_context->external_symtab_contents_ = external_symtab->first;
        bound_context->external_symtab_size_ = external_symtab->second;
    }

    if (external_constant_tab and external_constant_tab->second) {
        bound_context->external_constant_tab_ = external_constant_tab->first;
        bound_context->external_constant_tab_size_ =
            external_constant_tab->second;
    }

    auto& ctx = bound_context;

    value_pool_init();
    ctx->nil_ = alloc_value();
    ctx->nil_->hdr_.type_ = Value::Type::nil;
    ctx->nil_->hdr_.mode_bits_ = 0;
    ctx->globals_tree_ = ctx->nil_;
    ctx->callstack_ = ctx->nil_;
    ctx->lexical_bindings_ = ctx->nil_;

    ctx->string_buffer_ = ctx->nil_;
    ctx->macros_ = ctx->nil_;

    ctx->tree_nullnode_ = make_cons(get_nil(), make_cons(get_nil(), get_nil()));

    // Push a few nil onto the operand stack. Allows us to access the first few
    // elements of the operand stack without performing size checks.
    push_op(get_nil());
    push_op(get_nil());


    if (dcompr(compr(get_nil())) not_eq get_nil()) {
        PLATFORM.fatal("pointer compression test failed 1");
    }

    push_callstack(make_string_from_literal("toplevel"));

    for (int i = 0; i < MAX_NAMED_ARGUMENTS; ++i) {
        ctx->argument_symbols_[i] = make_symbol(::format("$%", i).c_str());
    }
}


} // namespace lisp
