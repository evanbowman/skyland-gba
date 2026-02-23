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
#include "builtins.hpp"
#include "debug.hpp"
#include "eternal/eternal.hpp"
#include "ext_workram_data.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"
#include "localization.hpp"
#include "memory/buffer.hpp"
#include "memory/pool.hpp"
#include "number/random.hpp"
#include "number/ratio.hpp"
#include "platform/libc.hpp"
#include "vm.hpp"

#if not MAPBOX_ETERNAL_IS_CONSTEXPR
#error "NON-Constexpr lookup table!"
#endif


#define USE_SYMBOL_CACHE


namespace lisp
{


static const u32 string_intern_table_size = 2000;


#if defined(__NDS__)
#define VALUE_POOL_SIZE 20000
#elif defined(__GBA__)
#define VALUE_POOL_SIZE 10000
#else
#define VALUE_POOL_SIZE 200000
#endif

#define SYMBOL_CACHE_SIZE 8


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
    DataBuffer databuffer_;
    String string_;
    Wrapped wrapped_;
    Ratio ratio_;
    Promise promise_;
    __Reserved<Value::Type::rational> __reserved_3;
    __Reserved<Value::Type::__reserved_1> __reserved_1;
    __Reserved<Value::Type::__reserved_0> __reserved_0;
};


#if defined(__GBA__) or defined(__NDS__)
static_assert(sizeof(ValueMemory) == 8);
#endif

static constexpr const int early_gc_threshold_min = 30;

static EXT_WORKRAM_DATA int value_remaining_count;
static EXT_WORKRAM_DATA int early_gc_threshold = early_gc_threshold_min;
static EXT_WORKRAM_DATA ValueMemory value_pool_data[VALUE_POOL_SIZE];
static Value* value_pool = nullptr;


static inline void gc_safepoint()
{
    if (value_remaining_count < early_gc_threshold) {
        gc();
    }
}


static inline void gc_require_space()
{
    // NOTE: Some parts of the interpreter are not safe to run gc in, but also
    // need to allocate values. So we run the gc early if we're below the
    // required values theshold, to make sure that we won't trigger the gc in a
    // part of the code where gc cannot be run.
    //
    // NOTE: because the early_gc_threshold will always be higher than the small
    // number of values that we require space for, there's no reason to actually
    // check how many values are left. We just need to check if we're below the
    // early gc threshold.
    gc_safepoint();
}

#define GC_REQUIRE_SPACE(REQUIRED_VALS)                                        \
    static_assert(REQUIRED_VALS < early_gc_threshold_min);                     \
    gc_require_space();


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
        DataBuffer::finalizer,
        String::finalizer,
        Float::finalizer,
        Wrapped::finalizer,
        Ratio::finalizer,
        Promise::finalizer,
        __Reserved<Value::Type::rational>::finalizer,
        __Reserved<Value::Type::__reserved_1>::finalizer,
        __Reserved<Value::Type::__reserved_0>::finalizer,
};


struct Context
{
    using OperandStack = Buffer<Value*, 497>;


    Context() : operand_stack_(allocate<OperandStack>("lisp-operand-stack"))
    {
        if (not operand_stack_) {
            PLATFORM.fatal("pointer compression test failed");
        }
    }

    DynamicMemory<OperandStack> operand_stack_;

    // If the game was built with a correctly formatted symbol lookup table,
    // then this in-memory table should never be needed...
    Optional<DynamicMemory<StringInternTable>> string_intern_table_;
    Optional<debug::DebugHandler> debug_handler_;
    Value* debug_breakpoints_ = nullptr;
    Value* debug_watchpoints_ = nullptr;

    Value* nil_ = nullptr;
    Value* string_buffer_ = nullptr;
    Value* bytecode_buffer_ = nullptr;
    Value* globals_tree_ = nullptr;
    Value* tree_nullnode_ = nullptr;

    Value* lexical_bindings_ = nullptr;
    Value* macros_ = nullptr;

    Value* callstack_ = nullptr;

    // contains symbol values $0, $1, $2, etc. used for argument substitution.
    // storing the symbols here significantly simplifies some edge cases
    // involving garbage collection that crop up during argument substitution.
    Value* argument_symbols_[MAX_NAMED_ARGUMENTS];

#ifdef USE_SYMBOL_CACHE
    Value* symbol_cache_[SYMBOL_CACHE_SIZE];
    u8 symbol_cache_index_ = 0;
#endif

    const char* external_symtab_contents_ = nullptr;
    u32 external_symtab_size_;

    const char* external_constant_tab_ = nullptr;
    u32 external_constant_tab_size_;

    NativeInterface native_interface_;

    int string_intern_pos_ = 0;
    u32 alloc_highwater_ = 0;

    Symbol::UniqueId if_symbol_id_;
    Symbol::UniqueId let_symbol_id_;
    Symbol::UniqueId while_symbol_id_;
    Symbol::UniqueId lambda_symbol_id_;
    Symbol::UniqueId fn_symbol_id_;
    Symbol::UniqueId quote_symbol_id_;
    Symbol::UniqueId quasiquote_symbol_id_;
    Symbol::UniqueId macro_symbol_id_;
    Symbol::UniqueId defconstant_symbol_id_;
    Symbol::UniqueId await_symbol_id_;
    Symbol::UniqueId apply_symbol_id_;
    Symbol::UniqueId map_symbol_id_;
    Symbol::UniqueId foreach_symbol_id_;

    u16 string_buffer_remaining_ = 0;
    u16 arguments_break_loc_;
    u8 current_fn_argc_ = 0;
    bool strict_ : 1 = false;
    bool callstack_untouched_ : 1 = true;
    bool critical_gc_alert_ : 1 = false;
    bool debug_break_ = false;
    bool debug_mode_ = false;
};


static Optional<Context> bound_context;
#define L_CTX (*bound_context)


Value*& get_bytecode_buffer()
{
    return L_CTX.bytecode_buffer_;
}


void reset_operand_stack()
{
    L_CTX.operand_stack_->clear();

    // Push a few nil onto the operand stack. Allows us to access the first few
    // elements of the operand stack without performing size checks.
    push_op(get_nil());
    push_op(get_nil());
}


static void invoke_finalizer(Value* value);
void value_pool_free(Value* value);


static void collect_value(Value* value)
{
    invoke_finalizer(value);
    value_pool_free(value);
}


static void push_callstack(Value* function)
{
    push_op(function); // GC protect
    L_CTX.callstack_ = make_cons(function, L_CTX.callstack_);
    pop_op(); // gc unprotect
}


static void reset_callstack()
{
    L_CTX.current_fn_argc_ = 0;
    L_CTX.arguments_break_loc_ = 0;
    L_CTX.callstack_ = L_NIL;
    push_callstack(make_string_from_literal("toplevel"));
}


static void pop_callstack()
{
    auto old = L_CTX.callstack_;

    L_CTX.callstack_ = L_CTX.callstack_->cons().cdr();

    if (L_CTX.callstack_untouched_) {
        // If nothing referenced the callstack by calling stacktrace(), then we
        // can deallocate the memory reserved for the stack frame right away.
        collect_value(old);
    }

    if (length(L_CTX.callstack_) == 1) {
        // If we're returning back to the toplevel, invoke the GC sometimes if
        // it looks like we're running low on lisp values...
        //
        // Returning to the toplevel should be a very safe place to run the
        // collector--we aren't in the middle of running any interesting code.
        gc_safepoint();
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
    if (L_CTX.native_interface_.lookup_function_ not_eq
        native_interface_fn_lookup_default) {
        PLATFORM.fatal("only one NativeInterface may be registered at a time!");
    }
    L_CTX.native_interface_ = ni;
}


void register_external_symtab(const char* data, u32 len)
{
    L_CTX.external_symtab_contents_ = data;
    L_CTX.external_symtab_size_ = len;
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
    Value* temp = L_CTX.tree_nullnode_;

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
    if (L_CTX.globals_tree_ == get_nil()) {

        if (not define_var) {
            return false;
        }

        Protected new_kvp(make_cons(key, value));

        // The empty set of left/right children
        push_op(make_cons(get_nil(), get_nil()));

        auto new_tree = make_cons(new_kvp, get_op0());
        pop_op();

        L_CTX.globals_tree_ = new_tree;

        return true;

    } else {
        auto pt = globals_tree_splay(L_CTX.globals_tree_, key);

        if (key->symbol().unique_id() < TKEY(pt)) {
            Protected new_kvp(make_cons(key, value));
            Protected children(make_cons(get_nil(), get_nil()));
            auto node = make_cons(new_kvp, children);
            SLST(node, LST(pt));
            SRST(node, pt);
            SLST(pt, get_nil());
            L_CTX.globals_tree_ = node;
            if (not define_var) {
                return false;
            }
        } else if (key->symbol().unique_id() > TKEY(pt)) {
            Protected new_kvp(make_cons(key, value));
            Protected children(make_cons(get_nil(), get_nil()));
            auto node = make_cons(new_kvp, children);
            SRST(node, RST(pt));
            SLST(node, pt);
            SRST(pt, get_nil());
            L_CTX.globals_tree_ = node;
            if (not define_var) {
                return false;
            }
        } else {
            pt->cons().car()->cons().set_cdr(value);
            L_CTX.globals_tree_ = pt;
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
    if (L_CTX.globals_tree_ == get_nil()) {
        return;
    }

    L_CTX.globals_tree_ = globals_tree_splay(L_CTX.globals_tree_, key);

    if (TKEY(L_CTX.globals_tree_) != key->symbol().unique_id()) {
        return;
    }

    // Key is now at the root, remove it by joining left and right subtrees
    Value* left = LST(L_CTX.globals_tree_);
    Value* right = RST(L_CTX.globals_tree_);

    if (left == get_nil()) {
        L_CTX.globals_tree_ = right;
    } else if (right == get_nil()) {
        L_CTX.globals_tree_ = left;
    } else {
        // Both children exist - splay the maximum element of left subtree
        // to bring it to the root of left subtree (it will have no right child)
        // This is typically done by splaying with a key larger than all keys
        left = globals_tree_splay(left, key); // Will end up at rightmost

        // Actually, we need to find max of left subtree more explicitly
        // Splay left tree with the key we're deleting, which will bring
        // the largest element < key to the root
        SRST(left, right);
        L_CTX.globals_tree_ = left;
    }
}


static Value* globals_tree_find(Value* key)
{
    if (L_CTX.globals_tree_ == get_nil()) {
        return nullptr;
    }

    auto pt = globals_tree_splay(L_CTX.globals_tree_, key);
    L_CTX.globals_tree_ = pt;
    if (key->symbol().unique_id() ==
        pt->cons().car()->cons().car()->symbol().unique_id()) {
        return pt->cons().car()->cons().cdr();
    }

    return nullptr;
}


bool is_error(Value* val)
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
    return L_CTX.nil_;
}


void get_interns(::Function<6 * sizeof(void*), void(const char*)> callback)
{
    if (L_CTX.string_intern_table_) {
        const char* search = (*L_CTX.string_intern_table_)->data_;
        for (int i = 0; i < L_CTX.string_intern_pos_;) {
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
    auto br = L_CTX.arguments_break_loc_;
    auto argc = L_CTX.current_fn_argc_;
    if (br >= ((argc - 1) - n)) {
        return (*L_CTX.operand_stack_)[br - ((argc - 1) - n)];
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
    u32 offset = ptr.offset_; // explicit zero-extend
    return (Value*)((offset << 3) + (uintptr_t)value_pool_data);
#else
    return (Value*)ptr.ptr_;
#endif
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


bool Function::is_compiled() const
{
    return hdr_.mode_bits_ == ModeBits::cpp_function or
           hdr_.mode_bits_ == ModeBits::lisp_bytecode_function;
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


Value* alloc_value()
{
    auto init_val = [](Value* val) {
        val->hdr_.mark_bit_ = false;
        val->hdr_.alive_ = true;
        return val;
    };

    if (auto val = value_pool_alloc()) {
#ifdef MEM_PTR_TRACE
        std::cout << ::format("alloc %", (int)(intptr_t)val).c_str()
                  << std::endl;
#endif
        return init_val(val);
    }

    if (L_CTX.critical_gc_alert_) {
        PLATFORM.fatal("unexpected gc run!");
    }

    // To be honest, this is a bit of a precarious place to run the GC... lots
    // of things call alloc_value, meaning there's lots of code to comb through
    // for gc bugs. We do preemptively invoke the gc when we're running low in
    // some places to avoid having to do this...
    gc();

    // Hopefully, we've freed up enough memory...
    if (auto val = value_pool_alloc()) {
        return init_val(val);
    }

    Platform::fatal("LISP out of memory");

    return nullptr;
}


Value* wrap(void* input, Value* type_sym)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::wrapped;
    val->hdr_.mode_bits_ = (u8)Wrapped::Variant::userdata;
    val->wrapped().userdata_ = input;
    val->wrapped().type_sym_ = compr(type_sym);
    return val;
}


Value* wrap(Value* input, Value* type_sym)
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::wrapped;
    val->hdr_.mode_bits_ = (u8)Wrapped::Variant::lisp_data;
    val->wrapped().lisp_data_ = compr(input);
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
        compr(L_CTX.lexical_bindings_);
    val->hdr_.mode_bits_ = Function::ModeBits::lisp_function;
    return val;
}


Value* clone(Value* value)
{
    // TODO!!!!!
    return value;
}


template <typename... Args> void unrecoverable(const char* msg, Args&&... args)
{
    auto buf = allocate_small<StringBuffer<238>>("error-msg");
    make_format(*buf, msg, std::forward<Args>(args)...);
    PLATFORM.fatal(*buf);
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
                    unrecoverable("invalid type declaration: %",
                                  val_to_string<96>(val));
                } else {
                    sym = val->cons().car();
                }
            } else {
                unrecoverable(
                    "value \'%\' in argument list \'%\' is non-symbol!",
                    val,
                    arg_lat);
            }
        } else {
            sym = val;
        }

        if (not L_CTX.external_symtab_contents_ and
            sym->hdr_.mode_bits_ not_eq (u8) Symbol::ModeBits::small) {
            unrecoverable(
                "symbol name \'%\' in argument list \'%\' is too long! "
                "(4 char limit)",
                sym->symbol().name(),
                arg_lat);
        }
    });

    return argc;
}



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
            } else if (str_eq(type_symbol.name(), "databuffer")) {
                type = Value::Type::databuffer;
            } else if (str_eq(type_symbol.name(), "nil")) {
                type = Value::Type::nil;
            } else if (str_eq(type_symbol.name(), "lambda")) {
                type = Value::Type::function;
            } else if (str_eq(type_symbol.name(), "wrapped")) {
                type = Value::Type::wrapped;
            } else {
                unrecoverable("invalid type symbol %", type_symbol.name());
            }
        } else {
            sym = val;
        }
        if (not b.bindings_.push_back(
                ArgBinding{&sym->symbol(), (u8)arg++, type, false})) {
            PLATFORM.fatal("too many named arguments for function! Max 5");
        }
    });

    return b;
}


static void arg_substitution_impl(Value* impl, ArgBindings& bindings)
{
    while (true) {
        if (impl->type() not_eq Value::Type::cons) {
            break;
        } else {
            auto val = impl->cons().car();

            if (is_list(val)) {
                auto first = val->cons().car();
                if (first->type() == Value::Type::symbol) {
                    auto id = first->symbol().unique_id();
                    if (id == L_CTX.let_symbol_id_) {
                        auto let_bindings = val->cons().cdr()->cons().car();

                        l_foreach(let_bindings, [&](Value* val) {
                            auto sym = val->cons().car();
                            for (auto& binding : bindings.bindings_) {
                                if (sym->symbol().unique_id() ==
                                    binding.sym_->unique_id()) {

                                    unrecoverable(
                                        "let binding % shadows argument %",
                                        binding.sym_->name(),
                                        sym->symbol().name());
                                }
                            }

                            auto current = bindings.parent_;
                            while (current) {
                                for (auto& b : current->bindings_) {
                                    if (b.sym_->unique_id() ==
                                        sym->symbol().unique_id()) {
                                        unrecoverable(
                                            "let binding shadows "
                                            "captured parent arg '%' ",
                                            sym->symbol().name());
                                    }
                                }
                                current = current->parent_;
                            }
                        });

                        arg_substitution_impl(let_bindings, bindings);
                        arg_substitution_impl(val, bindings);
                    } else if (id == L_CTX.lambda_symbol_id_) {
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
                                bind.push_back(
                                    L_CTX.argument_symbols_[binding
                                                                .replacement_]);
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
                            val->cons().set_cdr(L_CONS(
                                closure.result(),
                                L_CONS(L_CONS(make_symbol("fn"), fn_impl),
                                       L_NIL)));
                        }

                    } else if (id == L_CTX.fn_symbol_id_) {
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

                        auto sym =
                            L_CTX.argument_symbols_[binding.replacement_];

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

    Protected val(alloc_value());

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
        compr(L_CTX.lexical_bindings_);

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
        compr(L_CTX.lexical_bindings_);
    val->function().sig_.reset();

    val->function().bytecode_impl_.bytecode_ = compr(bytecode);
    val->hdr_.mode_bits_ = Function::ModeBits::lisp_bytecode_function;
    return val;
}


Value* make_ratio(s32 num, s32 div)
{
    reduce_fraction(num, div);
    Protected numerator(make_integer(num));
    if (div == 1) {
        return numerator;
    }
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::ratio;
    val->ratio().numerator_ = compr(numerator);
    val->ratio().divisor_ = div;
    return val;
}


Value* make_promise()
{
    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::promise;
    val->promise().eval_stack_ = compr(L_NIL);
    val->promise().operand_stack_ = compr(L_NIL);
    val->promise().eval_stack_elems_ = 0;
    val->promise().operand_stack_elems_ = 0;
    return val;
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


static void debug_resume()
{
    L_CTX.debug_break_ = false;
    if (L_CTX.debug_breakpoints_ == L_NIL) {
        L_CTX.debug_mode_ = false;
    }
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
    if (L_CTX.debug_handler_) {
        L_CTX.debug_mode_ = true;
        L_CTX.debug_break_ = true;
        auto& handler = *L_CTX.debug_handler_;
        auto reason = debug::Interrupt::error_occurred;
        auto act = handler(reason, val);
        if (act == debug::Action::resume) {
            debug_resume();
        }
    }
    pop_op(); // unprotect
    return val;
}


Value* make_symbol(const char* name, Symbol::ModeBits mode)
{
#ifdef USE_SYMBOL_CACHE
    for (int i = 0; i < 8; ++i) {
        auto v = L_CTX.symbol_cache_[i];
        if (v not_eq L_NIL and str_eq(name, v->symbol().name())) {
            return v;
        }
    }
#endif

    const auto len = strlen(name);

    if (mode == Symbol::ModeBits::small and len > Symbol::buffer_size) {
        Platform::fatal("Symbol ModeBits small with len > internal buffer");
    }
    if (len <= Symbol::buffer_size) {
        mode = Symbol::ModeBits::small;
    }

    auto val = alloc_value();
    val->hdr_.type_ = Value::Type::symbol;
    val->hdr_.mode_bits_ = (u8)mode;
    val->symbol().set_name(name);

#ifdef USE_SYMBOL_CACHE
    L_CTX.symbol_cache_[L_CTX.symbol_cache_index_] = val;
    L_CTX.symbol_cache_index_ =
        (L_CTX.symbol_cache_index_ + 1) % SYMBOL_CACHE_SIZE;
#endif

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


Value* make_databuffer(const char* sbr_tag)
{
    if (not scratch_buffers_remaining()) {
        // Collect any data buffers that may be lying around.
        gc();
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
    auto free = L_CTX.string_buffer_remaining_;

    if (L_CTX.string_buffer_ not_eq L_NIL) {
        if (free > len + 1) { // +1 for null term, > for other null term
            existing_buffer = L_CTX.string_buffer_;
            L_CTX.string_buffer_remaining_ -= len + 1;
        } else {
            L_CTX.string_buffer_ = L_NIL;
            L_CTX.string_buffer_remaining_ = 0;
        }
    }

    if (existing_buffer) {
        const auto offset = (SCRATCH_BUFFER_SIZE - free) + 1;

        auto write_ptr = existing_buffer->databuffer().value()->data_ + offset;

        memcpy(write_ptr, string, len);

        return {existing_buffer, offset};

    } else {

        // Because we're allocating a fresh buffer, as the prior one was full.
        L_CTX.string_buffer_remaining_ = SCRATCH_BUFFER_SIZE - (len + 1);

        auto buffer = make_databuffer("lisp-string-bulk-allocator");

        Protected p(buffer);
        L_CTX.string_buffer_ = buffer;

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
    L_CTX.operand_stack_->pop_back();
}


void push_op(Value* operand)
{
    L_CTX.operand_stack_->push_back(operand, nullptr, [](void*) {
        Platform::fatal("LISP stack overflow.");
    });
}


void insert_op(u32 offset, Value* operand)
{
    auto& stack = L_CTX.operand_stack_;
    auto pos = stack->end() - offset;
    stack->insert(pos, operand);
}


Value* get_op0()
{
    auto& stack = L_CTX.operand_stack_;
    return stack->back();
}


Value* get_op1()
{
    auto& stack = L_CTX.operand_stack_;
    return *(stack->end() - 2);
}


OperandStackUsed get_op_count()
{
    return L_CTX.operand_stack_->size();
}


Value* get_op(u32 offset)
{
    auto& stack = L_CTX.operand_stack_;
    if (offset >= stack->size()) {
        return get_nil(); // TODO: raise error
    }

    return (*stack)[(stack.obj_->size() - 1) - offset];
}


void lexical_frame_push()
{
    L_CTX.lexical_bindings_ = make_cons(get_nil(), L_CTX.lexical_bindings_);
}


void lexical_frame_pop()
{
    L_CTX.lexical_bindings_ = L_CTX.lexical_bindings_->cons().cdr();
}


void lexical_frame_store(Value* kvp)
{
    L_CTX.lexical_bindings_->cons().set_car(
        make_cons(kvp, L_CTX.lexical_bindings_->cons().car()));
}


const char* type_to_string(ValueHeader::Type tp)
{
    switch (tp) {
    case Value::Type::count:
    case Value::Type::__reserved_1:
    case Value::Type::__reserved_0:
    case Value::Type::nil:
        return "nil";
    case Value::Type::promise:
        return "promise";
    case Value::Type::rational:
        return "rational";
    case Value::Type::ratio:
        return "ratio";
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


struct EvalFrame
{
    Value* expr_;
    enum State : u8 {
        start,
        funcall_apply,
        if_check_branch,
        while_check_condition,
        while_body,
        while_body_discard_result,
        let_install_bindings,
        let_body,
        let_body_discard,
        let_cleanup,
        lisp_funcall_setup,
        lisp_funcall_body,
        lisp_funcall_body_discard,
        lisp_funcall_cleanup,
        await_check_result,
        await_resume,
        pop_root,
        apply_with_list,
        debug_enable_break,
        vm_resume,
        vm_cleanup,
        foreach_iter,
        foreach_iter_init,
        foreach_iter_start,
    } state_;

    struct FuncallApplyParams
    {
        int argc_;
    };

    struct InstallLetParams
    {
        int binding_count_;
    };

    struct LispFuncallCleanupParams
    {
        int argc_;
        int saved_break_loc_;
        u8 saved_argc_;
    };

    struct AwaitResumeParams
    {
        int saved_break_loc_;
        u8 saved_argc_;
    };

    struct VMRestoreStateParams
    {
        int saved_break_loc_;
        u8 saved_argc_;
    };

    using VMResumeParams = ExecutionContext;

    union
    {
        FuncallApplyParams funcall_apply_;
        InstallLetParams install_let_;
        LispFuncallCleanupParams lisp_funcall_cleanup_;
        AwaitResumeParams await_resume_;
        VMResumeParams vm_resume_;
    };
};


using EvalStack = Vector<EvalFrame>;


void eval_loop(EvalStack& eval_stack);


using RestoreDebugBreak = bool;
RestoreDebugBreak debug_break_compiled_fn(Value* obj)
{
    if (L_CTX.debug_handler_ and L_CTX.debug_break_) {
        auto& handler = *L_CTX.debug_handler_;
        auto reason = debug::Interrupt::enter_compiled_function;
        auto act = handler(reason, obj);
        switch (act) {
        case debug::Action::resume:
            debug_resume();
            return false;

        case debug::Action::step_over:
            L_CTX.debug_break_ = false;
            return true;

        case debug::Action::step:
            return true;
        }
    }
    return false;
}


template <typename... Args> void push_error(const char* msg, Args&&... args)
{
    auto buf = allocate_small<StringBuffer<238>>("error-msg");
    make_format(*buf, msg, std::forward<Args>(args)...);
    push_op(make_error(buf->c_str()));
}


// The function arguments should be sitting at the top of the operand stack
// prior to calling funcall. The arguments will be consumed, and replaced with
// the result of the function call.
void funcall(Value* obj, u8 argc)
{
    auto pop_args = [&argc] {
        for (int i = 0; i < argc; ++i) {
            L_CTX.operand_stack_->pop_back();
        }
    };

    // NOTE: The callee must be somewhere on the operand stack, so it's safe
    // to store this unprotected var here.
    Protected prev_bindings(L_CTX.lexical_bindings_);

    auto prev_arguments_break_loc = L_CTX.arguments_break_loc_;
    auto prev_argc = L_CTX.current_fn_argc_;

    push_callstack(obj);

    switch (obj->type()) {
    case Value::Type::function: {
        if (obj->function().sig_.required_args_ > argc) {
            pop_args();
            push_op(make_error(Error::Code::invalid_argc, obj));
            break;
        }

        auto arg_error = [&](auto exp_type, auto got_type) {
            push_error("invalid arg type for %! expected %, got %",
                       obj,
                       type_to_string((ValueHeader::Type)exp_type),
                       type_to_string((ValueHeader::Type)got_type));
        };

#define CHECK_ARG_TYPE(ARG, FIELD)                                             \
    if (obj->function().sig_.FIELD not_eq Value::Type::nil and                 \
        get_arg(ARG)->type() not_eq obj->function().sig_.FIELD and             \
        not(get_arg(ARG)->type() == Value::Type::nil and                       \
            obj->function().sig_.FIELD == Value::Type::cons) and               \
        not(get_arg(ARG)->type() == Value::Type::rational and                  \
            (obj->function().sig_.FIELD == Value::Type::integer or             \
             obj->function().sig_.FIELD == Value::Type::ratio))) {             \
        pop_args();                                                            \
        arg_error(obj->function().sig_.FIELD, get_arg(ARG)->type());           \
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
            const auto break_loc = L_CTX.operand_stack_->size() - 1;
            L_CTX.arguments_break_loc_ = break_loc;
            L_CTX.current_fn_argc_ = argc;
            Value* result = L_NIL;
            if (UNLIKELY(L_CTX.debug_break_)) {
                const auto restore_debug_break = debug_break_compiled_fn(obj);
                result = obj->function().cpp_impl_(argc);
                L_CTX.debug_break_ = restore_debug_break;
            } else {
                result = obj->function().cpp_impl_(argc);
            }
            pop_args();
            push_op(result);
            break;
        }

        case Function::ModeBits::lisp_function: {
            PLATFORM_EXTENSION(stack_check);
            pop_callstack(); // lisp_funcall_setup handles it

            // State machine expects prev_bindings and the function itself on
            // the stack above the args.
            insert_op(argc, prev_bindings);
            insert_op(argc, obj);

            EvalStack eval_stack(make_scratch_buffer("eval-stack-buffer"));

            eval_stack.push_back(
                {.expr_ = obj,
                 .state_ = EvalFrame::lisp_funcall_cleanup,
                 .lisp_funcall_cleanup_ = {
                     argc, prev_arguments_break_loc, prev_argc}});

            eval_stack.push_back({.expr_ = obj,
                                  .state_ = EvalFrame::lisp_funcall_setup,
                                  .funcall_apply_ = {argc}});

            eval_loop(eval_stack);

            // lisp_funcall_cleanup handles all of the cleanup that would
            // normally happen at the bottom of funcall.
            return;
        }

        case Function::ModeBits::lisp_bytecode_function: {
            gc_safepoint();

            const auto break_loc = L_CTX.operand_stack_->size() - 1;
            L_CTX.arguments_break_loc_ = break_loc;
            L_CTX.current_fn_argc_ = argc;

            bool restore_debug_break = false;
            if (UNLIKELY(L_CTX.debug_break_)) {
                restore_debug_break = debug_break_compiled_fn(obj);
            }

            if (L_CTX.strict_) {
                CHECK_ARG_TYPES();
            }

            L_CTX.lexical_bindings_ =
                dcompr(obj->function().lisp_impl_.lexical_bindings_);

            auto suspend =
                vm_execute(obj->function().bytecode_impl_.databuffer(),
                           obj->function()
                               .bytecode_impl_.bytecode_offset()
                               ->integer()
                               .value_);
            if (suspend) {
                PLATFORM.fatal("cannot suspend from here!");
            }

            L_CTX.debug_break_ = restore_debug_break;

            auto result = get_op0();
            pop_op();
            pop_args();
            push_op(result);
            break;
        }
        }
        break;
    }

    case Value::Type::wrapped: {
        if (obj->hdr_.mode_bits_ == (u8)Wrapped::Variant::userdata) {
            pop_args();
            push_op(make_error("cannot invoke wrapped userdata"));
            break;
        }
        auto sym_name = dcompr(obj->wrapped().type_sym_)->symbol().name();
        if (auto handler =
                get_var(::format<64>("-invoke-%", sym_name).c_str())) {
            insert_op(argc, obj);
            funcall(handler, argc + 1);
        } else {
            pop_args();
            push_error("missing -invoke-%", sym_name);
        }
        break;
    }

    default:
        pop_args();
        push_op(make_error(Error::Code::value_not_callable, obj));
        break;
    }

    pop_callstack();
    L_CTX.lexical_bindings_ = prev_bindings;
    L_CTX.arguments_break_loc_ = prev_arguments_break_loc;
    L_CTX.current_fn_argc_ = prev_argc;
}


void resolve_promise_safe(Value* pr, Value* val)
{
    resolve_promise(pr, val);
    if (is_error(get_op0())) {
        const char* tag = "lisp-fmt-buffer";
        auto p = allocate<DefaultPrinter>(tag);
        format(get_op0(), *p);
        Platform::fatal(p->data_.c_str());
    }
}


const char* nameof(Value* value);


bool contains(Value* list, Value* val)
{
    bool result = false;
    l_foreach(list, [&](Value* v2) {
        if (is_equal(val, v2)) {
            result = true;
        }
    });

    return result;
}


void safecall(Value* fn, u8 argc)
{
    if (fn->type() not_eq Value::Type::function) {
        Platform::fatal("attempt to call non-function!");
    }

    if (L_CTX.operand_stack_->size() < argc) {
        Platform::fatal("invalid argc for safecall");
    }

    if (L_CTX.debug_mode_) {
        auto breakpoints = L_CTX.debug_breakpoints_;
        if (breakpoints not_eq L_NIL and not L_CTX.debug_break_) {
            if (auto detect_name = nameof(fn)) {
                bool is_breakpoint = false;
                Value* br_sym = L_NIL;
                l_foreach(breakpoints, [&](Value* v) {
                    if (v->type() == Value::Type::symbol) {
                        if (str_eq(v->symbol().name(), detect_name)) {
                            is_breakpoint = true;
                            br_sym = v;
                        }
                    }
                });
                if (is_breakpoint) {
                    if (L_CTX.debug_handler_) {
                        auto& handler = *L_CTX.debug_handler_;
                        auto reason = debug::Interrupt::breakpoint;
                        handler(reason, br_sym);
                    }
                    L_CTX.debug_break_ = true;
                }
            }
        }
    }


    lisp::funcall(fn, argc);
    auto result = lisp::get_op(0);

    if (is_error(result)) {
        const char* tag = "lisp-fmt-buffer";
        auto p = allocate<DefaultPrinter>(tag);
        format(result, *p);
        Platform::fatal(p->data_.c_str());
    }
}


u8 get_argc()
{
    return L_CTX.current_fn_argc_;
}


Value* get_this()
{
    return L_CTX.callstack_->cons().car();
}


Value* get_var_stable(const char* intern_str)
{
    return get_var(make_symbol(intern_str, Symbol::ModeBits::stable_pointer));
}


Value* get_var(const char* name)
{
    auto var_sym = make_symbol(name);
    if (var_sym->type() not_eq Value::Type::symbol) {
        return var_sym;
    }

    auto val = get_var(var_sym);
    // collect_value(var_sym);
    return val;
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



bool lint_type_check(Value* expr, u8 expected_type, int slot)
{
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
            if (str_eq(arg->cons().car()->symbol().name(), "'") and
                arg->cons().cdr()->type() == Value::Type::symbol) {
                return true;
            }
        }
    }
    auto detected_type = arg->hdr_.type();
    if (is_list(arg)) {
        auto first = get_list(arg, 0);
        if (first->type() == Value::Type::symbol) {
            auto name = first->symbol().name();

            if (str_eq(name, "lambda") or str_eq(name, "fn")) {
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
                    if (v->type() == Value::Type::function) {
                        auto& fn = v->function();
                        if (fn.sig_.ret_type_ == Value::Type::nil) {
                            return true;
                        } else {
                            detected_type = (Value::Type)fn.sig_.ret_type_;
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
    if (expected_type == Value::Type::rational and
        (detected_type == Value::Type::integer or
         detected_type == Value::Type::ratio)) {
        // Special case: integers can be promoted to ratios.
        return true;
    }
    return detected_type == expected_type;
}



bool lint_find_variable(Value* cv, Value* variable_list, Protected& gvar_list)
{
    if (cv->type() == Value::Type::symbol) {
        bool found = false;
        l_foreach(variable_list, [cv, &found](Value* v) {
            if (cv->symbol().unique_id() == v->symbol().unique_id()) {
                found = true;
            }
        });
        l_foreach(gvar_list, [cv, &found](Value* v) {
            if (cv->symbol().unique_id() == v->symbol().unique_id()) {
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
}


StringBuffer<128> lint_arg_error(Value* expr, Value* fn, int arg, u8 expected)
{
    auto tp = (ValueHeader::Type)expected;
    return ::format("invalid arg % type for %! "
                    "expected %, got %",
                    arg,
                    val_to_string<64>(fn).c_str(),
                    type_to_string(tp),
                    val_to_string<64>(get_list(expr, arg + 1)).c_str());
}


void lint_push_arg_error(Value* expr, Value* fn, int arg, u8 exp)
{
    push_op(make_error(lint_arg_error(expr, fn, arg, exp).c_str()));
}


bool lint_check_let_bindings(Value* expr,
                             Value*& variable_list,
                             lisp::Protected& gvar_list)
{
    if (length(expr) < 2) {
        push_op(make_error("malformed let expr!"));
        return false;
    }
    auto bindings = get_list(expr, 1);
    while (bindings not_eq L_NIL) {
        if (bindings->type() not_eq Value::Type::cons) {
            push_op(make_error("invalid let binding list"));
            return false;
        }
        auto binding = bindings->cons().car();
        if (not is_list(binding)) {
            push_op(make_error("invalid let binding list"));
            return false;
        }
        auto sym = get_list(binding, 0);
        if (sym->type() == Value::Type::symbol) {
            variable_list = L_CONS(sym, variable_list);
        } else if (is_list(sym)) {
            auto destructure_list = sym;
            while (destructure_list not_eq L_NIL) {
                auto dsym = destructure_list->cons().car();
                if (dsym->type() not_eq Value::Type::symbol) {
                    push_op(make_error("non-symbol in "
                                       "destructuring let"));
                    return false;
                }
                variable_list = L_CONS(dsym, variable_list);
                destructure_list = destructure_list->cons().cdr();
            }
        } else if (sym->type() == Value::Type::cons) {
            auto car_sym = sym->cons().car();
            auto cdr_sym = sym->cons().cdr();
            if (car_sym->type() == Value::Type::symbol and
                cdr_sym->type() == Value::Type::symbol) {
                variable_list = L_CONS(car_sym, variable_list);
                variable_list = L_CONS(cdr_sym, variable_list);
            } else if (car_sym->type() == Value::Type::symbol and
                       cdr_sym->type() == Value::Type::cons) {
                auto lat = sym;
                while (lat->type() == Value::Type::cons) {
                    auto lsym = lat->cons().car();
                    if (lsym->type() not_eq Value::Type::symbol) {
                        push_error("invalid value % "
                                   "in destructuring "
                                   "let %",
                                   lsym,
                                   sym);
                        return false;
                    } else {
                        variable_list = L_CONS(lsym, variable_list);
                    }
                    lat = lat->cons().cdr();
                }
                if (lat->type() not_eq Value::Type::symbol) {
                    push_error("invalid value % in "
                               "destructuring let %",
                               lat,
                               sym);
                    return false;
                } else {
                    variable_list = L_CONS(lat, variable_list);
                }
            } else {
                push_op(make_error("pair in destructuring let "
                                   "must contain symbols!"));
                return false;
            }
        } else {
            push_op(make_error("let binding missing symbol"));
            return false;
        }
        bindings = bindings->cons().cdr();
    }
    return true;
}


// Checks for undefined variable access, checks to make sure enough function
// params are supplied, checks the proper structure of special forms, etc...
void lint(Value* expr, Value* variable_list, lisp::Protected& gvar_list)
{
    push_op(variable_list);
    gc_safepoint();
    pop_op(); // variable_list

    bool is_special_form = false;

    switch (expr->type()) {
    case Value::Type::cons:
        if (is_list(expr)) {
            auto fn_sym = get_list(expr, 0);
            if (fn_sym->type() == Value::Type::cons and is_list(fn_sym)) {
                lint(fn_sym, variable_list, gvar_list);
                if (get_op0()->type() == Value::Type::error) {
                    return;
                }
                pop_op();
            } else if (fn_sym->type() == Value::Type::symbol) {
                auto name = fn_sym->symbol().name();

                if (str_eq(name, "setfn") or str_eq(name, "set-temp") or
                    str_eq(name, "defconstant")) {
                    auto pair = get_list(expr, 1);
                    if (pair->type() == Value::Type::cons) {
                        auto sym = pair->cons().cdr();
                        if (sym->type() == Value::Type::symbol) {
                            gvar_list = L_CONS(sym, gvar_list);
                        }
                    }
                }

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
                    if (not lint_check_let_bindings(
                            expr, variable_list, gvar_list)) {
                        return;
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
                } else if (str_eq(name, "await")) {
                    if (not lint_type_check(expr, Value::Type::promise, 0)) {
                        push_op(make_error(
                            "await requires input of type promise!"));
                        return;
                    }
                    is_special_form = true;
                }

                auto fn = L_NIL;
                if (not is_special_form) {
                    fn = get_var(fn_sym);
                }
                if (fn->type() == Value::Type::function) {
                    int reqd_args = fn->function().sig_.required_args_;
                    if (length(expr) - 1 < reqd_args) {
                        push_error("insufficient args for %!", fn);
                        return;
                    }
                    auto& fn_var = fn->function();

                    if (fn_var.sig_.arg0_type_ not_eq ValueHeader::Type::nil) {
                        if (not lint_type_check(
                                expr, fn_var.sig_.arg0_type_, 0)) {
                            lint_push_arg_error(
                                expr, fn, 0, fn_var.sig_.arg0_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg1_type_ not_eq ValueHeader::Type::nil) {
                        if (not lint_type_check(
                                expr, fn_var.sig_.arg1_type_, 1)) {
                            lint_push_arg_error(
                                expr, fn, 1, fn_var.sig_.arg1_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg2_type_ not_eq ValueHeader::Type::nil) {
                        if (not lint_type_check(
                                expr, fn_var.sig_.arg2_type_, 2)) {
                            lint_push_arg_error(
                                expr, fn, 2, fn_var.sig_.arg2_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.arg3_type_ not_eq ValueHeader::Type::nil) {
                        if (not lint_type_check(
                                expr, fn_var.sig_.arg3_type_, 3)) {
                            lint_push_arg_error(
                                expr, fn, 3, fn_var.sig_.arg3_type_);
                            return;
                        }
                    }
                    if (fn_var.sig_.ret_type_ not_eq ValueHeader::Type::nil) {
                        // TODO...
                    }
                }
            }



            if (not is_special_form) {
                auto cv = expr->cons().car();
                if (not lint_find_variable(cv, variable_list, gvar_list)) {
                    push_error("invalid variable access: %", cv);
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

                            if (not lint_find_variable(
                                    set_sym, variable_list, gvar_list)) {
                                push_error("set for unknown variable %",
                                           set_sym->symbol().name());
                                return;
                            }
                        }
                    }
                }
            }

            expr = expr->cons().cdr();
            while (expr not_eq L_NIL) {

                auto cv = expr->cons().car();

                if (not lint_find_variable(cv, variable_list, gvar_list)) {
                    push_error("invalid variable access: %",
                               cv->symbol().name());
                    return;
                }

                lint(cv, variable_list, gvar_list);

                if (get_op0()->type() == Value::Type::error) {
                    return;
                }
                pop_op();
                expr = expr->cons().cdr();
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
    Protected gvar_list = L_NIL;

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
        lint(reader_result, varlist, gvar_list);
        auto expr_result = get_op0();
        result.set(expr_result);
        pop_op(); // expression result
        pop_op(); // reader result

        if (is_error(expr_result)) {
            Protected p(expr_result);
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
    int i = 0;

    Protected result(get_nil());

    auto prev_stk = L_CTX.operand_stack_->size();

    while (true) {
        const auto last_i = i;
        {
            // NOTE: we need to disable breakpoints during expression read,
            // because it's tedious to look at evals triggered during
            // macroexpansion.
            const bool was_debug = L_CTX.debug_mode_;
            L_CTX.debug_mode_ = false;
            i += read(code, i);
            L_CTX.debug_mode_ = was_debug;
        }
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
            result = expr_result;
            const auto current_line = error_find_linenum(code, last_i);
            error_append_line_hint(expr_result->error(), current_line);
            push_op(expr_result);
            on_error(*expr_result);
            pop_op();
            break;
        }
        gc_safepoint();
    }

    if (L_CTX.strict_ and L_CTX.operand_stack_->size() not_eq prev_stk) {
        PLATFORM.fatal(::format<64>(
            "stack spill! % %", L_CTX.operand_stack_->size(), prev_stk));
    }

    return result;
}


void format_impl(Value* value, Printer& p, int depth, bool skip_quotes = false)
{
    if (not value->hdr_.alive_) {
        return;
    }

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

    case lisp::Value::Type::promise:
        p.put_str("#promise");
        break;

    case lisp::Value::Type::rational:
    case lisp::Value::Type::__reserved_1:
    case lisp::Value::Type::__reserved_0:
        break;

    case lisp::Value::Type::wrapped: {
        auto type = dcompr(value->wrapped().type_sym_);

        auto decorator_fn =
            get_var(::format<64>("-decorate-%", type->symbol().name()).c_str());

        if (decorator_fn->type() == Value::Type::function) {
            push_op(value); // argument
            safecall(decorator_fn, 1);
            format_impl(get_op0(), p, depth + 1, true);
            pop_op(); // result
        } else {
            Platform::fatal(::format<64>("missing decorator function for %",
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

    case lisp::Value::Type::ratio:
        format_impl(dcompr(value->ratio().numerator_), p, depth);
        p.put_str("/");
        p.put_str(to_string<32>(value->ratio().divisor_).c_str());
        break;

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
#if defined(__GBA__) or defined(__linux__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
                ptr[i] = *(name++);
#if defined(__GBA__) or defined(__linux__)
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

    value->hdr_.mark_bit_ = true;

    switch (value->type()) {
    case Value::Type::wrapped:
        if (value->hdr_.mode_bits_ == (u8)Wrapped::Variant::lisp_data) {
            gc_mark_value(dcompr(value->wrapped().lisp_data_));
        }
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

    case Value::Type::promise:
        gc_mark_value(dcompr(value->promise().eval_stack_));
        gc_mark_value(dcompr(value->promise().operand_stack_));
        for (int i = 0; i < value->promise().eval_stack_elems_; ++i) {
            auto frame = value->promise().load_eval_frame(i);
            gc_mark_value(frame.expr_);
        }
        for (int i = 0; i < value->promise().operand_stack_elems_; ++i) {
            gc_mark_value(value->promise().load_operand(i));
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

    case Value::Type::ratio:
        gc_mark_value(dcompr(value->ratio().numerator_));
        break;

    default:
        break;
    }
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
    gc_mark_value(L_CTX.nil_);
    gc_mark_value(L_CTX.lexical_bindings_);
    gc_mark_value(L_CTX.macros_);
    gc_mark_value(L_CTX.tree_nullnode_);
    gc_mark_value(L_CTX.debug_breakpoints_);
    gc_mark_value(L_CTX.debug_watchpoints_);

    for (auto& sym : L_CTX.argument_symbols_) {
        gc_mark_value(sym);
    }

#ifdef USE_SYMBOL_CACHE
    for (auto v : L_CTX.symbol_cache_) {
        gc_mark_value(v);
    }
#endif

    for (auto elem : *L_CTX.operand_stack_) {
        gc_mark_value(elem);
    }

    globals_tree_traverse(L_CTX.globals_tree_, [](Value& car, Value& node) {
        node.hdr_.mark_bit_ = true;
        node.cons().cdr()->hdr_.mark_bit_ = true;
        gc_mark_value(&car);
    });

    gc_mark_value(L_CTX.callstack_);

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
    if (not scratch_buffers_remaining()) {
        // We cannot allocate a databuffer to compact into if we're oom.
        return 0;
    }

    // The interpreter uses bump allocation when allocating strings. This can
    // cause fragmentation, and after collecting lisp objects, we should squeeze
    // the resulting gaps out of the memory region used for storing strings.

    for (auto& v : *L_CTX.operand_stack_) {
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

    L_CTX.string_buffer_ = db;
    L_CTX.string_buffer_remaining_ = (SCRATCH_BUFFER_SIZE - (write_offset + 1));

    for (auto& b : recovered_buffers) {
        collect_value(b);
    }

    return recovered_buffers.size();
}


static int gc_sweep()
{
    if (not L_CTX.string_buffer_->hdr_.mark_bit_) {
        L_CTX.string_buffer_ = L_NIL;
        L_CTX.string_buffer_remaining_ = 0;
    }

    if (not L_CTX.bytecode_buffer_->hdr_.mark_bit_) {
        L_CTX.bytecode_buffer_ = L_NIL;
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
#ifdef MEM_PTR_TRACE
                std::cout << ::format("free % ", (int)(intptr_t)val).c_str()
                          << std::endl;
#endif
                collect_value(val);
                ++collect_count;
            }
        }
    }

    L_CTX.callstack_untouched_ = true;

    if (used_count > L_CTX.alloc_highwater_) {
        L_CTX.alloc_highwater_ = used_count;
        info(::format<32>("LISP mem %", used_count));
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


static bool gc_running;
int gc()
{
    if (gc_running) {
        return 0;
    }
    gc_running = true;

    l_foreach(get_var("--autoload-symbols"), [](Value* sym) {
        if (sym->type() == Value::Type::symbol) {
            if (globals_tree_find(sym)) {
                globals_tree_erase(sym);
            }
        }
    });

    gc_mark();
    int collect_count = gc_sweep();
    gc_running = false;

    return collect_count;
}


template <typename F> void foreach_string_intern(F&& fn)
{
    if (L_CTX.external_symtab_contents_) {
        const char* search = L_CTX.external_symtab_contents_;
        for (u32 i = 0; i < L_CTX.external_symtab_size_;) {
            fn(search + i);
            i += 32;
        }
    }

    if (L_CTX.string_intern_table_) {
        char* const interns = (*L_CTX.string_intern_table_)->data_;
        char* str = interns;

        while (static_cast<u32>(str - interns) < string_intern_table_size and
               static_cast<s32>(str - interns) < L_CTX.string_intern_pos_ and
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

    gc_safepoint();

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

            if (L_CTX.string_intern_table_ and
                id >= (*L_CTX.string_intern_table_)->data_ and
                id < (*L_CTX.string_intern_table_)->data_ +
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
    StringBuffer<64> num_str2;

    bool is_fp = false;
    bool is_ratio = false;

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
            if (is_ratio) {
                num_str2.push_back(code[offset + i++]);
            } else {
                num_str.push_back(code[offset + i++]);
            }
            break;

        case '/': {
            if (is_ratio) {
                goto FINAL;
            }
            is_ratio = true;
            ++i;
            break;
        };

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

    auto parse_int = [](auto& str) {
        s32 result = 0;
        for (u32 i = 0; i < str.length(); ++i) {
            result = result * 10 + (str[i] - '0');
        }
        return result;
    };

    if (is_ratio) {
        push_op(make_ratio(parse_int(num_str), parse_int(num_str2)));
    } else if (is_fp) {
        push_op(L_FP(atof(num_str.c_str())));
    } else if (num_str.length() > 1 and num_str[1] == 'x') {
        push_op(make_integer(hexdec((const u8*)num_str.begin() + 2)));
    } else {
        push_op(make_integer(parse_int(num_str)));
    }

    return i;
}


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
            // This following line... when I originally wrote the code, I
            // thought it was needed, but perhaps not...?
            // macroexpand()
            // Here's my thinking:
            // macroexpand itself calls macroexpand_macro after macro expanding,
            // you wouldn't have to macroexpand again after macroexapnd_macro in
            // macroexpand_macro because the macroexpand called by
            // macroexpand_macro called macroexpand_macro which called
            // macroexpand... right?
            result.push_back(get_op0());
            pop_op();
        } else {
            result.push_back(car_val);
        }
    }

    pop_op();
    push_op(result.result());
}


static void eval_let_recursive(Value*);


// Argument: list on operand stack
// result: list on operand stack
static void macroexpand()
{
    // NOTE: I know this function looks complicated. But it's really not too
    // bad.

    auto lat = get_op0();

    if (lat->cons().car()->type() == Value::Type::symbol) {

        auto macros = L_CTX.macros_;
        for (; macros not_eq get_nil(); macros = macros->cons().cdr()) {

            // if Symbol matches?
            if (macros->cons().car()->cons().car()->symbol().unique_id() ==
                lat->cons().car()->symbol().unique_id()) {

                auto supplied_macro_args = lat->cons().cdr();

                auto macro = macros->cons().car()->cons().cdr();
                auto macro_args = macro->cons().car();

                if (length(macro_args) > length(supplied_macro_args)) {
                    pop_op();
                    Protected error_str = make_string("invalid arguments "
                                                      "passed to macro");
                    push_op(make_error(Error::Code::invalid_syntax, error_str));
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

                eval_let_recursive(synthetic_let.result());

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
    if (v->type() == Value::Type::ratio) {
        dcompr(v->ratio().numerator_)->integer().value_ *= -1;
    } else if (v->type() == Value::Type::fp) {
        v->fp().value_ *= -1;
    } else {
        v->integer().value_ *= -1;
    }
}


u32 read(CharSequence& code, int offset)
{
    int i = 0;

    gc_safepoint();

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


// The old recursive implementation of eval_let. Our new eval implementation is
// stack-based, but we still need this older implementation in some cases.
static void eval_let_recursive(Value* code)
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
                                              L_CTX.lexical_bindings_);

            if (is_error(new_binding_list)) {
                push_op(new_binding_list);
                return;
            } else {
                L_CTX.lexical_bindings_ = new_binding_list;
            }
        }
    }

    l_foreach(code->cons().cdr(), [&](Value* val) {
        eval(val);
        result.set(get_op0());
        pop_op();
    });

    if (has_bindings) {
        L_CTX.lexical_bindings_ = L_CTX.lexical_bindings_->cons().cdr();
    }

    push_op(result);
}


static void eval_macro(Value* code)
{
    if (code->cons().car()->type() == Value::Type::symbol) {
        L_CTX.macros_ = make_cons(code, L_CTX.macros_);
        push_op(get_nil());
    } else {
        // TODO: raise error!
        PLATFORM.fatal("invalid macro format");
    }
}


// NOTE: eval_quasiquote is implemented recursively, unlike the rest of
// eval(). Quasiquote is fairly complex, and typically there's no need to yield
// execution or capture continuations within quasiquote expansion...
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


Value* Promise::load_operand(int slot)
{
    char* mem = dcompr(operand_stack_)->databuffer().value()->data_;
    Value* result;
    memcpy(&result, mem + slot * sizeof(result), sizeof(result));
    return result;
}


void Promise::store_operand(int slot, Value* v)
{
    char* mem = dcompr(operand_stack_)->databuffer().value()->data_;
    memcpy(mem + slot * sizeof(Value*), &v, sizeof(v));
}


EvalFrame Promise::load_eval_frame(int slot)
{
    char* mem = dcompr(eval_stack_)->databuffer().value()->data_;
    EvalFrame result;
    memcpy(&result, mem + slot * sizeof(result), sizeof(result));
    return result;
}


void Promise::store_eval_frame(int slot, const EvalFrame& frame)
{
    char* mem = dcompr(eval_stack_)->databuffer().value()->data_;
    memcpy(mem + slot * sizeof(frame), &frame, sizeof frame);
}



void setup_promise(Promise& pr, EvalStack& eval_stack)
{
    static_assert(sizeof(Context::OperandStack) <= SCRATCH_BUFFER_SIZE);
    // NOTE: checked in can_suspend instead.
    // static_assert(sizeof(EvalFrame) * 256 < SCRATCH_BUFFER_SIZE);

    pr.operand_stack_ = compr(make_databuffer("op-stack-copy"));
    pr.eval_stack_ = compr(make_databuffer("ev-stack-copy"));
    pr.operand_stack_elems_ = L_CTX.operand_stack_->size();
    pr.eval_stack_elems_ = eval_stack.size();

    for (u32 i = 0; i < L_CTX.operand_stack_->size(); ++i) {
        pr.store_operand(i, (*L_CTX.operand_stack_)[i]);
    }
    for (u32 i = 0; i < eval_stack.size(); ++i) {
        pr.store_eval_frame(i, eval_stack[i]);
    }
}


void eval_loop(EvalStack& eval_stack);


void resolve_promise(Value* pr, Value* result)
{
    if (pr->type() not_eq Value::Type::promise) {
        error("invalid argument to resolve_promise");
    }

    auto& promise = pr->promise();
    if (promise.eval_stack_elems_ == 0 or promise.operand_stack_elems_ == 0 or
        dcompr(promise.operand_stack_) == L_NIL or
        dcompr(promise.eval_stack_) == L_NIL) {
        info("broken promise!");
        // Uninitialized promise! Nowhere for the result to go!?
        return;
    }

    L_CTX.operand_stack_->clear();
    for (int i = 0; i < promise.operand_stack_elems_; ++i) {
        L_CTX.operand_stack_->push_back(promise.load_operand(i));
    }

    EvalStack eval_stack;
    for (int i = 0; i < promise.eval_stack_elems_; ++i) {
        eval_stack.push_back(promise.load_eval_frame(i));
    }

    if (get_op0()->type() not_eq Value::Type::promise) {
        LOGIC_ERROR();
    }
    pop_op();        // pop the promise
    push_op(result); // replace promise on stack with result
    eval_loop(eval_stack);

    // Because only this promise value retained references to these two
    // databuffers, they can be collected prematurely to free up databuffer
    // memory. Otherwise, scratch buffers will accumulate in discarded promise
    // values until the gc runs, wasting a lot of memory.
    collect_value(dcompr(promise.operand_stack_));
    collect_value(dcompr(promise.eval_stack_));
    promise.operand_stack_ = compr(L_NIL);
    promise.eval_stack_ = compr(L_NIL);
    promise.operand_stack_elems_ = 0;
    promise.eval_stack_elems_ = 0;

    // info(stringify(L_CTX.operand_stack_->size()));
}


bool can_suspend(EvalStack& eval_stack, StringBuffer<48>& agitant)
{
    static const u32 max_eval_suspend_stack =
        SCRATCH_BUFFER_SIZE / sizeof(EvalFrame);

    if (eval_stack.size() > max_eval_suspend_stack) {
        agitant = "eval stack too deep";
        return false;
    }

    if (L_CTX.operand_stack_->size() > 255) {
        agitant = "operand stack too deep";
        return false;
    }

    if (length(L_CTX.callstack_) == 1) {
        if (L_CTX.callstack_->cons().car()->type() not_eq
            lisp::Value::Type::function) {
            agitant = "cannot suspend from toplevel";
            // We're at the toplevel, and cannot suspend
            return false;
        }
    }

    bool can_suspend = true;
    l_foreach(L_CTX.callstack_, [&](Value* v) {
        if (v->type() == Value::Type::function) {
            if (v->hdr_.mode_bits_ not_eq Function::ModeBits::lisp_function) {
                can_suspend = false;
                agitant = val_to_string<48>(v);
            }
        }
    });
    if (not can_suspend) {
        return false;
    }
    return true;
}


void eval(Value* code_root)
{
    push_op(code_root); // gc protect

    EvalStack eval_stack(make_scratch_buffer("eval-stack-buffer"));
    eval_stack.push_back({code_root, EvalFrame::pop_root});
    eval_stack.push_back({code_root, EvalFrame::start});

    eval_loop(eval_stack);
}


#ifdef __GBA__
__attribute__((always_inline))
#endif
static inline void
eval_iter_start(EvalFrame& frame, EvalStack& eval_stack)
{
    gc_safepoint();

#define PUSH_ERR(V, C) push_op(make_error((V), (C)))

    auto code = frame.expr_;
    if (code->type() == Value::Type::symbol) {
        push_op(get_var(code));
    } else if (code->type() == Value::Type::cons) {
        auto code = frame.expr_;
        auto form = code->cons().car();
        if (form->type() == Value::Type::symbol) {
            auto id = form->symbol().unique_id();
            if (id == L_CTX.if_symbol_id_) {
                auto if_code = code->cons().cdr();

                if (if_code->type() != Value::Type::cons) {
                    PUSH_ERR(Error::Code::mismatched_parentheses, L_NIL);
                    return;
                }

                auto cond = if_code->cons().car();
                eval_stack.push_back({code, EvalFrame::if_check_branch});
                eval_stack.push_back({cond, EvalFrame::start});
                return;

            } else if (id == L_CTX.let_symbol_id_) {
                auto let_code = code->cons().cdr();

                if (let_code->type() != Value::Type::cons) {
                    PUSH_ERR(Error::Code::mismatched_parentheses, L_NIL);
                    return;
                }

                auto bindings = let_code->cons().car();
                int binding_count = length(bindings);

                eval_stack.push_back({.expr_ = code,
                                      .state_ = EvalFrame::let_install_bindings,
                                      .install_let_ = {binding_count}});

                Buffer<Value*, 32> binding_exprs;
                auto b = bindings;
                while (b != get_nil()) {
                    auto binding = b->cons().car();
                    auto value_expr = binding->cons().cdr()->cons().car();
                    binding_exprs.push_back(value_expr);
                    b = b->cons().cdr();
                }

                for (auto expr : reversed(binding_exprs)) {
                    eval_stack.push_back({expr, EvalFrame::start});
                }
                return;
            } else if (id == L_CTX.while_symbol_id_) {
                auto while_code = code->cons().cdr();

                if (while_code->type() != Value::Type::cons) {
                    PUSH_ERR(Error::Code::mismatched_parentheses, L_NIL);
                    return;
                }

                eval_stack.push_back({code, EvalFrame::while_check_condition});
                eval_stack.push_back(
                    {while_code->cons().car(), EvalFrame::start});
                return;

            } else if (id == L_CTX.lambda_symbol_id_) {
                push_op(make_lisp_argumented_function(code->cons().cdr()));
                // NOTE: evaluating an argumented function is
                // destructive. Once we've evaluated an argumented
                // function once, the function body is converted to an
                // intermediate representation, and we adjust the head
                // position in the list and splice out the argument list
                // so that next time we reach the same function, we
                // evaluate it in the lower level intermediate
                // representation rather than performing argument
                // substition again. In fact, re-substituting arguments
                // in an already argument-substituted function creates
                // re-entrancy bugs, and if it didn't, I wouldn't be
                // fixing this, so it's not purely an optmization issue.
                code->cons().set_car(make_symbol("fn"));
                code->cons().set_cdr(code->cons().cdr()->cons().cdr());
                return;
            } else if (id == L_CTX.fn_symbol_id_) {
                push_op(make_lisp_function(code->cons().cdr()));
                return;
            } else if (id == L_CTX.quote_symbol_id_) {
                push_op(code->cons().cdr());
                return;
            } else if (id == L_CTX.quasiquote_symbol_id_) {
                eval_quasiquote(code->cons().cdr());
                return;
            } else if (id == L_CTX.macro_symbol_id_) {
                eval_macro(code->cons().cdr());
                return;
            } else if (id == L_CTX.defconstant_symbol_id_) {
                if (not L_CTX.external_constant_tab_) {
                    PLATFORM.fatal("missing constant tab!");
                }
                push_op(L_NIL);
                return;
            } else if (id == L_CTX.await_symbol_id_) {
                eval_stack.push_back({code, EvalFrame::await_check_result});
                eval_stack.push_back(
                    {code->cons().cdr()->cons().car(), EvalFrame::start});
                return;
            } else if (id == L_CTX.apply_symbol_id_) {
                if (length(code) < 3) {
                    push_op(make_error("insufficent args to apply"));
                    return;
                }
                auto fn_expr = code->cons().cdr()->cons().car();
                auto args_list_expr =
                    code->cons().cdr()->cons().cdr()->cons().car();

                eval_stack.push_back({code, EvalFrame::apply_with_list});
                eval_stack.push_back({args_list_expr, EvalFrame::start});
                eval_stack.push_back({fn_expr, EvalFrame::start});
                return;
            } else if (id == L_CTX.foreach_symbol_id_) {
                // Foreach is implemented in eval because the native C++ version
                // of foreach cannot be suspended with the await keyword.

                // We need to evaluate the function expression, evaluate the
                // list expression.
                if (length(code) < 3) {
                    push_op(make_error("insufficent args to foreach"));
                    return;
                }

                auto fn_expr = code->cons().cdr()->cons().car();
                auto args_list_expr =
                    code->cons().cdr()->cons().cdr()->cons().car();

                eval_stack.push_back({L_NIL, EvalFrame::foreach_iter_init});
                eval_stack.push_back({args_list_expr, EvalFrame::start});
                eval_stack.push_back({fn_expr, EvalFrame::start});
                return;
            }
        }
        push_op(L_CTX.lexical_bindings_);
        auto funcall_expr = code->cons().car();
        auto arg_list = code->cons().cdr();
        const int argc = length(arg_list);
        eval_stack.push_back({.expr_ = code,
                              .state_ = EvalFrame::funcall_apply,
                              .funcall_apply_ = {argc}});
        Buffer<Value*, 32> tmp_buf;
        while (true) {
            if (arg_list == get_nil()) {
                break;
            }
            if (arg_list->type() not_eq Value::Type::cons) {
                // TODO: raise error!!!!!!
            }
            tmp_buf.push_back(arg_list->cons().car()); // todo: check full
            arg_list = arg_list->cons().cdr();
        }
        for (auto arg : reversed(tmp_buf)) {
            eval_stack.push_back({arg, EvalFrame::start});
        }
        eval_stack.push_back({funcall_expr, EvalFrame::start});
    } else {
        // If the value is not a symbol or list, it must be a different literal
        // type.
        push_op(frame.expr_);
    }
}


namespace debug
{


void get_globals(Vector<VariableBinding>& results)
{
    globals_tree_traverse(L_CTX.globals_tree_, [&](Value& val, Value& node) {
        auto name = val.cons().car()->symbol().name();
        for (auto& result : results) {
            if (str_eq(name, result.name_)) {
                return;
            }
        }
        results.push_back({name, val.cons().cdr()});
    });
}


void get_locals(Vector<VariableBinding>& results)
{
    if (L_CTX.lexical_bindings_ not_eq get_nil()) {
        auto stack = L_CTX.lexical_bindings_;

        while (stack not_eq get_nil()) {

            auto bindings = stack->cons().car();
            while (bindings not_eq get_nil()) {
                auto kvp = bindings->cons().car();
                auto name = kvp->cons().car()->symbol().name();
                bool var_exists = false;
                for (auto& result : results) {
                    if (str_eq(result.name_, name)) {
                        var_exists = true;
                        break;
                    }
                }
                if (not var_exists) {
                    results.push_back({name, kvp->cons().cdr()});
                }

                bindings = bindings->cons().cdr();
            }

            stack = stack->cons().cdr();
        }
    }
}


void register_debug_handler(DebugHandler handler)
{
    L_CTX.debug_handler_ = handler;
}


void register_symbol_watchpoint(Value* symbol)
{
    if (contains(L_CTX.debug_watchpoints_, symbol)) {
        return;
    }
    if (symbol->type() == Value::Type::symbol) {
        auto& wp = L_CTX.debug_watchpoints_;
        wp = L_CONS(symbol, wp);
    }
}


void delete_symbol_watchpoint(Value* symbol)
{
    ListBuilder new_watchpoints;
    l_foreach(L_CTX.debug_watchpoints_, [&](Value* v) {
        if (not is_equal(symbol, v)) {
            new_watchpoints.push_back(v);
        }
    });
    L_CTX.debug_watchpoints_ = new_watchpoints.result();
}


void register_symbol_breakpoint(Value* symbol)
{
    if (contains(L_CTX.debug_breakpoints_, symbol)) {
        return;
    }
    if (symbol->type() == Value::Type::symbol) {
        auto& br = L_CTX.debug_breakpoints_;
        br = L_CONS(symbol, br);
        L_CTX.debug_mode_ = true;
    }
}


bool is_symbol_breakpoint(const char* str)
{
    bool result = false;
    l_foreach(L_CTX.debug_breakpoints_, [&](Value* v) {
        if (str_eq(v->symbol().name(), str)) {
            result = true;
        }
    });
    return result;
}


void delete_symbol_breakpoint(Value* symbol)
{
    ListBuilder new_breakpoints;
    l_foreach(L_CTX.debug_breakpoints_, [&](Value* v) {
        if (not is_equal(symbol, v)) {
            new_breakpoints.push_back(v);
        }
    });
    L_CTX.debug_breakpoints_ = new_breakpoints.result();
}


bool check_breakpoint(Value* expr)
{
    if (L_CTX.debug_breakpoints_ == L_NIL) {
        return false;
    }

    if (expr->type() == Value::Type::cons) {
        auto fn_expr = expr->cons().car();
        if (fn_expr->type() == Value::Type::symbol) {
            // Check if this symbol is in breakpoint list
            auto bp_list = L_CTX.debug_breakpoints_;
            while (bp_list not_eq L_NIL) {
                if (is_equal(bp_list->cons().car(), fn_expr)) {
                    return true;
                }
                bp_list = bp_list->cons().cdr();
            }
        }
    }
    return false;
}


} // namespace debug


void push_suspend(EvalStack& eval_stack, u32 op_stack_init)
{
    auto result = get_op0();
    if (result->type() not_eq lisp::Value::Type::promise) {
        LOGIC_ERROR();
    }

    ListBuilder lat;
    lat.push_back(L_CTX.lexical_bindings_);
    lat.push_back(L_CTX.callstack_);
    eval_stack.push_back({.expr_ = lat.result(),
                          .state_ = EvalFrame::await_resume,
                          .await_resume_ = {L_CTX.arguments_break_loc_,
                                            L_CTX.current_fn_argc_}});
    // Execution suspended. Prior to resume, the interpreter
    // will expect the caller to pop the promise and push the
    // result in the captured execution context.
    setup_promise(result->promise(), eval_stack);
    while (L_CTX.operand_stack_->size() > op_stack_init) {
        pop_op();
    }
    push_op(L_NIL);
    L_CTX.lexical_bindings_ = L_NIL;
    reset_callstack();
}


bool destructure_binding(Value* sym, Value* value, ListBuilder& binding_list)
{
    if (is_list(sym)) {
        if (not is_list(value)) {
            push_error("cannot destructure % into %", value, sym);
            return false;
        }
        if (length(sym) not_eq length(value)) {
            if (is_list(value) and length(value) < length(sym)) {
                push_error("expression result % is"
                           " too short to bind to "
                           "destructuring let %",
                           value,
                           sym);
            } else if (is_list(value)) {
                StringBuffer<64> suggestion;
                suggestion += "(";
                l_foreach(sym, [&suggestion](Value* v) {
                    suggestion += stringify(v);
                    suggestion += " ";
                });
                suggestion += ". rest)";
                auto fmt_buffer = allocate_small<StringBuffer<200>>("err-fmt");
                make_format(*fmt_buffer,
                            "expression result % is"
                            " too long to bind to "
                            "destructuring let %. Use improper "
                            "list destructuring to capture "
                            "remaining arguments: %",
                            value,
                            sym,
                            suggestion);
                push_op(make_error(*fmt_buffer));
            } else {
                push_error("cannot destructure % into %", value, sym);
            }
            return false;
        }
        int j = 0;
        while (sym not_eq L_NIL) {
            auto car = sym->cons().car();
            if (car->type() not_eq Value::Type::symbol) {
                push_error("non-symbol % in destructuring let!", car);
                return false;
            }
            binding_list.push_back(L_CONS(car, get_list(value, j++)));
            sym = sym->cons().cdr();
        }
    } else if (sym->type() == Value::Type::cons) {
        auto car = sym->cons().car();
        auto cdr = sym->cons().cdr();
        if (car->type() == Value::Type::symbol and
            cdr->type() == Value::Type::symbol) {
            if (value->type() not_eq Value::Type::cons) {
                push_error("cannot destructure % into %", value, sym);
                return false;
            }
            binding_list.push_back(L_CONS(car, value->cons().car()));
            binding_list.push_back(L_CONS(cdr, value->cons().cdr()));
        } else if (car->type() == Value::Type::symbol and
                   cdr->type() == Value::Type::cons) {
            auto value_lat = value;
            auto sym_lat = sym;
            while (sym->type() == Value::Type::cons) {
                if (value_lat->type() not_eq Value::Type::cons) {
                    push_error("expression result % is too "
                               "short to bind to destructuring"
                               " let %",
                               value,
                               sym_lat);
                    return false;
                }
                auto car = sym->cons().car();
                if (car->type() == Value::Type::symbol) {
                    binding_list.push_back(
                        L_CONS(car, value_lat->cons().car()));
                } else {
                    push_error("Invalid value % in "
                               "destructuring let %",
                               car,
                               sym_lat);
                    return false;
                }
                value_lat = value_lat->cons().cdr();
                sym = sym->cons().cdr();
            }
            if (sym->type() == Value::Type::symbol) {
                binding_list.push_back(L_CONS(sym, value_lat));
            } else {
                push_error("Invalid value % in destructuring "
                           "let %",
                           sym,
                           sym_lat);
                return false;
            }
        } else {
            push_error("non-symbol % in "
                       "destructuring let!",
                       sym);
            return false;
        }
    } else {
        push_error("invalid value % in let "
                   "expression",
                   sym);
        return false;
    }
    return true;
}


static bool is_recursive_invocation(Value* function)
{
    return function == get_this();
}


static bool apply_tail_funcall(Value* fn, int argc, EvalStack& eval_stack)
{
    if (eval_stack.size() == 0) {
        // We aren't executing a function
        return false;
    }

    if (not is_recursive_invocation(fn)) {
        return false;
    }

    int lexical_pop_count = 0;
    auto it = eval_stack.end();
    --it;
    while (it not_eq eval_stack.begin()) {
        // We can do tail call optimization if the next state on the eval stack
        // is function call cleanup (exit the current function), or if the only
        // eval frames between the current frame and the funcall cleanup frame
        // consist of popping lexical scopes.
        if (it->state_ == EvalFrame::lisp_funcall_cleanup) {
            break;
        } else if (it->state_ == EvalFrame::let_cleanup) {
            ++lexical_pop_count;
        } else {
            // FIXME: this does not perform tail call optimization in some cases
            // if we're within debug mode and there are step_over states on the
            // eval stack. That's a bit of an extreme edge case though...
            return false;
        }
        --it;
    }

    while (lexical_pop_count) {
        L_CTX.lexical_bindings_ = L_CTX.lexical_bindings_->cons().cdr();
        eval_stack.pop_back();
        --lexical_pop_count;
    }

    auto& last_frame = eval_stack.back();

    // NOTE: if our next evaluation state just consists of cleaning up after the
    // currently execution function call, and the currently executing funciton
    // is the same as the recursion, we can inline the cleanup step and jump to
    // the top of the current function... There's no need to actually do
    // anything special to detect if an expression is in tail position in a
    // stack-based state machine, because you can see what the future states
    // are, and if the only future state is function call cleanup, the fact that
    // you're in tail position is implicit.
    if (last_frame.state_ == EvalFrame::lisp_funcall_cleanup and
        last_frame.expr_ == fn and
        last_frame.lisp_funcall_cleanup_.argc_ == argc) {

        auto expression_list = dcompr(fn->function().lisp_impl_.code_);

        // operand stack now contains:
        // [... saved_bindings function [args] saved_bindings function [args]]

        // NOTE: at this point, both the prior function call's arguments and our
        // own arguments are ont the stack. Remove intermediate args.
        Buffer<Value*, 32> saved_args;
        for (int i = 0; i < argc; ++i) {
            saved_args.push_back(get_op0());
            pop_op();
        }
        pop_op(); // function
        pop_op(); // lexical bindigns
        for (int i = 0; i < argc; ++i) {
            // Previous function call's arguments
            pop_op();
        }
        for (auto v : reversed(saved_args)) {
            push_op(v);
        }

        if (expression_list == get_nil()) {
            push_op(get_nil());
        } else {
            eval_stack.push_back(
                {expression_list, EvalFrame::lisp_funcall_body});
        }
        return true;
    }

    return false;
}


void inline_foreach_cleanup()
{
    pop_op(); // list
    pop_op(); // fn
    pop_callstack();
}


void eval_loop(EvalStack& eval_stack)
{
    const u32 op_stack_init = L_CTX.operand_stack_->size();

    while (eval_stack.size() not_eq 0) {
        EvalFrame frame = eval_stack.back();
        eval_stack.pop_back();

        switch (frame.state_) {
        case EvalFrame::State::pop_root: {
            auto result = get_op0();
            pop_op(); // result
            pop_op(); // code_root, pushed by eval
            push_op(result);
            break;
        }

        case EvalFrame::State::debug_enable_break:
            // This was necessary for implementing step-over in the
            // debugger. EvalFrame::State::start turns off the debug break flag,
            // and pushes a future state to re-enable it at the end of the
            // current expression.
            L_CTX.debug_break_ = true;
            break;

        case EvalFrame::State::start: {
            if (UNLIKELY(L_CTX.debug_mode_)) {
                if (debug::check_breakpoint(frame.expr_)) {
                    L_CTX.debug_break_ = true;
                    if (L_CTX.debug_handler_) {
                        auto& handler = *L_CTX.debug_handler_;
                        auto reason = debug::Interrupt::breakpoint;
                        handler(reason, frame.expr_);
                    }
                }
                if (L_CTX.debug_break_) {
                    if (L_CTX.debug_handler_) {
                        auto& handler = *L_CTX.debug_handler_;
                        auto reason = debug::Interrupt::step;
                        switch (handler(reason, frame.expr_)) {
                        case debug::Action::step:
                            break;

                        case debug::Action::step_over:
                            L_CTX.debug_break_ = false;
                            eval_stack.push_back(
                                {L_NIL, EvalFrame::State::debug_enable_break});
                            break;

                        case debug::Action::resume:
                            debug_resume();
                            break;
                        }
                    } else {
                        L_CTX.debug_break_ = false;
                    }
                }
            }

            eval_iter_start(frame, eval_stack);
            break;
        }

        case EvalFrame::State::if_check_branch: {
            auto test_result = get_op0();
            pop_op();


            auto if_code = frame.expr_->cons().cdr();
            if (if_code->cons().cdr()->type() not_eq Value::Type::cons) {
                push_op(make_error("if statement with no body!"));
                break;
            }

            Value* branch_to_eval;
            if (is_error(test_result)) {
                push_op(test_result);
                break;
            } else if (is_boolean_true(test_result)) {
                if (if_code->cons().cdr()->type() == Value::Type::cons) {
                    branch_to_eval = if_code->cons().cdr()->cons().car();
                } else {
                    branch_to_eval = get_nil();
                }
            } else {
                auto rest = if_code->cons().cdr();
                if (rest->type() == Value::Type::cons &&
                    rest->cons().cdr()->type() == Value::Type::cons) {
                    branch_to_eval = rest->cons().cdr()->cons().car();
                } else {
                    branch_to_eval = get_nil();
                }
            }

            // Eval the chosen branch
            eval_stack.push_back({branch_to_eval, EvalFrame::start});
            break;
        }

        case EvalFrame::State::while_check_condition: {
            // Condition result is on operand stack
            auto test = get_op0();
            if (is_error(test)) {
                break;
            }
            pop_op();

            if (!is_boolean_true(test)) {
                push_op(get_nil());
                break;
            }

            // Condition is true, need to eval body then loop back
            auto while_code = frame.expr_->cons().cdr();
            auto body = while_code->cons().cdr();

            // After body completes, loop back to test condition again
            eval_stack.push_back(
                {frame.expr_, EvalFrame::while_check_condition});
            eval_stack.push_back({while_code->cons().car(), EvalFrame::start});

            // Now eval all body expressions
            // We need to eval them in sequence, keeping only the last result
            eval_stack.push_back({body, EvalFrame::while_body});
            break;
        }

        case EvalFrame::State::foreach_iter_init:
            // For cleaner stacktraces, the runtime loads an empty lisp function
            // called --inline-foreach and pushes it to the callstack.
            push_callstack(get_var("--inline-foreach"));
            goto FOREACH_START;

        case EvalFrame::State::foreach_iter:
            if (is_error(get_op0())) {
                // Short circuit on error to mirror old behavior.
                auto err = get_op0();
                pop_op(); // err
                inline_foreach_cleanup();
                push_op(err);
                break;
            }
            pop_op(); // funcall result
            // intentional fallthrough to foreach_iter_start

        case EvalFrame::State::foreach_iter_start: {
        FOREACH_START:
            auto fn = get_op1();
            auto list = get_op0();
            if (list->type() == Value::Type::nil) {
                inline_foreach_cleanup();
                push_op(L_NIL); // foreach result
            } else {
                auto car = list->cons().car();
                list = list->cons().cdr();
                pop_op();      // pop old list of stack
                push_op(list); // push list remainder

                push_op(L_CTX.lexical_bindings_);
                push_op(fn); // setup arguments for funcall on stack
                push_op(car);
                eval_stack.push_back({L_NIL, EvalFrame::foreach_iter});
                eval_stack.push_back({.expr_ = fn,
                                      .state_ = EvalFrame::funcall_apply,
                                      .funcall_apply_ = {1}});
            }
            break;
        }

        case EvalFrame::State::await_check_result: {
            auto result = get_op0();
            if (result->type() not_eq Value::Type::promise) {
                pop_op();
                push_error("await expects a promise object, got %", result);
            } else {
                StringBuffer<48> agitant;
                if (can_suspend(eval_stack, agitant)) {
                    push_suspend(eval_stack, op_stack_init);
                    return;
                } else {
                    pop_op(); // The promise
                    push_error("await failed: compiled caller % "
                               "cannot call functions that await",
                               agitant);
                }
            }
            break;
        }

        case EvalFrame::State::await_resume: {
            // Restore all the saved state
            auto saved_list = frame.expr_;
            L_CTX.lexical_bindings_ = saved_list->cons().car();
            L_CTX.callstack_ = saved_list->cons().cdr()->cons().car();
            L_CTX.arguments_break_loc_ = frame.await_resume_.saved_break_loc_;
            L_CTX.current_fn_argc_ = frame.await_resume_.saved_argc_;
            break;
        }

        case EvalFrame::State::while_body: {
            auto body = frame.expr_;

            if (body == get_nil()) {
                // No body!?
                push_op(get_nil());
                break;
            }

            auto first_expr = body->cons().car();
            auto rest = body->cons().cdr();

            if (rest == get_nil()) {
                eval_stack.push_back(
                    {first_expr, EvalFrame::while_body_discard_result});
                eval_stack.push_back({first_expr, EvalFrame::start});
            } else {
                eval_stack.push_back({rest, EvalFrame::while_body});
                eval_stack.push_back(
                    {first_expr, EvalFrame::while_body_discard_result});
                eval_stack.push_back({first_expr, EvalFrame::start});
            }
            break;
        }

        case EvalFrame::State::while_body_discard_result: {
            // Discard the result of this body expression (we only want the last one)
            pop_op();
            break;
        }

        case EvalFrame::State::let_install_bindings: {
            // Operand stack now has: [value1 value2 value3 ...]
            // Build the binding list

            auto let_code = frame.expr_->cons().cdr();
            auto bindings = let_code->cons().car();

            gc_safepoint();

            ListBuilder binding_list;

            // Pop values in reverse order and pair with symbols
            Buffer<Value*, 32> values;
            for (int i = 0; i < frame.install_let_.binding_count_; i++) {
                values.push_back(get_op0());
                pop_op();
            }

            auto b = bindings;
            for (int i = values.size() - 1; i >= 0; i--) {
                auto binding = b->cons().car();
                auto sym = binding->cons().car();
                if (sym->type() == Value::Type::symbol) {
                    binding_list.push_back(make_cons(sym, values[i]));
                } else {
                    if (not destructure_binding(sym, values[i], binding_list)) {
                        goto BINDING_ERROR;
                    }
                }

                b = b->cons().cdr();
            }

            // Install bindings
            L_CTX.lexical_bindings_ =
                make_cons(binding_list.result(), L_CTX.lexical_bindings_);

            // After body, cleanup bindings
            eval_stack.push_back({frame.expr_, EvalFrame::let_cleanup});

            // Eval body
            eval_stack.push_back({let_code->cons().cdr(), EvalFrame::let_body});
            break;

        BINDING_ERROR:
            break;
        }

        case EvalFrame::State::let_body: {
            // Similar to while_body - eval each expression, keep last result
            auto body = frame.expr_;

            if (body == get_nil()) {
                push_op(get_nil());
                break;
            }

            auto first = body->cons().car();
            auto rest = body->cons().cdr();

            if (rest == get_nil()) {
                eval_stack.push_back({first, EvalFrame::start});
            } else {
                eval_stack.push_back({rest, EvalFrame::let_body});
                eval_stack.push_back({first, EvalFrame::let_body_discard});
                eval_stack.push_back({first, EvalFrame::start});
            }
            break;
        }

        case EvalFrame::State::let_body_discard: {
            pop_op();
            break;
        }

        case EvalFrame::State::let_cleanup: {
            // Restore previous bindings
            L_CTX.lexical_bindings_ = L_CTX.lexical_bindings_->cons().cdr();
            // Result already on stack
            break;
        }

        case EvalFrame::State::lisp_funcall_body: {
            auto expression_list = frame.expr_;

            if (expression_list == get_nil()) {
                push_op(get_nil());
                break;
            }

            if (expression_list->type() != Value::Type::cons) {
                push_op(make_error(Error::Code::mismatched_parentheses, L_NIL));
                break;
            }

            auto first = expression_list->cons().car();
            auto rest = expression_list->cons().cdr();

            if (rest == get_nil()) {
                // Last expression
                eval_stack.push_back({first, EvalFrame::start});
            } else {
                // More expressions
                eval_stack.push_back({rest, EvalFrame::lisp_funcall_body});
                eval_stack.push_back(
                    {first, EvalFrame::lisp_funcall_body_discard});
                eval_stack.push_back({first, EvalFrame::start});
            }
            break;
        }

        case EvalFrame::State::lisp_funcall_body_discard: {
            pop_op();
            break;
        }

        case EvalFrame::State::lisp_funcall_setup: {
            auto fn = frame.expr_;
            int argc = frame.funcall_apply_.argc_;

            // Stack is currently: [... saved_bindings function arg1 arg2 arg3]

            L_CTX.lexical_bindings_ =
                dcompr(fn->function().lisp_impl_.lexical_bindings_);

            const auto break_loc = L_CTX.operand_stack_->size() - 1;
            L_CTX.arguments_break_loc_ = break_loc;
            L_CTX.current_fn_argc_ = argc;

            push_callstack(fn);

            auto expression_list = dcompr(fn->function().lisp_impl_.code_);

            if (expression_list == get_nil()) {
                push_op(get_nil());
            } else {
                eval_stack.push_back(
                    {expression_list, EvalFrame::lisp_funcall_body});
            }
            break;
        }

        case EvalFrame::vm_cleanup:
        case EvalFrame::State::lisp_funcall_cleanup: {
            int argc = frame.lisp_funcall_cleanup_.argc_;

            pop_callstack();

            // Stack is: [... saved_bindings function arg1 arg2 ... argN result]

            auto result = get_op0();
            pop_op();

            for (int i = 0; i < argc; ++i) {
                pop_op();
            }

            pop_op();

            auto saved_bindings = get_op0();
            pop_op();
            L_CTX.lexical_bindings_ = saved_bindings;

            L_CTX.arguments_break_loc_ =
                frame.lisp_funcall_cleanup_.saved_break_loc_;
            L_CTX.current_fn_argc_ = frame.lisp_funcall_cleanup_.saved_argc_;

            push_op(result);
            break;
        }

        case EvalFrame::State::apply_with_list: {
            // args_list is on stack, function is below it
            auto args_list = get_op0();
            pop_op();
            auto fn = get_op0();
            pop_op();

            if (not is_list(args_list)) {
                push_op(make_error("parameter passed to apply is not list"));
                break;
            }

            push_op(L_CTX.lexical_bindings_);
            push_op(fn);

            int argc = 0;
            while (args_list != get_nil()) {
                push_op(args_list->cons().car());
                argc++;
                args_list = args_list->cons().cdr();
            }

            eval_stack.push_back({.expr_ = frame.expr_,
                                  .state_ = EvalFrame::funcall_apply,
                                  .funcall_apply_ = {argc}});
            break;
        }

        case EvalFrame::State::funcall_apply: {
            int argc = frame.funcall_apply_.argc_;
            auto fn = get_op(argc); // Function is below arguments on stack

            if (fn->type() == Value::Type::function and
                (fn->hdr_.mode_bits_ == Function::ModeBits::lisp_function or
                 fn->hdr_.mode_bits_ ==
                     Function::ModeBits::lisp_bytecode_function)) {

                if (fn->function().sig_.required_args_ > argc) {
                    for (int i = 0; i < argc; ++i) {
                        pop_op();
                    }
                    pop_op();
                    auto saved_bindings = get_op0();
                    pop_op(); // saved bindings
                    L_CTX.lexical_bindings_ = saved_bindings;
                    push_op(make_error(Error::Code::invalid_argc, fn));
                    break;
                }

                if (L_CTX.strict_) {
                    auto arg_error = [&](auto exp_type, auto got_type) {
                        push_error("invalid arg type for %! expected %, got %",
                                   val_to_string<64>(fn).c_str(),
                                   type_to_string((ValueHeader::Type)exp_type),
                                   type_to_string((ValueHeader::Type)got_type));
                    };

                    auto pop_args = [argc] {
                        for (int i = 0; i < argc; ++i) {
                            pop_op();
                        }
                    };

                    auto saved_break_loc = L_CTX.arguments_break_loc_;
                    auto saved_argc = L_CTX.current_fn_argc_;

                    const auto break_loc = L_CTX.operand_stack_->size() - 1;
                    L_CTX.arguments_break_loc_ = break_loc;
                    L_CTX.current_fn_argc_ = argc;

#define CHECK_ARG_TYPE_INL(ARG, FIELD)                                         \
    if (fn->function().sig_.FIELD not_eq Value::Type::nil and                  \
        get_arg(ARG)->type() not_eq fn->function().sig_.FIELD and              \
        not(get_arg(ARG)->type() == Value::Type::nil and                       \
            fn->function().sig_.FIELD == Value::Type::cons) and                \
        not(get_arg(ARG)->type() == Value::Type::rational and                  \
            (fn->function().sig_.FIELD == Value::Type::integer or              \
             fn->function().sig_.FIELD == Value::Type::ratio))) {              \
        pop_args();                                                            \
        pop_op();                                                              \
        pop_op();                                                              \
        L_CTX.arguments_break_loc_ = saved_break_loc;                          \
        L_CTX.current_fn_argc_ = saved_argc;                                   \
        arg_error(fn->function().sig_.FIELD, get_arg(ARG)->type());            \
        break;                                                                 \
    }

                    if (argc > 3) {
                        CHECK_ARG_TYPE_INL(0, arg0_type_);
                        CHECK_ARG_TYPE_INL(1, arg1_type_);
                        CHECK_ARG_TYPE_INL(2, arg2_type_);
                        CHECK_ARG_TYPE_INL(3, arg3_type_);
                    } else if (argc > 2) {
                        CHECK_ARG_TYPE_INL(0, arg0_type_);
                        CHECK_ARG_TYPE_INL(1, arg1_type_);
                        CHECK_ARG_TYPE_INL(2, arg2_type_);
                    } else if (argc > 1) {
                        CHECK_ARG_TYPE_INL(0, arg0_type_);
                        CHECK_ARG_TYPE_INL(1, arg1_type_);
                    } else if (argc > 0) {
                        CHECK_ARG_TYPE_INL(0, arg0_type_);
                    }

                    L_CTX.arguments_break_loc_ = saved_break_loc;
                    L_CTX.current_fn_argc_ = saved_argc;
                }

                if (fn->hdr_.mode_bits_ == Function::ModeBits::lisp_function) {
                    if (apply_tail_funcall(fn, argc, eval_stack)) {
                        break;
                    } else {
                        EvalFrame cleanup_frame = {
                            .expr_ = fn,
                            .state_ = EvalFrame::lisp_funcall_cleanup,
                            .lisp_funcall_cleanup_ = {
                                argc,
                                L_CTX.arguments_break_loc_,
                                L_CTX.current_fn_argc_}};
                        eval_stack.push_back(cleanup_frame);

                        eval_stack.push_back(
                            {.expr_ = fn,
                             .state_ = EvalFrame::lisp_funcall_setup,
                             .funcall_apply_ = {argc}});
                    }
                } else if (fn->hdr_.mode_bits_ ==
                           Function::ModeBits::lisp_bytecode_function) {

                    push_callstack(fn);
                    gc_safepoint();

                    eval_stack.push_back(
                        {.expr_ = L_NIL,
                         .state_ = EvalFrame::vm_cleanup,
                         .lisp_funcall_cleanup_ = {
                             .argc_ = argc,
                             .saved_break_loc_ = L_CTX.arguments_break_loc_,
                             .saved_argc_ = L_CTX.current_fn_argc_}});

                    const auto break_loc = L_CTX.operand_stack_->size() - 1;
                    L_CTX.arguments_break_loc_ = break_loc;
                    L_CTX.current_fn_argc_ = argc;

                    if (UNLIKELY(L_CTX.debug_break_)) {
                        if (debug_break_compiled_fn(fn)) {
                            eval_stack.push_back(
                                {L_NIL, EvalFrame::debug_enable_break});
                        }
                    }

                    L_CTX.lexical_bindings_ =
                        dcompr(fn->function().lisp_impl_.lexical_bindings_);

                    eval_stack.push_back(
                        {.expr_ = fn,
                         .state_ = EvalFrame::vm_resume,
                         .vm_resume_ = {
                             .program_counter_ =
                                 fn->function()
                                     .bytecode_impl_.bytecode_offset()
                                     ->integer()
                                     .value_,
                             .nested_scope_ = 0}});
                } else {
                    LOGIC_ERROR();
                }

            } else {
                // Non-lisp function - use regular funcall
                funcall(fn, argc);
                auto result = get_op0();
                pop_op(); // result
                pop_op(); // function on stack
                pop_op(); // lexical bindings, ignored
                push_op(result);
            }
            break;
        }

        case EvalFrame::vm_resume: {
            auto fn = frame.expr_;
            auto suspend = vm_resume(fn->function().bytecode_impl_.databuffer(),
                                     fn->function()
                                         .bytecode_impl_.bytecode_offset()
                                         ->integer()
                                         .value_,
                                     frame.vm_resume_);

            if (suspend) {
                eval_stack.push_back({.expr_ = fn,
                                      .state_ = EvalFrame::vm_resume,
                                      .vm_resume_ = *suspend});
                push_suspend(eval_stack, op_stack_init);
                return;
            }
            break;
        }
        }
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

    case Value::Type::promise:
        return lhs == rhs;

    case Value::Type::count:
    case Value::Type::rational:
    case Value::Type::__reserved_1:
    case Value::Type::__reserved_0:
    case Value::Type::nil:
    case Value::Type::heap_node:
    case Value::Type::databuffer:
        return lhs == rhs;

    case Value::Type::ratio:
        return is_equal(dcompr(lhs->ratio().numerator_),
                        dcompr(rhs->ratio().numerator_)) and
               lhs->ratio().divisor_ == rhs->ratio().divisor_;
        break;

    case Value::Type::wrapped: {
        if (not str_eq(dcompr(lhs->wrapped().type_sym_)->symbol().name(),
                       dcompr(rhs->wrapped().type_sym_)->symbol().name())) {
            return false;
        }
        auto type = dcompr(lhs->wrapped().type_sym_);
        auto equal_fn =
            get_var(::format<64>("-equal-%", type->symbol().name()).c_str());

        if (equal_fn->type() == Value::Type::function) {
            push_op(rhs);
            push_op(lhs);
            safecall(equal_fn, 2);
            auto ret = is_boolean_true(get_op0());
            pop_op(); // result
            return ret;
        } else {
            Platform::fatal(::format<64>("missing equal function for %",
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

    case Value::Type::string:
        return str_cmp(lhs->string().value(), rhs->string().value()) == 0;
    }
    return false;
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

    if (L_CTX.external_constant_tab_) {
        u32 i = 0;
        for (; i < L_CTX.external_constant_tab_size_;) {
            auto ptr = L_CTX.external_constant_tab_ + i;
            u8 field_size = ((ConstantTabEntryHeader*)ptr)->field_size_;
            u8 value_size = ((ConstantTabEntryHeader*)ptr)->value_size_;
            const char* name = ptr + sizeof(ConstantTabEntryHeader);
            handle_completion(name);
            i += sizeof(ConstantTabEntryHeader) + field_size + value_size;
        }
    }
}


const char* nameof(Function::CPP_Impl impl);


Value* stacktrace()
{
    L_CTX.callstack_untouched_ = false;
    return L_CTX.callstack_->cons().cdr();
}


s32 to_integer(Value* v)
{
    if (v->type() == Value::Type::integer) {
        return v->integer().value_;
    } else if (v->type() == Value::Type::ratio) {
        auto& ratio = v->ratio();
        return dcompr(ratio.numerator_)->integer().value_ / ratio.divisor_;
    }
    if (L_CTX.strict_) {
        PLATFORM.fatal(::format<64>("cannot convert % to integer!", v));
    }
    return 1;
}


bool to_rational(Value* v, s32& num, s32& div)
{
    if (v->type() == Value::Type::integer) {
        num = v->integer().value_;
        div = 1;
        return true;
    } else if (v->type() == Value::Type::ratio) {
        auto& ratio = v->ratio();
        num = dcompr(ratio.numerator_)->integer().value_;
        div = ratio.divisor_;
        return true;
    }
    return false; // Not a rational type
}


bool rational_less(s32 a_num, s32 a_div, s32 b_num, s32 b_div)
{
    // a/b < c/d  iff  a*d < c*b  (when denominators are positive)
    return a_num * b_div < b_num * a_div;
}


Value* make_argument_error(ValueHeader::Type expected, int position)
{
    return make_error(::format("%: expected type % in arg %, got %",
                               get_this(),
                               type_to_string(expected),
                               position,
                               type_to_string(get_op(position)->type())));
}


bool is_numeric(Value* v)
{
    return v->type() == Value::Type::rational ||
           v->type() == Value::Type::integer ||
           v->type() == Value::Type::ratio || v->type() == Value::Type::fp;
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

           bool define_if_missing = not L_CTX.strict_;

           GC_REQUIRE_SPACE(4);

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
               GC_REQUIRE_SPACE(4);
               if (not globals_tree_find(get_op(i))) {
                   set_var(get_op(i), L_NIL, true);
               }
           }
           return get_op0();
       }}},
     {"bound?",
      {SIG1(nil, symbol),
       [](int argc) {
           L_EXPECT_OP(0, symbol);

           auto found = globals_tree_find(get_op0());
           return make_integer(found not_eq nullptr);
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
               GC_REQUIRE_SPACE(20);
               set_var(sym, L_NIL, true);
               globals_tree_erase(sym);
           }

           return get_nil();
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
     {"this",
      {SIG0(function),
       [](int argc) { return L_CTX.callstack_->cons().cdr()->cons().car(); }}},
     {"stacktrace", {SIG0(cons), [](int argc) { return stacktrace(); }}},
     {"lint",
      {SIG1(nil, cons),
       [](int argc) {
           L_EXPECT_OP(0, cons);
           if (not is_list(get_op0())) {
               return make_error("lint expects list parameter!");
           }

           Protected gvar_list(L_NIL);
           lint(get_op0(), L_NIL, gvar_list);
           auto result = get_op0();
           pop_op();
           return result;
       }}},
     {"strict-mode",
      {SIG1(nil, nil),
       [](int argc) {
           if (is_error(get_op0())) {
               return get_op0();
           }
           L_CTX.strict_ = is_boolean_true(get_op0());
           return L_NIL;
       }}},
     {"lisp-mem-stack-used",
      {SIG0(integer),
       [](int argc) { return L_INT(L_CTX.operand_stack_->size()); }}},
     {"lisp-mem-stack-contents",
      {SIG0(cons),
       [](int argc) {
           lisp::ListBuilder b;
           for (auto& v : *L_CTX.operand_stack_) {
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
           const char* early_gc_err_fmt =
               "GC threshold % too low, minimum % required for safe operations";
           L_EXPECT_OP(0, integer);
           auto val = L_LOAD_INT(0);
           if (val < early_gc_threshold_min) {
               return make_error(
                   ::format(early_gc_err_fmt, val, early_gc_threshold_min)
                       .c_str());
           }
           early_gc_threshold = val;
           return L_NIL;
       }}},
     {"lisp-mem-crit-gc-alert",
      {SIG1(nil, integer),
       [](int) {
           L_EXPECT_OP(0, integer);
           L_CTX.critical_gc_alert_ = L_LOAD_INT(0);
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
           int globals_used = 0;
           globals_tree_traverse(
               L_CTX.globals_tree_,
               [&globals_used](Value&, Value&) { ++globals_used; });

           return L_INT(globals_used);
       }}},
     {"lisp-mem-string-internb",
      {SIG0(integer),
       [](int argc) { return L_INT(L_CTX.string_intern_pos_); }}},
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
     {"breakpoint",
      {SIG0(nil),
       [](int argc) {
           L_CTX.debug_mode_ = true;
           L_CTX.debug_break_ = true;
           return L_NIL;
       }}},
     {"breakpoint-register", {SIG1(nil, symbol), builtin_breakpoint_reg}},
     {"breakpoint-unregister", {SIG1(nil, symbol), builtin_breakpoint_unreg}},
     {"watchpoint-register", {SIG1(nil, symbol), builtin_watchpoint_reg}},
     {"watchpoint-unregister", {SIG1(nil, symbol), builtin_watchpoint_unreg}},
     {"signature", {SIG1(cons, function), builtin_signature}},
     {"require-args",
      {SIG2(function, function, integer), builtin_require_args}},
     {"rot13", {SIG1(string, string), builtin_rot13}},
     {"cons", {SIG2(cons, nil, nil), builtin_cons}},
     {"car", {SIG1(nil, cons), builtin_car}},
     {"first", {SIG1(nil, cons), builtin_car}},
     {"identity", {SIG1(nil, nil), builtin_identity}},
     {"cddr", {SIG1(nil, cons), builtin_cddr}},
     {"cadr", {SIG1(nil, cons), builtin_cadr}},
     {"cdar", {SIG1(nil, cons), builtin_cdar}},
     {"caar", {SIG1(nil, cons), builtin_caar}},
     {"cdr", {SIG1(nil, cons), builtin_cdr}},
     {"second", {SIG1(nil, cons), builtin_cdr}},
     {"rest", {SIG1(nil, cons), builtin_cdr}},
     {"list", {SIG0(cons), builtin_list}},
     {"split", {SIG2(list, string, string), builtin_split}},
     {"abs", {SIG1(rational, rational), builtin_abs}},
     {"not", {EMPTY_SIG(1), builtin_logical_not}},
     {"equal", {EMPTY_SIG(2), builtin_comp_equal}},
     {"boolean?", {EMPTY_SIG(1), builtin_is_boolean}},
     {"list?", {EMPTY_SIG(1), builtin_is_list}},
     {"nil?", {EMPTY_SIG(1), builtin_is_nil}},
     {"ratio?", {EMPTY_SIG(1), builtin_is_ratio}},
     {"int?", {EMPTY_SIG(1), builtin_is_int}},
     {"float?", {EMPTY_SIG(1), builtin_is_float}},
     {"pair?", {EMPTY_SIG(1), builtin_is_pair}},
     {"lambda?", {EMPTY_SIG(1), builtin_is_lambda}},
     {"error?", {EMPTY_SIG(1), builtin_is_error}},
     {"symbol?", {EMPTY_SIG(1), builtin_is_symbol}},
     {"type", {SIG1(symbol, nil), builtin_type}},
     {"databuffer?", {EMPTY_SIG(1), builtin_is_databuffer}},
     {"string?", {EMPTY_SIG(1), builtin_is_string}},
     {"wrapped?", {EMPTY_SIG(1), builtin_is_wrapped}},
     {"wrap", {SIG2(wrapped, nil, symbol), builtin_wrap}},
     {"unwrap", {SIG1(nil, wrapped), builtin_unwrap}},
     {"odd?", {SIG1(nil, integer), builtin_is_odd}},
     {"string-to-bytes", {SIG1(cons, string), builtin_string_to_bytes}},
     {"bytes-to-string", {SIG1(string, cons), builtin_bytes_to_string}},
     {"int-to-bytes", {SIG1(cons, integer), builtin_int_to_bytes}},
     {"bytes-to-int", {SIG1(integer, cons), builtin_bytes_to_int}},
     {"int", {SIG1(integer, nil), builtin_toint}},
     {"float", {SIG1(fp, nil), builtin_tofloat}},
     {"error-info", {SIG1(nil, error), builtin_error_info}},
     {"databuffer", {SIG0(databuffer), builtin_databuffer}},
     {"buffer-write!",
      {SIG3(nil, databuffer, integer, cons), builtin_buffer_write}},
     {"buffer-read",
      {SIG3(cons, databuffer, integer, integer), builtin_buffer_read}},
     {"apropos", {SIG1(nil, string), builtin_apropos}},
     {"apply", {SIG2(nil, function, cons), builtin_apply}},
     {"fill", {SIG2(cons, integer, nil), builtin_fill}},
     {"difference", {SIG2(cons, cons, cons), builtin_difference}},
     {"union", {SIG2(cons, cons, cons), builtin_union}},
     {"length", {SIG1(integer, nil), builtin_length}},
     {"<", {EMPTY_SIG(2), builtin_comp_less_than}},
     {">", {EMPTY_SIG(2), builtin_comp_greater_than}},
     {"bit-and", {SIG2(integer, integer, integer), builtin_bit_and}},
     {"bit-or", {SIG2(integer, integer, integer), builtin_bit_or}},
     {"bit-xor", {SIG2(integer, integer, integer), builtin_bit_xor}},
     {"bit-not", {SIG1(integer, integer), builtin_bit_not}},
     {"mod", {SIG2(rational, rational, rational), builtin_mod}},
     {"bit-shift-right",
      {SIG2(integer, integer, integer), builtin_bit_shift_right}},
     {"bit-shift-left",
      {SIG2(integer, integer, integer), builtin_bit_shift_left}},
     {"hex", {SIG1(string, integer), builtin_hex}},
     {"incr", {SIG1(rational, rational), builtin_incr}},
     {"decr", {SIG1(rational, rational), builtin_decr}},
     {"+", {EMPTY_SIG(0), builtin_add}},
     {"-", {EMPTY_SIG(1), builtin_subtract}},
     {"*", {EMPTY_SIG(0), builtin_multiply}},
     {"/", {EMPTY_SIG(2), builtin_divide}},
     {"rationalize", {SIG1(rational, fp), builtin_rationalize}},
     {"range", {SIG1(cons, rational), builtin_range}},
     {"symbol", {SIG1(symbol, string), builtin_symbol}},
     {"format", {SIG2(string, string, nil), builtin_format}},
     {"profile", {SIG1(integer, function), builtin_profile}},
     {"slice", {EMPTY_SIG(2), builtin_slice}},
     {"string-explode", {SIG1(cons, string), builtin_string_explode}},
     {"string-assemble", {SIG1(string, cons), builtin_string_assemble}},
     {"string", {SIG0(string), builtin_string}},
     {"error", {SIG1(error, string), builtin_error}},
     {"filter", {SIG2(cons, function, cons), builtin_filter}},
     {"nameof", {SIG1(nil, nil), builtin_nameof}},
     {"foreach", {SIG2(nil, function, cons), builtin_foreach}},
     {"map", {SIG2(cons, function, cons), builtin_map}},
     {"find", {SIG2(nil, nil, cons), builtin_find}},
     {"flatten", {SIG1(cons, cons), builtin_flatten}},
     {"reverse", {SIG1(cons, cons), builtin_reverse}},
     {"gc", {EMPTY_SIG(0), [](int argc) { return make_integer(gc()); }}},
     {"get", {SIG2(nil, nil, rational), builtin_get}},
     {"--on-autoload", {SIG1(nil, symbol), [](int argc) { return L_NIL; }}},
     {"read", {SIG1(nil, string), builtin_read}},
     {"eval", {SIG1(nil, nil), builtin_eval}},
     {"sort", {SIG2(cons, cons, function), builtin_sort}},
     {"compile", {SIG1(function, function), builtin_compile}},
     {"disassemble", {SIG1(nil, function), builtin_disassemble}}});


int toplevel_count()
{
    int count = 0;

    globals_tree_traverse(L_CTX.globals_tree_,
                          [&count](Value& val, Value&) { ++count; });

    L_CTX.native_interface_.get_symbols_([&count](const char*) { ++count; });
    count += builtin_table.size();

    return count;
}


void get_env(SymbolCallback callback)
{
    for (auto& kvp : builtin_table) {
        callback(kvp.first.c_str());
    }

    L_CTX.native_interface_.get_symbols_(callback);

    globals_tree_traverse(L_CTX.globals_tree_, [&callback](Value& val, Value&) {
        callback((const char*)val.cons().car()->symbol().name());
    });

    l_foreach(L_CTX.macros_,
              [&](Value* v) { callback(v->cons().car()->symbol().name()); });
}



const char* intern(const char* string)
{
    const auto len = strlen(string);

    auto found_builtin = builtin_table.find(string);
    if (found_builtin not_eq builtin_table.end()) {
        // If the string exists as a constant in the builtin table, then it
        // needn't be copied to the string intern table.
        return found_builtin->first.c_str();
    }

    if (auto ni_sym = L_CTX.native_interface_.resolve_intern_sym_(string)) {
        return ni_sym;
    }

    if (L_CTX.external_symtab_contents_) {
        const char* search = L_CTX.external_symtab_contents_;
        u32 left = 0;
        u32 right = L_CTX.external_symtab_size_ / 32;

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
    }

    // Ok, no stable pointer to the string exists anywhere, so we'll have to
    // preserve the string contents in intern memory.

    if (len + 1 > string_intern_table_size - L_CTX.string_intern_pos_) {

        PLATFORM.fatal("string intern table full");
    }

    if (not L_CTX.string_intern_table_) {
        L_CTX.string_intern_table_ =
            allocate<StringInternTable>("string-intern-table");
        info(::format("allocating string intern table (due to symbol %)",
                      string));
    }

    const char* search = (*L_CTX.string_intern_table_)->data_;
    for (int i = 0; i < L_CTX.string_intern_pos_;) {
        if (str_eq(search + i, string)) {
            return search + i;
        } else {
            while (search[i] not_eq '\0') {
                ++i;
            }
            ++i;
        }
    }

    auto result =
        (*L_CTX.string_intern_table_)->data_ + L_CTX.string_intern_pos_;

    for (u32 i = 0; i < len; ++i) {
        ((*L_CTX.string_intern_table_)->data_)[L_CTX.string_intern_pos_++] =
            string[i];
    }
    ((*L_CTX.string_intern_table_)->data_)[L_CTX.string_intern_pos_++] = '\0';

    return result;
}


Value* __get_local(LocalVariableOffset off)
{
    auto stack = L_CTX.lexical_bindings_;

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

    if (L_CTX.lexical_bindings_ not_eq get_nil()) {
        auto stack = L_CTX.lexical_bindings_;

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

    auto found_ni_fn = L_CTX.native_interface_.lookup_function_(name);

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
            for (int i = L_CTX.current_fn_argc_ - 1; i > -1; --i) {
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
    if (L_CTX.lexical_bindings_ not_eq get_nil()) {
        auto stack = L_CTX.lexical_bindings_;
        auto sym_id = symbol->symbol().unique_id();

        while (stack not_eq get_nil()) {

            auto bindings = stack->cons().car();
            while (bindings not_eq get_nil()) {
                auto kvp = bindings->cons().car();
                if (kvp->cons().car()->symbol().unique_id() == sym_id) {
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
    if (L_CTX.external_constant_tab_) {
        u32 i = 0;
        for (; i < L_CTX.external_constant_tab_size_;) {
            auto ptr = L_CTX.external_constant_tab_ + i;
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

    push_op(symbol);
    funcall(get_var("--on-autoload"), 1);
    auto result = get_op0();
    pop_op();
    if (not is_error(result) and result not_eq L_NIL) {
        return result;
    }

    StringBuffer<31> hint("[var: ");
    hint += symbol->symbol().name();
    hint += "]";

    Protected err_str(make_string(hint.c_str()));
    return make_error(Error::Code::undefined_variable_access, err_str);
}


Value* set_var(Value* symbol, Value* val, bool define_var)
{
    if (L_CTX.debug_watchpoints_ not_eq get_nil()) {
        bool watchpoint = contains(L_CTX.debug_watchpoints_, symbol);
        if (watchpoint and L_CTX.debug_handler_) {
            auto reason = debug::Interrupt::watchpoint;
            Protected pack(L_CONS(symbol, val));
            auto resp = (*L_CTX.debug_handler_)(reason, pack);
            if (resp == debug::Action::step) {
                L_CTX.debug_break_ = true;
            } else {
                L_CTX.debug_break_ = false;
                if (L_CTX.debug_breakpoints_ == L_NIL) {
                    L_CTX.debug_mode_ = false;
                }
            }
        }
    }

    if (L_CTX.lexical_bindings_ not_eq get_nil()) {
        auto stack = L_CTX.lexical_bindings_;

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

    if (auto n = L_CTX.native_interface_.lookup_name_(impl)) {
        return n;
    }

    return nullptr;
}


const char* nameof(Value* value)
{
    const char* name = nullptr;
    globals_tree_traverse(L_CTX.globals_tree_, [&](Value& car, Value& node) {
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


void init(Optional<std::pair<const char*, u32>> external_symtab,
          Optional<std::pair<const char*, u32>> external_constant_tab)
{
    if (bound_context) {
        return;
    }

    bound_context.emplace();

    if (external_symtab and external_symtab->second) {
        L_CTX.external_symtab_contents_ = external_symtab->first;
        L_CTX.external_symtab_size_ = external_symtab->second;
    }

    if (external_constant_tab and external_constant_tab->second) {
        L_CTX.external_constant_tab_ = external_constant_tab->first;
        L_CTX.external_constant_tab_size_ = external_constant_tab->second;
    }

    value_pool_init();
    L_CTX.nil_ = alloc_value();
    L_CTX.nil_->hdr_.type_ = Value::Type::nil;
    L_CTX.nil_->hdr_.mode_bits_ = 0;
    L_CTX.globals_tree_ = L_CTX.nil_;
    L_CTX.callstack_ = L_CTX.nil_;
    L_CTX.lexical_bindings_ = L_CTX.nil_;
    L_CTX.debug_breakpoints_ = L_CTX.nil_;
    L_CTX.debug_watchpoints_ = L_CTX.nil_;

    L_CTX.bytecode_buffer_ = L_CTX.nil_;
    L_CTX.string_buffer_ = L_CTX.nil_;
    L_CTX.macros_ = L_CTX.nil_;

#ifdef USE_SYMBOL_CACHE
    for (auto& v : L_CTX.symbol_cache_) {
        v = L_CTX.nil_;
    }
#endif

    L_CTX.tree_nullnode_ = L_CONS(get_nil(), L_CONS(get_nil(), get_nil()));

    reset_operand_stack();

    if (dcompr(compr(get_nil())) not_eq get_nil()) {
        PLATFORM.fatal("pointer compression test failed 1");
    }

    reset_callstack();

    for (int i = 0; i < MAX_NAMED_ARGUMENTS; ++i) {
        L_CTX.argument_symbols_[i] = make_symbol(::format("$%", i).c_str());
    }

    auto sym_id = [](const char* str) {
        return make_symbol(str)->symbol().unique_id();
    };

    L_CTX.if_symbol_id_ = sym_id("if");
    L_CTX.let_symbol_id_ = sym_id("let");
    L_CTX.while_symbol_id_ = sym_id("while");
    L_CTX.lambda_symbol_id_ = sym_id("lambda");
    L_CTX.fn_symbol_id_ = sym_id("fn");
    L_CTX.quote_symbol_id_ = sym_id("'");
    L_CTX.quasiquote_symbol_id_ = sym_id("`");
    L_CTX.macro_symbol_id_ = sym_id("macro");
    L_CTX.defconstant_symbol_id_ = sym_id("defconstant");
    L_CTX.await_symbol_id_ = sym_id("await");
    L_CTX.apply_symbol_id_ = sym_id("apply");
    L_CTX.map_symbol_id_ = sym_id("map");
    L_CTX.foreach_symbol_id_ = sym_id("foreach");
}


} // namespace lisp
