////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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


#include "lisp.hpp"
#include "allocator.hpp"
#include "bytecode.hpp"
#include "eternal/eternal.hpp"
#include "heap_data.hpp"
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


static const u32 string_intern_table_size = 2900;


#if defined(__NDS__)
#define VALUE_POOL_SIZE 20000
#elif defined(__GBA__)
#define VALUE_POOL_SIZE 9600
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
    DataBuffer data_buffer_;
    String string_;
    __Reserved __reserved_;
};


#if defined(__GBA__) or defined(__NDS__)
static_assert(sizeof(ValueMemory) == 8);
#endif


static HEAP_DATA ValueMemory value_pool_data[VALUE_POOL_SIZE];
static Value* value_pool = nullptr;


static HEAP_DATA char symbol_intern_table[string_intern_table_size];


const char* intern(const char* string);


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
}


Value* value_pool_alloc()
{
    if (value_pool) {
        auto ret = value_pool;
        value_pool = ret->heap_node().next_;
        return (Value*)ret;
    }
    return nullptr;
}


void value_pool_free(Value* value)
{
    value->hdr_.type_ = Value::Type::heap_node;
    value->hdr_.alive_ = false;
    value->hdr_.mark_bit_ = false;

    value->heap_node().next_ = value_pool;
    value_pool = value;
}


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

    Value* nil_ = nullptr;
    Value* oom_ = nullptr;
    Value* string_buffer_ = nullptr;
    Value* globals_tree_ = nullptr;
    Value* tree_nullnode_ = nullptr;

    Value* lexical_bindings_ = nullptr;
    Value* macros_ = nullptr;

    Value* callstack_ = nullptr;

    NativeInterface native_interface_;

    int string_intern_pos_ = 0;
    int eval_depth_ = 0;
    int interp_entry_count_ = 0;
    u32 alloc_highwater_ = 0;

    u16 string_buffer_remaining_ = 0;
    u16 arguments_break_loc_;
    u8 current_fn_argc_ = 0;
    bool strict_ = false;

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


static void push_callstack(Value* function)
{
    bound_context->callstack_ = make_cons(function, bound_context->callstack_);
}


static void pop_callstack()
{
    bound_context->callstack_ = bound_context->callstack_->cons().cdr();
}


const char* native_interface_resolve_intern_default(const char*)
{
    return nullptr;
}


NativeInterface::LookupResult native_interface_fn_lookup_default(const char*)
{
    return {0, nullptr};
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

    if (auto len = is_list_slowpath(val)) {
        if (val->type() == Value::Type::cons) {
            val->cons().is_definitely_list_ = true;

            if (len < 127) {
                val->cons().cached_length_ = len;
            }
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

    const char* search = symbol_intern_table;
    for (int i = 0; i < ctx->string_intern_pos_;) {
        callback(search + i);
        while (search[i] not_eq '\0') {
            ++i;
        }
        ++i;
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

    Value* front = lat;

    if (front->cons().cached_length_) {
        return front->cons().cached_length_;
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

    if (len < 127) {
        front->cons().cached_length_ = len;
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


static Value* alloc_value()
{
    auto init_val = [](Value* val) {
        val->hdr_.mark_bit_ = false;
        val->hdr_.alive_ = true;
        return val;
    };

    if (auto val = value_pool_alloc()) {
        return init_val(val);
    }

    run_gc();

    // Hopefully, we've freed up enough memory...
    if (auto val = value_pool_alloc()) {
        return init_val(val);
    }

    Platform::fatal("LISP out of memory");

    return nullptr;
}


Value* make_function(Function::CPP_Impl impl)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::function;
        val->function().cpp_impl_ = impl;
        val->function().required_args_ = 0;
        val->hdr_.mode_bits_ = Function::ModeBits::cpp_function;
        return val;
    }
    return bound_context->oom_;
}


static Value* make_lisp_function(Value* impl)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::function;
        val->function().lisp_impl_.code_ = compr(impl);
        val->function().required_args_ = 0;
        val->function().lisp_impl_.lexical_bindings_ =
            compr(bound_context->lexical_bindings_);

        val->hdr_.mode_bits_ = Function::ModeBits::lisp_function;
        return val;
    }
    return bound_context->oom_;
}


Value* make_bytecode_function(Value* bytecode)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::function;
        val->function().bytecode_impl_.lexical_bindings_ =
            compr(bound_context->lexical_bindings_);
        val->function().required_args_ = 0;

        val->function().bytecode_impl_.bytecode_ = compr(bytecode);
        val->hdr_.mode_bits_ = Function::ModeBits::lisp_bytecode_function;
        return val;
    }
    return bound_context->oom_;
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
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::cons;
        val->cons().set_car(car);
        val->cons().__set_cdr(cdr);
        val->cons().is_definitely_list_ = false;
        val->cons().cached_length_ = 0;
        return val;
    }
    return bound_context->oom_;
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
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::integer;
        val->integer().value_ = value;
        return val;
    }
    return bound_context->oom_;
}


Value* make_float(Float::ValueType value)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::fp;
        val->fp().value_ = value;
        return val;
    }
    return bound_context->oom_;
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


Value* make_error(Error::Code error_code, Value* context)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::error;
        val->error().code_ = error_code;
        val->error().context_ = compr(context);
        val->error().stacktrace_ = compr(stacktrace());
        return val;
    }
    return bound_context->oom_;
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

    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::symbol;
        val->hdr_.mode_bits_ = (u8)mode;
        val->symbol().set_name(name);
        return val;
    }
    return bound_context->oom_;
}


static Value* intern_to_symbol(const char* already_interned_str)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::symbol;
        val->hdr_.mode_bits_ = (u8)Symbol::ModeBits::stable_pointer;
        val->symbol().set_name(already_interned_str);
        return val;
    }
    return bound_context->oom_;
}


Value* make_userdata(void* obj)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::user_data;
        val->user_data().obj_ = obj;
        return val;
    }
    return bound_context->oom_;
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

    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::data_buffer;
        new ((ScratchBufferPtr*)val->data_buffer().sbr_mem_)
            ScratchBufferPtr(make_scratch_buffer(sbr_tag));
        return val;
    }
    return bound_context->oom_;
}


void live_values(::Function<6 * sizeof(void*), void(Value&)> callback);


Value* make_string_from_literal(const char* str)
{
    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::string;
        val->string().data_.literal_.value_ = str;
        val->string().is_literal_ = true;
        return val;
    } else {
        return bound_context->oom_;
    }
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

        auto write_ptr = existing_buffer->data_buffer().value()->data_ + offset;

        memcpy(write_ptr, string, len);

        return {existing_buffer, offset};

    } else {

        // Because we're allocating a fresh buffer, as the prior one was full.
        bound_context->string_buffer_remaining_ =
            SCRATCH_BUFFER_SIZE - (len + 1);

        auto buffer = make_databuffer("lisp-string-bulk-allocator");

        if (buffer == bound_context->oom_) {
            Platform::fatal("oom");
        }

        Protected p(buffer);
        bound_context->string_buffer_ = buffer;

        for (int i = 0; i < SCRATCH_BUFFER_SIZE; ++i) {
            buffer->data_buffer().value()->data_[i] = '\0';
        }
        auto write_ptr = buffer->data_buffer().value()->data_;

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


    auto [buffer, offset] = store_string(string, len);

    if (auto val = alloc_value()) {
        val->hdr_.type_ = Value::Type::string;
        val->string().data_.memory_.data_buffer_ = compr(buffer);
        val->string().data_.memory_.offset_ = offset;
        val->string().is_literal_ = false;
        return val;
    } else {
        return bound_context->oom_;
    }
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
        if (obj->function().required_args_ > argc) {
            pop_args();
            push_op(make_error(Error::Code::invalid_argc, obj));
            break;
        }

        switch (obj->hdr_.mode_bits_) {
        case Function::ModeBits::cpp_function: {
            auto result = obj->function().cpp_impl_(argc);
            pop_args();
            push_op(result);
            break;
        }

        case Function::ModeBits::lisp_function: {
            PLATFORM.system_call("sc", nullptr);
            auto& ctx = *bound_context;
            ctx.lexical_bindings_ =
                dcompr(obj->function().lisp_impl_.lexical_bindings_);
            const auto break_loc = ctx.operand_stack_->size() - 1;
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
            PLATFORM.system_call("sc", nullptr);
            auto& ctx = *bound_context;
            const auto break_loc = ctx.operand_stack_->size() - 1;
            ctx.arguments_break_loc_ = break_loc;
            ctx.current_fn_argc_ = argc;

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
        Platform::fatal("attempt to all non-function!");
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


Value* dostring(const char* code)
{
    BasicCharSequence cs(code);
    return dostring(
        cs, [](Value&) { Platform::fatal("fatal error in dostring..."); });
}


Value* dostring(CharSequence& code,
                ::Function<4 * sizeof(void*), void(Value&)> on_error)
{
    ++bound_context->interp_entry_count_;

    int i = 0;

    Protected result(get_nil());

    while (true) {
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
            push_op(expr_result);
            on_error(*expr_result);
            pop_op();
            break;
        }
    }

    --bound_context->interp_entry_count_;

    return result;
}


const char* nameof(Value* value);


void format_impl(Value* value, Printer& p, int depth)
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

    case lisp::Value::Type::__reserved:
        break;

    case lisp::Value::Type::string:
        p.put_str("\"");
        p.put_str(value->string().value());
        p.put_str("\"");
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

        if (value->function().required_args_) {
            p.put_str(":");
            p.put_str(stringify(value->function().required_args_).c_str());
        }
        p.put_str(">");
        break;

    case lisp::Value::Type::user_data:
        p.put_str("<ud>");
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

    case lisp::Value::Type::data_buffer:
        p.put_str("<sbr>");
        break;

    case lisp::Value::Type::count:
        break;
    }
}


const char* String::value()
{
    if (is_literal_) {
        return data_.literal_.value_;
    } else {
        return dcompr(data_.memory_.data_buffer_)
                   ->data_buffer()
                   .value()
                   ->data_ +
               data_.memory_.offset_;
    }
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


void format(Value* value, Printer& p)
{
    format_impl(value, p, 0);
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
        if (not value->string().is_literal_) {
            gc_mark_value(dcompr(value->string().data_.memory_.data_buffer_));
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
    gc_mark_value(bound_context->oom_);
    gc_mark_value(bound_context->lexical_bindings_);
    gc_mark_value(bound_context->macros_);
    gc_mark_value(bound_context->tree_nullnode_);

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
        __Reserved::finalizer,
};


static void invoke_finalizer(Value* value)
{
    // NOTE: This ordering should match the Value::Type enum.

    fin_table[value->type()].fn_(value);
}

void DataBuffer::finalizer(Value* buffer)
{
    reinterpret_cast<ScratchBufferPtr*>(buffer->data_buffer().sbr_mem_)
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

    auto db = new_buffer();
    u32 write_offset = 0;
    u32 string_bytes_total = 0;

    Vector<Value*> recovered_buffers;

    for (int i = 0; i < VALUE_POOL_SIZE; ++i) {
        Value* val = (Value*)&value_pool_data[i];

        if (val->hdr_.alive_ and val->type() == Value::Type::string and
            not val->string().is_literal_) {

            const auto len = strlen(val->string().value()) + 1;

            if (write_offset + len >= SCRATCH_BUFFER_SIZE) {
                write_offset = 0;
                db = new_buffer();
            }

            memcpy(db->data_buffer().value()->data_ + write_offset,
                   val->string().value(),
                   len);

            auto old_buffer = dcompr(val->string().data_.memory_.data_buffer_);
            if (not contains(recovered_buffers, old_buffer)) {
                recovered_buffers.push_back(old_buffer);
            }

            val->string().data_.memory_.data_buffer_ = compr(db);
            val->string().data_.memory_.offset_ = write_offset;

            write_offset += len;
            string_bytes_total += len;
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
    char* const interns = symbol_intern_table;
    char* str = interns;

    while (static_cast<u32>(str - interns) < string_intern_table_size and
           static_cast<s32>(str - interns) <
               bound_context->string_intern_pos_ and
           *str not_eq '\0') {

        fn(str);

        str += strlen(str) + 1;
    }
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
                    push_op(lisp::make_error(
                        Error::Code::mismatched_parentheses, L_NIL));
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
            push_op(
                lisp::make_error(Error::Code::mismatched_parentheses, L_NIL));
            return i;
            break;

        default:
        DEFAULT:
            if (dotted_pair) {
                push_op(lisp::make_error(Error::Code::mismatched_parentheses,
                                         L_NIL));
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
            push_op(
                lisp::make_error(Error::Code::mismatched_parentheses, L_NIL));
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

            if (id >= symbol_intern_table and
                id < symbol_intern_table + string_intern_table_size) {
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
        if (is_list(lat->cons().car())) {
            push_op(lat->cons().car());
            macroexpand_macro();
            macroexpand();
            result.push_back(get_op0());
            pop_op();
        } else {
            result.push_back(lat->cons().car());
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
        case '(':
            ++i;
            pop_op(); // nil
            i += read_list(code, offset + i);
            macroexpand();
            // list now at stack top.
            return i;

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

        foreach (code->cons().cdr(), [&](Value* val) {
            eval(val);
            result.set(get_op0());
            pop_op();
        })
            ;
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

        foreach (bindings, [&](Value* val) {
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
        })
            ;

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

    foreach (code->cons().cdr(), [&](Value* val) {
        eval(val);
        result.set(get_op0());
        pop_op();
    })
        ;

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


static void eval_lambda(Value* code)
{
    // todo: argument list...

    push_op(make_lisp_function(code));
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
            if (str_eq(form->symbol().name(), "if")) {
                eval_if(code->cons().cdr());
                auto result = get_op0();
                pop_op(); // result
                pop_op(); // code
                push_op(result);
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "lambda")) {
                eval_lambda(code->cons().cdr());
                auto result = get_op0();
                pop_op(); // result
                pop_op(); // code
                push_op(result);
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "'")) {
                pop_op(); // code
                push_op(code->cons().cdr());
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "`")) {
                eval_quasiquote(code->cons().cdr());
                auto result = get_op0();
                pop_op(); // result
                pop_op(); // code
                push_op(result);
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "let")) {
                eval_let(code->cons().cdr());
                auto result = get_op0();
                pop_op();
                pop_op();
                push_op(result);
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "macro")) {
                eval_macro(code->cons().cdr());
                pop_op();
                // TODO: store macro!
                --bound_context->interp_entry_count_;
                return;
            } else if (str_eq(form->symbol().name(), "while")) {
                eval_while(code->cons().cdr());
                auto result = get_op0();
                pop_op();
                pop_op();
                push_op(result);
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
        // FIXME: this is problematic for large lists! Or datastructures with
        // cycles. Mainly intended for comparing single cons-cells.
        return is_equal(lhs->cons().car(), rhs->cons().car()) and
               is_equal(lhs->cons().cdr(), rhs->cons().cdr());

    case Value::Type::count:
    case Value::Type::__reserved:
    case Value::Type::nil:
    case Value::Type::heap_node:
    case Value::Type::data_buffer:
        return lhs == rhs;

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
}


static bool contains(Value* list, Value* val)
{
    bool result = false;
    foreach (list, [&](Value* v2) {
        if (is_equal(val, v2)) {
            result = true;
        }
    })
        ;

    return result;
}


const char* nameof(Function::CPP_Impl impl);


Value* stacktrace()
{
    return bound_context->callstack_->cons().cdr();
}


#ifdef __GBA__
#define BUILTIN_TABLE                                                          \
    MAPBOX_ETERNAL_CONSTEXPR const auto builtin_table =                        \
        mapbox::eternal::hash_map<mapbox::eternal::string, Builtin>
#else
#define BUILTIN_TABLE                                                          \
    const auto builtin_table = std::unordered_map<std::string, Builtin>
#endif

using RequiredArgc = int;
using Builtin = std::pair<RequiredArgc, lisp::Value* (*)(int)>;
// clang-format off
BUILTIN_TABLE(
    // clang-format on
    {{"set",
      {2,
       [](int argc) {
           L_EXPECT_OP(1, symbol);

           if (is_error(get_op0())) {
               return get_op0();
           }

           bool define_if_missing = not bound_context->strict_;

           return lisp::set_var(get_op1(), get_op0(), define_if_missing);
       }}},
     {"global",
      {1,
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
     {"strict",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           bound_context->strict_ = L_LOAD_INT(0);
           return L_NIL;
       }}},
     {"require-args",
      {2,
       [](int argc) {
           L_EXPECT_OP(1, function);
           L_EXPECT_OP(0, integer);

           lisp::Protected result(L_NIL);
           result = alloc_value();
           result->function() = get_op1()->function();
           result->function().required_args_ = L_LOAD_INT(0);

           return (Value*)result;
       }}},
     {"rot13",
      {1,
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
      {2,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().car();
       }}},
     {"first",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().car();
       }}},
     {"identity", {1, [](int argc) { return get_op0(); }}},
     {"cddr",
      {1,
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
      {1,
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
      {1,
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
      {1,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"second",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"rest",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           return get_op0()->cons().cdr();
       }}},
     {"list",
      {0,
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
      {2,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return get_arg(get_op0()->integer().value_);
       }}},
     {"abs",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return make_integer(abs(L_LOAD_INT(0)));
       }}},
     {"not",
      {1,
       [](int argc) { return make_integer(not is_boolean_true(get_op0())); }}},
     {"equal",
      {2,
       [](int argc) { return make_integer(is_equal(get_op0(), get_op1())); }}},
     {"list?", {1, [](int argc) { return make_boolean(is_list(get_op0())); }}},
     {"nil?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::nil);
       }}},
     {"int?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::integer);
       }}},
     {"float?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::fp);
       }}},
     {"pair?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::cons);
       }}},
     {"lambda?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::function);
       }}},
     {"error?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::error);
       }}},
     {"symbol?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::symbol);
       }}},
     {"userdata?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::user_data);
       }}},
     {"string?",
      {1,
       [](int argc) {
           return make_boolean(get_op0()->type() == Value::Type::string);
       }}},
     {"odd?",
      {1,
       [](int argc) {
           if (get_op0()->type() == Value::Type::integer) {
               return make_boolean(L_LOAD_INT(0) % 2);
           }
           return make_boolean(false);
       }}},
     {"int",
      {1,
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
      {1,
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
     {"apropos",
      {1,
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
      {2,
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
      {2,
       [](int argc) {
           L_EXPECT_OP(1, integer);

           auto result = make_list(get_op1()->integer().value_);
           for (int i = 0; i < get_op1()->integer().value_; ++i) {
               set_list(result, i, get_op0());
           }

           return result;
       }}},
     {"difference",
      {2,
       [](int argc) {
           ListBuilder list;

           auto find_difference = [&](Value* lat1, Value* lat2) {
               foreach (lat1, [&](Value* v1) {
                   if (not contains(lat2, v1)) {
                       list.push_back(v1);
                   }
               })
                   ;
           };
           find_difference(get_op0(), get_op1());
           find_difference(get_op1(), get_op0());

           return list.result();
       }}},
     {"union",
      {2,
       [](int argc) {
           ListBuilder list;

           foreach (get_op0(), [&](Value* v) {
               if (not contains(list.result(), v) and contains(get_op1(), v)) {
                   list.push_back(v);
               }
           })
               ;

           foreach (get_op1(), [&](Value* v) {
               if (not contains(list.result(), v) and contains(get_op0(), v)) {
                   list.push_back(v);
               }
           })
               ;

           return list.result();
       }}},
     {"collect",
      {1,
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
      {1,
       [](int argc) {
           if (get_op0()->type() == Value::Type::nil) {
               return make_integer(0);
           } else if (get_op0()->type() == Value::Type::string) {
               return make_integer(utf8::len(get_op0()->string().value()));
           }

           L_EXPECT_OP(0, cons);

           return make_integer(length(get_op0()));
       }}},
     {"<",
      {2,
       [](int argc) {
           if (get_op0()->type() == Value::Type::fp) {
               L_EXPECT_OP(1, fp);
               return make_integer(get_op1()->fp().value_ <
                                   get_op0()->fp().value_);
           }
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return make_integer(get_op1()->integer().value_ <
                               get_op0()->integer().value_);
       }}},
     {">",
      {2,
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
     {"&",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) & L_LOAD_INT(0));
       }}},
     {"|",
      {2,
       [](int argc) {
           int accum = 0;
           for (int i = 0; i < argc; ++i) {
               L_EXPECT_OP(i, integer);
               accum |= get_op(i)->integer().value_;
           }
           return make_integer(accum);
       }}},
     {"^",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) ^ L_LOAD_INT(0));
       }}},
     {"~",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           return L_INT(~L_LOAD_INT(0));
       }}},
     {"%",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) % L_LOAD_INT(0));
       }}},
     {">>",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) >> L_LOAD_INT(0));
       }}},
     {"<<",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, integer);
           L_EXPECT_OP(1, integer);
           return L_INT(L_LOAD_INT(1) << L_LOAD_INT(0));
       }}},
     {"hex",
      {1,
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
     {"+",
      {0,
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
      {1,
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
      {0,
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
      {2,
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
     {"gensym", {0, [](int) { return gensym(); }}},
     {"interp-stat",
      {0,
       [](int) {
           auto& ctx = bound_context;

           int values_remaining = 0;
           Value* current = value_pool;
           while (current) {
               ++values_remaining;
               current = current->heap_node().next_;
           }

           ListBuilder lat;

           auto make_stat = [&](const char* name, int value) {
               auto c = make_cons(get_nil(), get_nil());
               if (c == bound_context->oom_) {
                   return c;
               }
               push_op(c); // gc protect

               c->cons().set_car(
                   make_symbol(name, Symbol::ModeBits::stable_pointer));
               c->cons().set_cdr(make_integer(value));

               pop_op(); // gc unprotect
               return c;
           };

           lat.push_front(make_stat("vars", [&] {
               int symb_tab_used = 0;
               globals_tree_traverse(
                   ctx->globals_tree_,
                   [&symb_tab_used](Value&, Value&) { ++symb_tab_used; });
               return symb_tab_used;
           }()));

           lat.push_front(make_stat("stk", ctx->operand_stack_->size()));
           lat.push_front(make_stat("internb", ctx->string_intern_pos_));
           lat.push_front(make_stat("free", values_remaining));

           int databuffers = 0;

           for (int i = 0; i < VALUE_POOL_SIZE; ++i) {
               Value* val = (Value*)&value_pool_data[i];
               if (val->hdr_.alive_ and
                   val->hdr_.type_ == Value::Type::data_buffer) {
                   ++databuffers;
               }
           }

           lat.push_front(make_stat("sbr", databuffers));

           return lat.result();
       }}},
     {"range",
      {1,
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
      {0,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, string);

           return make_symbol(get_op0()->string().value());
       }}},
     {"format",
      {2,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, function);
           auto start = PLATFORM.delta_clock().sample();
           funcall(get_op0(), 0);
           auto stop = PLATFORM.delta_clock().sample();
           pop_op();
           return L_INT(stop - start);
       }}},
     {"slice",
      {2,
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
      {0,
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
      {0,
       [](int argc) {
           L_EXPECT_OP(0, cons);

           auto str = allocate_dynamic<StringBuffer<2000>>("tempstr");

           foreach (get_op0(), [&](Value* val) {
               auto v = val->integer().value_;
               auto cp = (const char*)&v;
               for (int i = 0; i < 4; ++i) {
                   if (cp[i]) {
                       str->push_back(cp[i]);
                   }
               }
           })
               ;

           return make_string(str->c_str());
       }}},
     {"string",
      {0,
       [](int argc) {
           EvalBuffer b;
           EvalPrinter p(b);

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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, string);
           return make_error(Error::Code::custom, get_op0());
       }}},
     {"bound?",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, symbol);

           auto found = globals_tree_find(get_op0());
           return make_integer(found not_eq nullptr);
       }}},
     {"filter",
      {2,
       [](int argc) {
           L_EXPECT_OP(0, cons);
           L_EXPECT_OP(1, function);

           auto fn = get_op1();
           Value* result = make_cons(L_NIL, L_NIL);
           auto prev = result;
           auto current = result;

           foreach (get_op0(), [&](Value* val) {
               push_op(result); // gc protect

               push_op(val);
               funcall(fn, 1);
               auto funcall_result = get_op0();

               if (is_boolean_true(funcall_result)) {
                   current->cons().set_car(val);
                   auto next = make_cons(L_NIL, L_NIL);
                   if (next == bound_context->oom_) {
                       current = result;
                       return;
                   }
                   current->cons().set_cdr(next);
                   prev = current;
                   current = next;
               }
               pop_op(); // funcall result

               pop_op(); // gc unprotect
           })
               ;

           if (current == result) {
               return L_NIL;
           }

           prev->cons().set_cdr(L_NIL);

           return result;
       }}},
     {"nameof",
      {1,
       [](int argc) {
           if (auto name = nameof(get_op0())) {
               return make_symbol(name);
           }
           return L_NIL;
       }}},
     {"map",
      {2,
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
      {2,
       [](int argc) {
           L_EXPECT_OP(0, cons);

           Optional<int> index;

           int i = 0;
           foreach (get_op0(), [&](Value* v) {
               if (index) {
                   return;
               }
               if (is_equal(get_op1(), v)) {
                   index = i;
               }
               ++i;
           })
               ;

           if (index) {
               return L_INT(*index);
           }

           return L_NIL;
       }}},
     {"flatten",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, cons);

           auto inp = get_op0();

           lisp::ListBuilder b;

           ::Function<6 * sizeof(void*), void(Value*)> flatten_impl(
               [](Value*) {});
           flatten_impl = [&](Value* val) {
               if (is_list(val)) {
                   foreach (val, flatten_impl)
                       ;
               } else {
                   b.push_back(val);
               }
           };

           foreach (inp, flatten_impl)
               ;

           return b.result();
       }}},
     {"reverse",
      {1,
       [](int argc) {
           if (get_op0()->type() not_eq lisp::Value::Type::cons) {
               return L_NIL;
           }

           L_EXPECT_OP(0, cons);

           Value* result = get_nil();
           foreach (get_op0(), [&](Value* car) {
               push_op(result);
               result = make_cons(car, result);
               pop_op();
           })
               ;

           return result;
       }}},
     {"gc", {0, [](int argc) { return make_integer(run_gc()); }}},
     {"get",
      {2,
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
      {1,
       [](int argc) {
           L_EXPECT_OP(0, string);
           BasicCharSequence seq(get_op0()->string().value());
           read(seq);
           auto result = get_op0();
           pop_op();
           return result;
       }}},
     {"eval",
      {1,
       [](int argc) {
           eval(get_op0());
           auto result = get_op0();
           pop_op(); // result

           return result;
       }}},
     {"stacktrace", {0, [](int argc) { return stacktrace(); }}},
     {"this",
      {0,
       [](int argc) {
           return bound_context->callstack_->cons().cdr()->cons().car();
       }}},
     {"env",
      {0,
       [](int argc) {
           auto pfrm = interp_get_pfrm();

           Value* result = make_cons(get_nil(), get_nil());
           push_op(result); // protect from the gc

           Value* current = result;

           get_env([&current, pfrm](const char* str) {
               current->cons().set_car(intern_to_symbol(str));
               auto next = make_cons(get_nil(), get_nil());
               if (next not_eq bound_context->oom_) {
                   current->cons().set_cdr(next);
                   current = next;
               }
           });

           pop_op(); // result

           return result;
       }}},
     {"compile",
      {1,
       [](int argc) {
           L_EXPECT_OP(0, function);

           if (get_op0()->hdr_.mode_bits_ ==
               Function::ModeBits::lisp_function) {
               compile(dcompr(get_op0()->function().lisp_impl_.code_));
               auto ret = get_op0();
               pop_op();
               return ret;
           } else {
               return get_op0();
           }
       }}},
     {"disassemble",
      {1, [](int argc) {
           L_EXPECT_OP(0, function);

           if (get_op0()->hdr_.mode_bits_ ==
               Function::ModeBits::lisp_bytecode_function) {

               Platform::RemoteConsole::Line out;

               u8 depth = 0;

               auto buffer = get_op0()->function().bytecode_impl_.databuffer();

               auto data = buffer->data_buffer().value();

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
                       out += to_string<10>(
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
                       out += to_string<10>(
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
                       out += to_string<10>(
                           ((HostInteger<s32>*)(data->data_ + i))->get());
                       out += ")";
                       i += 4;
                       break;

                   case PushSmallInteger::op():
                       out += "PUSH_SMALL_INTEGER(";
                       out += to_string<10>(*(data->data_ + i + 1));
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
                       out += to_string<10>(
                           ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                       out += ")";
                       i += 3;
                       break;

                   case Jump::op():
                       out += "JUMP(";
                       out += to_string<10>(
                           ((HostInteger<u16>*)(data->data_ + i + 1))->get());
                       out += ")";
                       i += 3;
                       break;

                   case SmallJumpIfFalse::op():
                       out += "SMALL_JUMP_IF_FALSE(";
                       out += to_string<10>(*(data->data_ + i + 1));
                       out += ")";
                       i += 2;
                       break;

                   case SmallJump::op():
                       out += "SMALL_JUMP(";
                       out += to_string<10>(*(data->data_ + i + 1));
                       out += ")";
                       i += 2;
                       break;

                   case PushLambda::op():
                       out += "PUSH_LAMBDA(";
                       out += to_string<10>(
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
                       out += to_string<10>(*(data->data_ + i + 1));
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
                       out += to_string<10>(*(data->data_ + i + 1));
                       out += ")";
                       i += 2;
                       break;

                   case PushList::op():
                       out += "PUSH_LIST(";
                       out += to_string<10>(*(data->data_ + i + 1));
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
                       out += to_string<10>(
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
                           pfrm->remote_console().printline(out.c_str(), "");
                           ((Platform*)pfrm)->sleep(80);
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

               Protected sym(make_symbol("lambda"));

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

    foreach (ctx->macros_,
             [&](Value* v) { callback(v->cons().car()->symbol().name()); })
        ;
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

    // Ok, no stable pointer to the string exists anywhere, so we'll have to
    // preserve the string contents in intern memory.

    if (len + 1 >
        string_intern_table_size - bound_context->string_intern_pos_) {

        PLATFORM.fatal("string intern table full");
    }

    const char* search = symbol_intern_table;
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

    auto result = symbol_intern_table + ctx->string_intern_pos_;

    for (u32 i = 0; i < len; ++i) {
        (symbol_intern_table)[ctx->string_intern_pos_++] = string[i];
    }
    (symbol_intern_table)[ctx->string_intern_pos_++] = '\0';

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

    return {0, nullptr};
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
        fn->function().required_args_ = builtin.first;
        return fn;
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
    auto var = lisp::get_var("gc");
    if (var->type() == lisp::Value::Type::function) {
        lisp::funcall(var, 0);
        lisp::pop_op();
    }
}


void init()
{
    if (bound_context) {
        return;
    }

    bound_context.emplace();

    value_pool_init();
    bound_context->nil_ = alloc_value();
    bound_context->nil_->hdr_.type_ = Value::Type::nil;
    bound_context->nil_->hdr_.mode_bits_ = 0;
    bound_context->globals_tree_ = bound_context->nil_;
    bound_context->callstack_ = bound_context->nil_;
    bound_context->lexical_bindings_ = bound_context->nil_;

    bound_context->oom_ = alloc_value();
    bound_context->oom_->hdr_.type_ = Value::Type::error;
    bound_context->oom_->error().code_ = Error::Code::out_of_memory;
    bound_context->oom_->error().context_ = compr(bound_context->nil_);

    bound_context->string_buffer_ = bound_context->nil_;
    bound_context->macros_ = bound_context->nil_;

    bound_context->tree_nullnode_ =
        make_cons(get_nil(), make_cons(get_nil(), get_nil()));

    // Push a few nil onto the operand stack. Allows us to access the first few
    // elements of the operand stack without performing size checks.
    push_op(get_nil());
    push_op(get_nil());


    if (dcompr(compr(get_nil())) not_eq get_nil()) {
        PLATFORM.fatal("pointer compression test failed 1");
    }

    push_callstack(make_string("toplevel"));
}


} // namespace lisp
