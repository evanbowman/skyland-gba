# Memory Allocation

The game's fundamental memory allocation mechanism is the reference counted
ScratchBuffer. ScratchBuffers are garbage collected buffers, containing at least
2KB on all target platforms. But you may be thinking, 2KB, that's not very
flexible! Surely, sometimes you want to allocate less than 2KB? So for that, we
have SubBuffers. The SubBufffer architecture slices a ScratchBuffer into a pool
of at least seven reference counted sub buffers, each containing at least 268
bytes of memory. For even smaller objects, the codebase generally uses
fixed-size memory pools, and also offers a class called ExtensionField, which
provides a means of allocating small 32 byte or smaller objects, and performs
compaction on ExtensionFields to squeeze out gaps (requiring memory stored in
extension fields to be trivially copyable). To summarize:

## ScratchBuffer:
A reference counted 2KB block of memory. You may allocate something within a
ScratchBuffer using the helper classes in allocator.hpp, or you may create
scratch buffers directly with the `make_scratch_buffer()` function. For
convenience, you may allocate an object using a scratch buffer via the
`allocate<T>()` function template.

## SubBuffer:
A 268 byte slice of a ScratchBuffer, also reference counted. You may allocate
objects via SubBuffers with the `allocate_small<T>()` function template.

## Pool:
Provides access to a fixed-size memory pool, containing N cells of memory.

## ExtensionField:
A 32 byte small object allocator. ExtensionFields are automatically compacted to
reduce fragmentation, and therefore, data stored in them must be trivially
copyable.


# What about malloc?
The game does not call malloc or new anywhere. Any references to malloc will
result in newlib dumping a whole bunch of junk into ram. Sure, there are other
malloc implementations, but we already have the fast, deterministic allocators
described above. Sure, they're not as flexible as malloc, but this software
application is fairly demanding and needs to run at, ideally, a steady 60fps on
a 16MHZ cpu with about a quarter of a megabyte of ram.


# "I need to allocate an array of things of an unspecified length, possibly larger than a scratch buffer"
See containers/vector.hpp
