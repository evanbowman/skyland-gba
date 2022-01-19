
 Sometimes, we define
 infrequently used code as
 scripts rather than as
 global functions.

 By defining shared code as
 scripts, we save memory in
 the interpreter's string
 intern table.

 Symbols in the intern table
 can never be freed, so it
 makes sense to declare some
 functions as separate script
 files.

 Some might say that doing
 things this way is sort of
 convoluted, but it gives
 you the best of both worlds:
 named functions, without
 needing space in the lisp
 symbol table.

 Generally, code that
 invokes a function from a
 script file will do the
 following:

 (let ((func (eval-file "/scripts/utils/...")))
   (func ...))
