NOTE: For reasons that I will not articulate here, it is not realistic to bundle
a version of the Skyland LISP interpreter with the GBA rom. I would like to
provide tooling that parses some of the Skyland save data. Therefore, I am
writing utility scripts in the Scheme dialect of lisp.

Scheme and Skyland Lisp are syntactically very different. But their datatypes
are similar enough that things mostly work interoperably when simply parsing
lisp datastructures.
