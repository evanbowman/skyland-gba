cmake_minimum_required(VERSION 3.5)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# The LISP interpreter is already compiled into BlindJump, this cmakelists,
# disconnected from other cmake scripts in the source tree, is intended for
# compiling a standalone version of the interpreter, for testing and development
# purposes.
project(LISP)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../external/)


add_executable(LISP
  vm.cpp
  lisp.cpp
  unittest.cpp
  compiler.cpp
  bootstrap.cpp)


target_compile_options(LISP PRIVATE
  -g3
  -O3)


# add_executable(LISPC
#   vm.cpp
#   lisp.cpp
#   commandlineCompiler.cpp
#   compiler.cpp
#   bootstrap.cpp)


# target_compile_options(LISPC PRIVATE
#   -g3)


# add_executable(LISPP
#   vm.cpp
#   lisp.cpp
#   commandlineDisassembler.cpp
#   compiler.cpp
#   bootstrap.cpp)


# target_compile_options(LISPC PRIVATE
#   -g3)
