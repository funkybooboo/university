cmake_minimum_required(VERSION 3.12)    # 3.12 for C++20 standard support
project(ParallelAlgorithms)

set(SOURCE_FILES main.cpp)

#
# This is the main target
#
add_executable(ParallelAlgorithms ${SOURCE_FILES})

#
# Enable a lot of warnings for both compilers, forcing the developer to write better code
#
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(ParallelAlgorithms PRIVATE /W4 /permissive-)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(ParallelAlgorithms PRIVATE -O3 -Wall -Wextra -pedantic) # -Wconversion -Wsign-conversion
endif()

#
# We want the C++ 20 standard for our project
#
set_property(TARGET ParallelAlgorithms PROPERTY CXX_STANDARD 23)

#
# Prepare a pre-build step to run clang-format over all the [ch]pp source files.
# Start by finding the location of the clang-format executable.
#
find_program(CLANG_FORMAT "clang-format")
if (CLANG_FORMAT)
    message("Clang-Format found at: " ${CLANG_FORMAT})

    #
    # Need to take the simple source file locations used for the project and get their full
    # file system locations for use in putting together the clang-format command line
    #
    unset(SOURCE_FILES_PATHS)
    foreach(SOURCE_FILE ${SOURCE_FILES})
        get_source_file_property(WHERE ${SOURCE_FILE} LOCATION)
        set(SOURCE_FILES_PATHS ${SOURCE_FILES_PATHS} ${WHERE})
    endforeach()

    #
    # This creates the clang-format target/command
    #
    add_custom_target(
        ClangFormat
        COMMAND ${CLANG_FORMAT}
        -i
        -style=file
        ${SOURCE_FILES_PATHS}
    )
    #
    # This makes the clang-format target a dependency of the main ParallelAlgorithms project
    #
    add_dependencies(ParallelAlgorithms ClangFormat)
else()
    message("Unable to find clang-format")
endif()
