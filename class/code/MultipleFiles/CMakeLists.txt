cmake_minimum_required(VERSION 3.12)

set(PROJECT_NAME MultipleFiles)
project(${PROJECT_NAME})

#
# Manually specifying all the source files.
#
set(HEADER_FILES
    utilities.hpp)

set(SOURCE_FILES
    main.cpp
    utilities.cpp)

#
# This is the main target
#
add_executable(${PROJECT_NAME} ${HEADER_FILES} ${SOURCE_FILES})

#
# We want the C++ 20 standard for our project
#
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 20)

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
    foreach(SOURCE_FILE ${HEADER_FILES} ${SOURCE_FILES})
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
    # This makes the clang-format target a dependency of the main MultipleFiles project
    #
    add_dependencies(${PROJECT_NAME} ClangFormat)
else()
    message("Unable to find clang-format")
endif()
