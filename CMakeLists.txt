cmake_minimum_required(VERSION 3.12)

# Config from project to project
set(PROJECT TypeAhead)
set(UNIT_TEST_RUNNER UnitTestRunner)

set(HEADER_FILES src/WordTree.hpp src/rlutil.h)
set(SOURCE_FILES src/WordTree.cpp)
set(UNIT_TEST_FILES src/TestWordTree.cpp)

# The rest should stay the same
project(${PROJECT})
add_executable(${PROJECT} ${HEADER_FILES} ${SOURCE_FILES} src/main.cpp)
add_executable(${UNIT_TEST_RUNNER} ${HEADER_FILES} ${SOURCE_FILES} ${UNIT_TEST_FILES})

set_property(TARGET ${PROJECT} PROPERTY CXX_STANDARD 20)
set_property(TARGET ${UNIT_TEST_RUNNER} PROPERTY CXX_STANDARD 20)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(${PROJECT} PRIVATE /W4 /permissive-)
    target_compile_options(${UNIT_TEST_RUNNER} PRIVATE /W4 /permissive-)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(${PROJECT} PRIVATE -Wall -Wextra -pedantic)
    target_compile_options(${UNIT_TEST_RUNNER} PRIVATE -Wall -Wextra -pedantic)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(${PROJECT} PRIVATE -Wall -Wextra -pedantic)
    target_compile_options(${UNIT_TEST_RUNNER} PRIVATE -Wall -Wextra -pedantic)
endif ()

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.15.0
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

target_link_libraries(${UNIT_TEST_RUNNER} gtest_main)

find_program(CLANG_FORMAT "clang-format")
if (CLANG_FORMAT)
    unset(SOURCE_FILES_PATHS)
    foreach(SOURCE_FILE ${HEADER_FILES} ${SOURCE_FILES} ${UNIT_TEST_FILES} main.cpp)
        get_source_file_property(WHERE ${SOURCE_FILES} LOCATION)
        set(SOURCE_FILES_PATHS ${SOURCE_FILES_PATHS} ${WHERE})
    endforeach ()

    add_custom_target(
            ClangFormat
            COMMAND ${CLANG_FORMAT}
            -i
            -style=file
            ${SOURCE_FILES_PATHS}
    )

    add_dependencies(${PROJECT} ClangFormat)
endif ()

add_custom_command(
        TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_SOURCE_DIR}/dictionary.txt dictionary.txt
)
