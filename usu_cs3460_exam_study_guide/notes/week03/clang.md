## Clang Format

Clang Format is a tool we use to format our code to a certain specification automatically. This is a helpful developer tool that automatically formats code which is more helpful than requiring developers to manually do so.


This is the CMAKE integration for Clang Format.
First it finds the file named "clang-format" (it will either be .clang-format or _clang-format in the file system)
Next, if it's found, then a for loop is used to look through each source file and find the specific file path meant for each source file. Then you will add a custom target with the clang format file and add it to all of the source files. Then add both your original executable and the ClangFormat one to your dependencies.

```cmake
find_program(CLANG_FORMAT "clang-format")
if (CLANG_FORMAT)
    unset(SOURCE_FILES_PATHS)
    foreach(SOURCE_FILE ${SOURCE_FILES})
        get_source_file_property(WHERE ${SOURCE_FILE} LOCATION)
        set(SOURCE_FILES_PATHS ${SOURCE_FILES_PATHS} ${WHERE})
    endforeach()

    add_custom_target(
        ClangFormat
        COMMAND ${CLANG_FORMAT}
        -i
        -style=file
        ${SOURCE_FILES_PATHS})

    add_dependencies(<Executable of src files> ClangFormat)
endif()

```