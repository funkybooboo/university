## CMAKE

### Basics
Meta-build system that can generate platform specific builds. It can create solutions for Visual Studio and valid make files for a linux system. With this, it also has a GUI and CLI.

For example, you'd start your CMAKE file with requiring the version, project name, and add your executables. You can also set the C++ standard for said executables. 
The cmake_minimum_required() is technically not required by CMAKE but it is starting to become more and more necessary depending on the minimum version you need.
You can have mulitple executables for a single project.

```cmake
cmake_minimum_required(VERSION 3.12)
project(HelloWorld)
add_executable(HelloWorld main.cpp)
set_property(TARGET HelloWorld PROPERTY CXX_STANDARD 20)
```

### Compiler Options
We can also set specific stuff depending on the compiler we're on. Like how we specified warnings and debug information as well as setting a specific stack size here.

```cmake
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(HelloWorld PRIVATE /W4 /permissive-)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(HelloWorld PRIVATE /W4 /permissive-)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(HelloWorld PRIVATE /W4 /permissive-)
endif()
```

### Multiple Files

We can make an executable with multiple files. This example combines header and implementation files.
```cmake
add_executable(BigProject
    main.cpp
    utilities.hpp
    utilities.cpp
    simulation.hpp
    simulation.cpp)
```

But typically, you'll do something more akin to this. Separate your source and header files from each other and add them as executables together after.
```cmake
set(HEADER_FILES
    utilities.hpp
    simulation.hpp)

set(SOURCE_FILES
    main.cpp
    utilities.cpp
    simulation.cpp)
    
add_executable(BigProject ${HEADER_FILES} ${SOURCE_FILES})
```