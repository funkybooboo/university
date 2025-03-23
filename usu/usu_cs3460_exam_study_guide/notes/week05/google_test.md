# Introduction to Google Test

## What is Google Test?
Google Test is a popular unit testing framework for C++ projects. Here’s what you need to know:

- **Purpose**: To help you test your C++ code and ensure it behaves as expected.
- **Requirements**: Requires a **C++14 standard-compliant compiler** (Version 1.14.x or later).
- **Integration**: Compiled with both the tests and the code being tested, making it easy to run tests on your functions.

### Key Features:
- **Large Suite of Assertions**: Provides a variety of ways to check code behavior.
- **Cross-Platform**: Works on Windows, Linux, macOS, and more.
- **CMake Integration**: Designed for easy integration with CMake, a widely used build system.

## CMake Integration

### Quick Setup
To integrate Google Test with your CMake project, you can follow these steps:

1. **Fetch Google Test**: Use CMake’s `FetchContent` to download Google Test automatically.

   ```cmake
   include(FetchContent)

   FetchContent_Declare(
       googletest
       GIT_REPOSITORY https://github.com/google/googletest.git
       GIT_TAG v1.15.0
   )

   FetchContent_MakeAvailable(googletest)
   ```

### CMakeLists.txt Setup
Here’s how to set up your `CMakeLists.txt` file:

1. **Define Source Files**:
   ```cmake
   set(HEADER_FILES utilities.hpp)
   set(SOURCE_FILES utilities.cpp)
   set(UNIT_TEST_FILES TestUtilities.cpp)
   ```

2. **Create Executable Targets**:
   ```cmake
   add_executable(GoogleTestIntro ${HEADER_FILES} ${SOURCE_FILES} main.cpp)
   add_executable(UnitTestRunner ${HEADER_FILES} ${SOURCE_FILES} ${UNIT_TEST_FILES})
   ```

3. **Link Google Test to Your Test Runner**:
   ```cmake
   target_link_libraries(UnitTestRunner gtest_main)
   ```

### Complete Example
Here’s a complete example of a simple `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.14)
project(GoogleTestExample)

include(FetchContent)

FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.15.0
)

FetchContent_MakeAvailable(googletest)

set(HEADER_FILES utilities.hpp)
set(SOURCE_FILES utilities.cpp)
set(UNIT_TEST_FILES TestUtilities.cpp)

add_executable(GoogleTestIntro ${HEADER_FILES} ${SOURCE_FILES} main.cpp)
add_executable(UnitTestRunner ${HEADER_FILES} ${SOURCE_FILES} ${UNIT_TEST_FILES})

target_link_libraries(UnitTestRunner gtest_main)
```

## Terminology
- **Test**: A set of assertions that check the behavior of code. If any assertion fails, the test fails.
- **Test Suite**: A collection of related tests.
- **Test Program**: The executable that runs the tests, which can contain multiple test suites.

## Making Assertions
Google Test provides many macros for making assertions about your code’s behavior. Here are the most common ones:

### Common Assertion Macros
1. **`ASSERT_*`**: If this assertion fails, the test stops immediately.
    - Example:
      ```cpp
      ASSERT_EQ(computed, 8); // Check if 'computed' is equal to 8
      ```

2. **`EXPECT_*`**: All assertions are evaluated, even if some fail.
    - Example:
      ```cpp
      EXPECT_EQ(computed, 8); // Check if 'computed' is equal to 8
      ```

### Example Code
Here’s how you might use these assertions in a test case:

```cpp
#include <gtest/gtest.h>

TEST(MathTests, Addition) {
    int result = 3 + 5;
    ASSERT_EQ(result, 8); // This test will pass.
}

TEST(MathTests, Subtraction) {
    int result = 5 - 3;
    EXPECT_EQ(result, 2); // This test will also pass.
}
```

## Testing Floating Point Numbers
When testing floating point numbers, use specific assertions to check if the values are close to each other:

### Floating Point Assertions
- **`ASSERT_FLOAT_EQ`** and **`EXPECT_FLOAT_EQ`**:
   ```cpp
   ASSERT_FLOAT_EQ(computed, expected); // Checks if they are close
   EXPECT_FLOAT_EQ(computed, expected); // Also checks if they are close
   ```

- **Defining Allowable Error**:
   ```cpp
   ASSERT_NEAR(computed, expected, absError); // Check if 'computed' is within 'absError' of 'expected'
   ```

### Example for Floating Point
```cpp
TEST(FloatingPointTests, Division) {
    float result = 1.0 / 3.0;
    ASSERT_FLOAT_EQ(result, 0.333333, 0.0001); // Will pass if within 0.0001 of expected
}
```

## Writing a Test
To write a test, use the `TEST` macro. The first parameter is the test suite name, and the second is the test name.

### Example: Testing a Swap Function
```cpp
void swap(int& x, int& y) {
    int temp = x;
    x = y;
    y = temp;
}

TEST(SwapTests, SwapFunction) {
    int a = 1;
    int b = 2;
    swap(a, b); // Call the function to test
    EXPECT_EQ(a, 2); // Assert that 'a' is now 2
    EXPECT_EQ(b, 1); // Assert that 'b' is now 1
}
```

## Executing Tests
To run the tests, you need to add the following lines to the main function of your test program:

```cpp
#include <gtest/gtest.h>

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS(); // Runs all tests
}
```
