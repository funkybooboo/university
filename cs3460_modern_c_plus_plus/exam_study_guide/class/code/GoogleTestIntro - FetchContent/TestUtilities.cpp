#include "utilities.hpp"

#include "gtest/gtest.h"
#include <array>

//
// Test to validate the swap function works correctly
//
TEST(SwapTests, SwapTest)
{
    int a = 1;
    int b = 2;
    cs3460::swap(a, b);
    EXPECT_EQ(a, 2);
    EXPECT_EQ(b, 1);
}

//
// Verify some well known sin values
//
TEST(SinTests, SinSimple)
{
    EXPECT_DOUBLE_EQ(cs3460::sin(0), 0.0);
    EXPECT_DOUBLE_EQ(cs3460::sin(30), 0.5);
    EXPECT_DOUBLE_EQ(cs3460::sin(90), 1.0);
    EXPECT_DOUBLE_EQ(cs3460::sin(150), 0.5);
}

//
// Test against values of sin as computed from LibreCalc
//
TEST(SinTests, SinAll)
{
    const auto SIN_TOLERANCE = 0.002;

    std::array angle = {
        0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0, 120.0, 130.0, 140.0, 150.0, 160.0, 170.0
    };
    std::array result = {
        0.0,
        0.17364817766693,
        0.342020143325669,
        0.5,
        0.642787609686539,
        0.766044443118978,
        0.866025403784439,
        0.939692620785908,
        0.984807753012208,
        1.0,
        0.984807753012208,
        0.939692620785908,
        0.866025403784439,
        0.766044443118978,
        0.642787609686539,
        0.5,
        0.342020143325669,
        0.17364817766693, // trailing comma to keep clang-format from bringing the trailing brace up
    };

    for (std::size_t index = 0; index < angle.size(); index++)
    {
        EXPECT_NEAR(cs3460::sin(angle[index]), result[index], SIN_TOLERANCE) << "expected " << result[index] << " actual " << cs3460::sin(angle[index]);
    }
}

//
// Demonstration of using Google Test for unit testing
//
int main(int argc, char* argv[])
{

    testing::InitGoogleTest(&argc, argv);

    return RUN_ALL_TESTS();
}
