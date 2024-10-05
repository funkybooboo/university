#include "sortutils.hpp"

#include <algorithm>
#include <array>
#include <random>


SourceArray getRandomData(const int min = -10000000, const int max = 10000000) {
    SourceArray data;
    std::random_device rd;
    std::default_random_engine engine(rd());
    std::uniform_int_distribution distribution(min, max);
    for (std::size_t i = 0; i < HOW_MANY_ELEMENTS; i++) {
        data[i] = distribution(engine);
    }
    return data;
}

int main()
{
    const SourceArray random = getRandomData();

    SourceArray sorted = random;
    std::ranges::sort(sorted);

    SourceArray reversed = sorted;
    std::ranges::reverse(reversed);

    SourceArray organPipe = sorted;
    organPipeStdArray(organPipe);

    SourceArray rotated = sorted;
    std::ranges::rotate(rotated, rotated.begin() + 1);

    evaluateRawArray(random, sorted, reversed, organPipe, rotated);

    evaluateStdArray(random, sorted, reversed, organPipe, rotated);

    evaluateStdVector(random, sorted, reversed, organPipe, rotated);
}
