#include <chrono>
#include <format>
#include <iostream>

void demoClocks()
{
    auto periodSystem = std::chrono::system_clock::period();
    std::cout << "System Clock Period          : "
              << periodSystem.num << " / "
              << periodSystem.den << std::endl;

    auto periodSteady = std::chrono::steady_clock::period();
    std::cout << "Steady Clock Period          : "
              << periodSteady.num << " / "
              << periodSteady.den << std::endl;

    auto periodHigh = std::chrono::high_resolution_clock::period();
    std::cout << "High Resolution Clock Period : "
              << periodHigh.num << " / "
              << periodHigh.den << std::endl;
}

void demoTimePoints()
{
    auto systemNow = std::chrono::system_clock::now();
    std::cout << "System clock periods since clock epoch          : "
              << systemNow.time_since_epoch().count() << std::endl;

    auto steadyNow = std::chrono::steady_clock::now();
    std::cout << "Steady clock periods since clock epoch          : "
              << steadyNow.time_since_epoch().count() << std::endl;

    auto highResolutionNow = std::chrono::high_resolution_clock::now();
    std::cout << "High resolution clock periods since clock epoch : "
              << highResolutionNow.time_since_epoch().count() << std::endl;

    auto systemHours =
        std::chrono::duration_cast<std::chrono::hours>(systemNow.time_since_epoch());
    std::cout << "System clock hours since epoch          : "
              << systemHours.count() << std::endl;

    std::chrono::hours steadyHours =
        std::chrono::duration_cast<std::chrono::hours>(steadyNow.time_since_epoch());
    std::cout << "Steady clock hours since epoch          : "
              << steadyHours.count() << std::endl;

    std::chrono::hours highResolutionHours =
        std::chrono::duration_cast<std::chrono::hours>(highResolutionNow.time_since_epoch());
    std::cout << "High resolution clock hours since epoch : "
              << highResolutionHours.count() << std::endl;
}

std::uint64_t fibonacci(std::uint16_t n)
{
    if (n == 0)
    {
        return 0;
    }
    if (n == 1)
    {
        return 1;
    }

    return fibonacci(n - 1) + fibonacci(n - 2);
}

void demoTimingThings(std::uint16_t n)
{
    //    const std::uint16_t FIB_NUMBER = 40;
    auto start = std::chrono::steady_clock::now();
    auto result = fibonacci(n);
    auto end = std::chrono::steady_clock::now();

    auto diffMilliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    std::cout << std::format("The fib({}) is {} and took {} ms\n",
                             n,
                             result,
                             diffMilliseconds.count());
}

int main()
{
    using namespace std::chrono;

    // using myseconds = duration<std::uint32_t, std::ratio<1, 1>>;
    // using mymilliseconds = duration<std::uint32_t, std::ratio<1, 1000>>;
    // using myminutes = duration<double, std::ratio<60, 1>>;

    // myseconds s(10);
    // mymilliseconds ms(10000);
    // myminutes min(4);

    // seconds s2(10);
    // milliseconds ms2(10000);
    // minutes min2(4);

    // std::chrono::seconds seconds(30);
    // std::chrono::minutes someMinutes(1);
    // std::chrono::seconds minutesToSeconds = someMinutes;

    // std::cout << minutesToSeconds.count() << std::endl;

    // someMinutes += duration_cast<minutes>(seconds);
    // someMinutes += duration_cast<minutes>(seconds);

    // std::cout << someMinutes.count() << std::endl;

    // std::chrono::seconds seconds(30);
    // myminutes minutes(1);

    // minutes += std::chrono::duration_cast<myminutes>(seconds);
    // minutes += std::chrono::duration_cast<myminutes>(seconds);

    // std::cout << minutes.count() << std::endl;

    // demoClocks();
    // demoTimePoints();

    std::uint16_t fibValue{ 0 };
    std::cout << "Enter the number: ";
    std::cin >> fibValue;

    demoTimingThings(fibValue);

    return 0;
}
