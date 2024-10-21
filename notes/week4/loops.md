## LOOPS

### Basics
Mostly similar to Java but there are some important differences.
We have the usual types
- while
- do while
- for
    - counted
        for (init-statement; loop-condition; iteration-expr) {body}

        The loop condition is a conditional that can resolve to a boolean or numeric value.
        It is interpreted the same way a conditional statement is in C++
    - ranged
        for (init-statement; range-declaration : range-expr) {body}

        Same synicatical patter that Java has. It does have some requirements though:
        - Can be a raw array
        - Can have any object that is compatible with .begin() and .end().
            - std::array and std::vector are examples of this.
        ```c++
        std::array primes{ 2, 3, 5, 7 };
        int sumOfPrimes{0};
        for (int prime : primes)
        {
            sumOfPrimes += prime;
        }
        ```
        - The init-statement is optional.
        - Can be used to define a type or declare/init a counter
        - Scope is within the loop
        ```c++
        std::array primes{ 2, 3, 5, 7 };
        for (auto which{1}; int prime : primes)
        {
            std::cout << std::format("The {} prime is {}\n", which++, prime);
        }
        ```
        