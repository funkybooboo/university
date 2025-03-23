# Introduction to STL Parallel Execution

## Overview
- Introduced with the C++17 standard, with minor updates in C++20.
- A significant number of `<algorithm>` functions were updated to support parallel execution policies, including vectorized parallel execution.

## Execution Policies
Four execution policies are defined in `<execution>`:

1. **`std::execution::seq`**: 
   - **Description**: Sequential execution.
   - **Usage**: Default behavior if no policy is specified.

2. **`std::execution::par`**: 
   - **Description**: Parallel execution.
   - **Usage**: Allows algorithms to run in parallel.

3. **`std::execution::par_unseq`**: 
   - **Description**: Parallel and vectorized execution.
   - **Usage**: Enables both parallel execution and vectorization for optimal performance.

4. **`std::execution::unseq`**: 
   - **Description**: Single-threaded vectorized execution.
   - **Usage**: Optimizes for vectorization while running on a single thread.

### Important Note
- **No Protection**: There is no built-in protection against race conditions or deadlocks. It is the developer's responsibility to manage synchronization and avoid these issues.

## Specifying Execution Policy
- If no execution policy is specified, the default is sequential execution.
  ```cpp
  std::sort(myArray.begin(), myArray.end()); // Sequential by default
  ```

- To specify an execution policy, include it as the first parameter:
  ```cpp
  std::sort(std::execution::par, myArray.begin(), myArray.end()); // Parallel execution
  ```

### Key Takeaway
- **Simplicity**: Specifying an execution policy is straightforward.
- **Responsibility**: The developer must ensure there are no race or deadlock conditions when using parallel execution.


## Code Demonstration – Parallel Algorithms
```cpp
#include <algorithm>
#include <execution>
#include <vector>

int main() {
    std::vector<int> myArray = {1, 5, 3, 4, 2};

    // Sequential sort
    std::sort(myArray.begin(), myArray.end());

    // Parallel sort
    std::sort(std::execution::par, myArray.begin(), myArray.end());

    return 0;
}
```

In this example:
- The first sort is executed sequentially.
- The second sort utilizes parallel execution, potentially improving performance on large datasets.
