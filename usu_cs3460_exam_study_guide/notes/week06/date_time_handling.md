# Introduction to Date & Time Handling in C++

## Overview
- C++11 introduced robust support for time handling.
- C++20 added strong support for date handling.
  
### Key Concepts
- **Duration**: A span of time (e.g., hours, minutes).
- **Time Point**: A specific moment in time (e.g., 4th of July).
- **Clock**: Used to obtain a time point.
- **Calendar**: Represents year, month, and day.

### Header File
- Include the necessary header:
  ```cpp
  #include <chrono>
  ```

## Duration
A duration is composed of two pieces of information:
1. **Period**: Unit of time as a fraction of a second.
2. **Count**: Number of periods in the duration.

### Examples of Duration
- **Example A**:
    - Period: 1 second
    - Count: 10
    - Represents: 10 seconds

- **Example B**:
    - Period: 0.001 seconds (1 millisecond)
    - Count: 10,000
    - Represents: 10 seconds

### Code Examples
```cpp
std::chrono::duration<std::uint32_t, std::ratio<1, 1>> seconds(10);
std::chrono::duration<std::uint32_t, std::ratio<1, 1000>> milliseconds(10000);
std::chrono::duration<std::uint32_t, std::ratio<60, 1>> minutes(4);
```

### Key Points
- **Type**: `std::chrono::duration`
- **Underlying Data Type**: Can be integral or floating point.

## Type Aliases for Durations
To simplify declarations, you can create type aliases:

### Example
```cpp
using seconds = std::chrono::duration<std::uint32_t, std::ratio<1, 1>>;
using milliseconds = std::chrono::duration<std::uint32_t, std::ratio<1, 1000>>;
using minutes = std::chrono::duration<std::uint32_t, std::ratio<60, 1>>;

seconds s(10);
milliseconds ms(10000);
minutes min(4);
```

### Built-in Types
You can also use the provided types for common durations:
```cpp
std::chrono::seconds s(10);
std::chrono::milliseconds ms(10000);
std::chrono::minutes min(4);
```

## Duration Conversions
- **Non-narrowing conversions** occur automatically:
  ```cpp
  std::chrono::seconds seconds(30);
  std::chrono::minutes minutes(1);
  std::chrono::seconds minutesToSeconds = minutes; // Automatic conversion
  ```

- **Narrowing conversions** require a cast:
  ```cpp
  std::chrono::seconds seconds(30);
  std::chrono::minutes minutes(1);
  minutes += std::chrono::duration_cast<std::chrono::minutes>(seconds);
  ```

## Clocks
A clock is defined by:
- **Epoch**: A starting point (e.g., 1/1/1970).
- **Tick Rate**: Rate at which time passes.

### Types of Clocks in `std::chrono`
- `system_clock`: Real-time clock (wall clock).
- `steady_clock`: Monotonic clock.
- `high_resolution_clock`: Shortest possible tick.
- `utc_clock`: Coordinated Universal Time.
- `tai_clock`: International Atomic Time.
- `gps_clock`: GPS time.
- `file_clock`: Represents filesystem times.

### Clock Methods
- **`now`**: Returns a `time_point` of the current time.

## Time Points
- Represents a point in time based on a clock's epoch.
- All clocks have a `::now` method to get the current `time_point`:
  ```cpp
  std::chrono::time_point<std::chrono::system_clock> now = std::chrono::system_clock::now();
  ```

## Calendars
A calendar represents a specific year, month, and day.

### Example Types
- `std::chrono::day`
- `std::chrono::month` (e.g., `std::chrono::January`)
- `std::chrono::weekday` (e.g., `std::chrono::Sunday`)
- `std::chrono::year_month_day`

## Adding Days to `year_month_day`
You cannot directly add days to a `year_month_day`. Here’s how to do it:

### Steps
1. Convert `year_month_day` to days:
   ```cpp
   auto ymd = std::chrono::January/1/2001;
   auto days = std::chrono::sys_days{ymd};
   ```
2. Add the desired number of days:
   ```cpp
   days += std::chrono::days{7};
   ```
3. Convert back to `year_month_day`:
   ```cpp
   ymd = std::chrono::year_month_day{days};
   ```
