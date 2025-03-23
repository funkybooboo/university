# Pi Digit Calculator

This project computes the digits of Pi using an efficient algorithm based on Bellard's work. 
By default, it calculates the first 1000 digits of Pi using all available threads for optimal performance.

## Table of Contents

- [Project Structure](#project-structure)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Classes Overview](#classes-overview)
- [License](#license)
- [Credits](#credits)

## Project Structure

```
CS3100_Program4/
├── build.gradle.kts
├── gradlew
├── gradlew.bat
├── LICENSE
├── src/
│   └── main/
│       └── java/
│           └── org/
│               └── natestott/
│                   ├── Main.java
│                   ├── DigitWorker.java
│                   ├── ResultTable.java
│                   ├── TaskQueue.java
│                   └── numberComputer/
│                       ├── NumberComputer.java
│                       └── PiComputer.java
│                   └── digitCalculator/
│                       ├── DigitCalculator.java
│                       └── Bpp.java
└── README.md
```

## Features

- Computes the first 1000 digits of Pi by default.
- Utilizes all available threads to speed up the computation.
- Based on advanced mathematical techniques to minimize resource usage.
- Multithreaded implementation for efficient digit calculation.

## Installation

To run this project, ensure you have Java and Gradle installed on your machine. 
You can download Java from the [official website](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html).

1. Clone the repository:
   ```bash
   git clone https://github.com/funkybooboo/CS3100_Program4.git
   ```
2. Navigate into the project directory:
   ```bash
   cd CS3100_Program4
   ```

## Usage

To compute the digits of Pi, you can run the project using Gradle. 
The program is configured to calculate the first 1000 digits by default.

### Running the Project

Use the following command to run the project:

```bash
./gradlew run
```

This will execute the `Main` class, compute the first 1000 digits of Pi, and print the result along with the computation time.

## Classes Overview

- **Main.java**: The entry point of the application. It initializes the `PiComputer` and starts the computation.

- **DigitWorker.java**: Extends `Thread` and is responsible for fetching tasks from the `TaskQueue`, calculating the digit using the `DigitCalculator`, and storing results in `ResultTable`.

- **ResultTable.java**: Manages the results of the computations, ensuring thread-safe access using a `ReentrantLock`.

- **TaskQueue.java**: A thread-safe queue for managing tasks to be processed by worker threads.

- **NumberComputer.java**: An interface that defines methods for computing numbers, including a method for printing the results.

- **PiComputer.java**: Implements the `NumberComputer` interface, computes digits of Pi, and manages task distribution among worker threads.

- **DigitCalculator.java**: An interface that defines the method for retrieving specific digits.

- **Bpp.java**: Implements the `DigitCalculator` interface, containing the algorithm to calculate the digits of Pi based on Bellard's method.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more information.

## Credits

- **Original Author:** feltocraig
- **Modified by:** Nate Stott
- This project is inspired by Bellard's work on calculating the digits of Pi.
