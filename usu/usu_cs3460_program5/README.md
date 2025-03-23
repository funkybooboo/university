# Word Predictor

## Overview

The **Word Predictor** project is an interactive application that provides real-time word suggestions based on partial user input. By utilizing a tree (prefix tree) data structure, this application efficiently manages a dictionary of words, allowing users to receive instant predictions as they type. This tool aims to enhance typing speed and accuracy for various applications.

## How It Works

The core functionality of the Word Predictor is based on a tree data structure. The application performs the following steps when a user types:

1. **Input Collection**: The application collects the current user input and identifies the last word typed.
2. **tree Traversal**: It traverses the tree to locate the node corresponding to the last word.
3. **Prediction Generation**: The application gathers all possible completions for that word and displays them to the user.

The tree is constructed from a provided dictionary file, ensuring that predictions are relevant and based on real words. Each node in the tree represents a character of a word, allowing for efficient retrieval of possible completions.

## Getting the Code

To get a copy of the project, you can clone the repository using Git. Open your terminal and run the following command:

```bash
git clone https://github.com/funkybooboo/CS3460_Program5.git
```

This command will create a local copy of the repository on your machine. You can then navigate into the project directory:

```bash
cd CS3460_Program5
```

## Running the Code

To build and run the project, follow these steps:

1. Create a build directory:
   ```bash
   mkdir build
   ```
2. Navigate to the build directory:
   ```bash
   cd build
   ```
3. Run CMake to configure the project:
   ```bash
   cmake ..
   ```
4. Compile the project using make:
   ```bash
   make
   ```
5. Execute the application:
   ```bash
   ./TypeAhead
   ```

Alternatively, you can run the unit tests with:
```bash
./UnitTestRunner
```

