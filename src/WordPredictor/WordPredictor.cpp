#include "WordPredictor.hpp"

#include "../WordTree/WordTree.hpp"
#include "rlutil.h"

#include <fstream>
#include <sstream>

/**
 * @brief Constructs a WordPredictor with the given WordTree.
 * @param wordTree A shared pointer to the WordTree used for predictions.
 */
WordPredictor::WordPredictor(const std::shared_ptr<WordTree>& wordTree) :
    wordTree(wordTree) {}

/**
 * @brief Renders the user interface for word prediction.
 *
 * This function continuously accepts user input, clears the console,
 * displays the current input, generates predictions for the last word,
 * and displays those predictions.
 */
[[noreturn]] void WordPredictor::render() const
{
    std::string userInput; // Stores the current user input

    while (true)
    {
        rlutil::cls();               // Clear the console
        displayUserInput(userInput); // Display current user input

        std::string lastWord = getLastWord(userInput);                       // Extract the last word
        auto predictions = wordTree->predict(lastWord, rlutil::trows() - 4); // Generate predictions

        displayPredictions(predictions); // Show predictions
        handleUserInput(userInput);      // Handle user input
    }
}

/**
 * @brief Splits a string into a vector of strings based on a delimiter.
 *
 * @param s The string to be split.
 * @param delim The delimiter character.
 * @return A vector containing the split strings.
 */
std::vector<std::string> WordPredictor::split(const std::string& s, const char delim)
{
    std::vector<std::string> elems;             // Vector to hold the resulting tokens
    splitHelper(s, delim, std::back_inserter(elems)); // Use the helper split function
    return elems;                               // Return the vector of tokens
}

/**
 * @brief Splits a string into tokens based on a delimiter.
 *
 * @tparam Out The output iterator type.
 * @param s The string to be split.
 * @param delim The delimiter character.
 * @param result The output iterator to write tokens to.
 */
template <typename Out>
void WordPredictor::splitHelper(const std::string& s, const char delim, Out result)
{
    std::istringstream iss(s); // Create a string stream from the input string
    std::string item;
    while (std::getline(iss, item, delim)) // Read until the delimiter
    {
        *result++ = item; // Write the token to the output iterator
    }
}

/**
 * @brief Displays the current user input at the top of the console.
 *
 * @param userInput The string representing the current user input.
 */
void WordPredictor::displayUserInput(const std::string& userInput)
{
    rlutil::locate(1, 1);   // Move cursor to the start of the console
    std::cout << userInput; // Output the current input
}

/**
 * @brief Retrieves the last word from the user input.
 *
 * @param userInput The full string of user input.
 * @return The last word as a string, or an empty string if none exists.
 */
std::string WordPredictor::getLastWord(const std::string& userInput)
{
    if (auto words = split(userInput, ' '); !words.empty())
    {
        return words.back(); // Return the most recent word
    }
    return ""; // Return an empty string if no words are found
}

/**
 * @brief Displays the predictions in the console.
 *
 * @param predictions A vector of predicted words to display.
 */
void WordPredictor::displayPredictions(const std::vector<std::string>& predictions)
{
    rlutil::locate(1, 3);                           // Move cursor to the prediction header line
    std::cout << "--- prediction ---" << std::endl; // Display header

    rlutil::locate(1, 4); // Move to the prediction area
    for (const auto& pred : predictions)
    {
        std::cout << pred << std::endl; // Output each predicted word
    }
}

/**
 * @brief Handles user input, including character entry and backspace.
 *
 * @param userInput A reference to the current user input string.
 */
void WordPredictor::handleUserInput(std::string& userInput)
{
    if (const int key = rlutil::getkey(); key == rlutil::KEY_BACKSPACE)
    {
        // Handle backspace key press
        if (!userInput.empty())
        {
            userInput.pop_back(); // Remove the last character
        }
    }
    else if (isprint(static_cast<char>(key)) || key == ' ')
    {
        // Check if the key is printable or a space
        userInput += static_cast<char>(key); // Append the character to the input
    }
}
