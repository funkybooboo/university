#include "WordPredictor.hpp"

#include "../WordTree/WordTree.hpp"
#include "rlutil.h"

#include <fstream>
#include <sstream>

WordPredictor::WordPredictor(const std::shared_ptr<WordTree>& wordTree) :
    wordTree(wordTree) {}

[[noreturn]] void WordPredictor::render() const
{
    std::string userInput;

    while (true)
    {
        rlutil::cls(); // Clear the console
        displayUserInput(userInput);

        std::string lastWord = getLastWord(userInput);
        auto predictions = wordTree->predict(lastWord, 10); // Get predictions

        displayPredictions(predictions);
        handleUserInput(userInput);
    }
}

template <typename Out>
void WordPredictor::split(const std::string& s, const char delim, Out result)
{
    std::istringstream iss(s);
    std::string item;
    while (std::getline(iss, item, delim))
    {
        *result++ = item;
    }
}

std::vector<std::string> WordPredictor::split(const std::string& s, const char delim)
{
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

void WordPredictor::displayUserInput(const std::string& userInput)
{
    rlutil::locate(1, 1);   // Move cursor to the start
    std::cout << userInput; // Display current input
}

std::string WordPredictor::getLastWord(const std::string& userInput)
{
    if (auto words = split(userInput, ' '); !words.empty())
    {
        return words.back(); // Get the most recent word
    }
    return "";
}

void WordPredictor::displayPredictions(const std::vector<std::string>& predictions)
{
    rlutil::locate(1, 3); // Move cursor to the prediction header line
    std::cout << "--- prediction ---" << std::endl;

    rlutil::locate(1, 4); // Move to prediction area
    for (const auto& pred : predictions)
    {
        std::cout << pred << std::endl;
    }
}

void WordPredictor::handleUserInput(std::string& userInput)
{
    if (const int key = rlutil::getkey(); key == rlutil::KEY_BACKSPACE)
    {
        // Handle backspace
        if (!userInput.empty())
        {
            userInput.pop_back(); // Remove last character
        }
    }
    else if (isprint(static_cast<char>(key)) || key == ' ')
    {
        // Check if the key is printable or space
        userInput += static_cast<char>(key); // Append character to input
    }
}