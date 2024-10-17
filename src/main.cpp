#include "WordTree/WordTree.hpp"
#include "rlutil.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

std::shared_ptr<WordTree> readDictionary(const std::string& filename)
{
    auto wordTree = std::make_shared<WordTree>();
    auto inFile = std::ifstream(filename, std::ios::in);

    while (!inFile.eof())
    {
        std::string word;
        std::getline(inFile, word);
        // Need to consume the carriage return character for some systems, if it exists
        if (!word.empty() && word[word.size() - 1] == '\r')
        {
            word.erase(word.end() - 1);
        }
        // Keep only if everything is an alphabetic character -- Have to send isalpha an unsigned char or
        // it will throw exception on negative values; e.g., characters with accent marks.
        if (std::ranges::all_of(word, [](const unsigned char c)
                                {
                                    return std::isalpha(c);
                                }))
        {
            std::ranges::transform(word, word.begin(), [](const char c)
                                   {
                                       return static_cast<char>(std::tolower(c));
                                   });
            wordTree->add(word);
        }
    }

    return wordTree;
}

class Console
{
  public:
    static void render(const std::shared_ptr<WordTree>& wordTree);

  private:
    template <typename Out>
    static void split(const std::string& s, char delim, Out result);

    static std::vector<std::string> split(const std::string& s, char delim);

    static void displayInput(const std::string& input);

    static std::string updateLastWord(const std::string& input);

    static void displayPredictions(const std::vector<std::string>& predictions);

    static bool handleUserInput(std::string& input);
};

template <typename Out>
void Console::split(const std::string& s, const char delim, Out result)
{
    std::istringstream iss(s);
    std::string item;
    while (std::getline(iss, item, delim))
    {
        *result++ = item;
    }
}

std::vector<std::string> Console::split(const std::string& s, const char delim)
{
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

void Console::displayInput(const std::string& input)
{
    rlutil::locate(1, 1);
    std::cout << input << std::endl;
}

std::string Console::updateLastWord(const std::string& input)
{
    if (auto words = split(input, ' '); !words.empty())
    {
        return words.back();
    }
    return "";
}

void Console::displayPredictions(const std::vector<std::string>& predictions)
{
    rlutil::locate(1, 3); // Move to the prediction line
    std::cout << "--- prediction ---" << std::endl;
    for (const auto& word : predictions)
    {
        std::cout << word << std::endl;
    }
}

bool Console::handleUserInput(std::string& input)
{
    const int key = rlutil::getkey();
    if (key == 27) // Escape key to exit
    {
        return false; // Signal to exit
    }
    if (key == rlutil::KEY_BACKSPACE)
    {
        if (!input.empty())
        {
            input.pop_back(); // Remove last character
        }
    }
    else if (std::isprint(key))
    {
        input += static_cast<char>(key); // Append character to input
    }
    return true; // Continue running
}

void Console::render(const std::shared_ptr<WordTree>& wordTree)
{
    std::string input;
    std::vector<std::string> predictions;

    rlutil::cls();

    while (true)
    {
        displayInput(input);

        if (std::string lastWord = updateLastWord(input); !lastWord.empty())
        {
            predictions = wordTree->predict(lastWord, rlutil::trows() - 4); // Adjust for input line and title
        }

        displayPredictions(predictions);

        if (!handleUserInput(input))
        {
            break;
        }
    }
}

int main()
{
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");
    Console::render(wordTree);
    return 0;
}
