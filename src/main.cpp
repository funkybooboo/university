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

template <typename Out>
void split(const std::string& s, const char delim, Out result)
{
    std::istringstream iss(s);
    std::string item;
    while (std::getline(iss, item, delim))
    {
        *result++ = item;
    }
}

std::vector<std::string> split(const std::string& s, const char delim)
{
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

void printNLines(const std::uint8_t n)
{
    for (std::uint8_t i = 0; i < n; i++)
    {
        std::cout << "\n";
    }
}

[[noreturn]] void render(const std::shared_ptr<WordTree>& wordTree)
{
    std::string input;

    rlutil::cls();

    printNLines(4);

    rlutil::locate(3, 1);
    std::cout << "--- prediction ---";

    rlutil::locate(1, 1);
    std::cout << input;

    if (const int key = rlutil::getkey(); key == rlutil::KEY_BACKSPACE)
    {
        input.pop_back();
    }
    else
    {
        input.push_back(static_cast<char>(key));
    }

    std::vector<std::string> words = split(input, ' ');
    std::string word = words.back();
    std::vector<std::string> predictions = wordTree->predict(word, rlutil::trows() - 4);

    rlutil::locate(4, 1);
    for (const std::string& prediction : predictions)
    {
        std::cout << prediction << "\n";
    }

    while (true)
    {
        rlutil::cls();

        printNLines(4);

        rlutil::locate(3, 1);
        std::cout << "--- prediction ---";

        rlutil::locate(4, 1);
        for (const std::string& prediction : predictions)
        {
            std::cout << prediction << "\n";
        }

        rlutil::locate(1, 1);
        std::cout << input;

        if (const int key = rlutil::getkey(); key == rlutil::KEY_BACKSPACE)
        {
            input.pop_back();
        }
        else
        {
            input.push_back(static_cast<char>(key));
        }

        words = split(input, ' ');
        word = words.back();
        predictions = wordTree->predict(word, rlutil::trows() - 4);

        rlutil::cls();

        printNLines(4);

        rlutil::locate(1, 1);
        std::cout << input;

        rlutil::locate(3, 1);
        std::cout << "--- prediction ---";

        rlutil::locate(4, 1);
        for (const std::string& prediction : predictions)
        {
            std::cout << prediction << "\n";
        }
    }
}

int main()
{
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");
    render(wordTree);
}
