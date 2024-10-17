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

void render(const std::shared_ptr<WordTree>& wordTree)
{

}

int main()
{
    const std::shared_ptr<WordTree> wordTree = readDictionary("../data/dictionary.txt");
    render(wordTree);
}
