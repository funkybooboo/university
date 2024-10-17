#include "WordTree.hpp"

#include <algorithm>

WordTree::WordTree() :
    m_size(0), m_root(std::nullopt)
{
}

void WordTree::add(const std::string& word)
{
    if (!isAlphaOnly(word))
    {
        return;
    }

    std::string lowerWord = word;
    tolower(lowerWord);

    if (!m_root)
    {
        m_root = std::make_shared<TreeNode>();
    }

    auto currentNode = *m_root;

    // TODO implement
}

bool WordTree::find(const std::string& word) const
{
    if (!isAlphaOnly(word))
    {
        return false;
    }

    std::string lowerWord = word;
    tolower(lowerWord);

    if (!m_root)
    {
        return false;
    }

    auto currentNode = *m_root;

    // TODO implement

    return false;
}

std::vector<std::string> WordTree::predict(const std::string& partial, const std::uint8_t howMany) const
{
    std::vector<std::string> results;
    if (!isAlphaOnly(partial))
    {
        return results;
    }

    std::string lowerPartial = partial;
    tolower(lowerPartial);

    if (!m_root)
    {
        return results;
    }

    auto currentNode = *m_root;

    // TODO implement

    return results;
}

std::size_t WordTree::size() const
{
    return m_size;
}

void WordTree::tolower(std::string& word)
{
    std::ranges::transform(word.begin(), word.end(), word.begin(), ::tolower);
}

bool WordTree::isAlphaOnly(const std::string& word)
{
    return std::ranges::all_of(word, [](const char c)
                               {
                                   return std::isalpha(c);
                               });
}