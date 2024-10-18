#include "WordTree.hpp"

#include <algorithm>
#include <cctype>
#include <queue>

WordTree::WordTree() :
    m_size(0), m_root(std::nullopt)
{
}

void WordTree::add(const std::string& word)
{
    if (!isAlphaOnly(word) || word.empty())
    {
        return;
    }

    std::string lowerWord = word;
    tolower(lowerWord);

    if (!m_root)
    {
        m_root = std::make_shared<TreeNode>();
    }

    addHelper(*m_root, lowerWord, 0);
}

void WordTree::addHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, const size_t index)
{
    if (index == word.length())
    {
        if (!node->isEndOfWord())
        {
            node->setEndOfWord();
            m_size++;
        }
        return;
    }

    const char c = word[index];
    auto childNodeOpt = node->findChild(c);
    if (!childNodeOpt)
    {
        node->addChild(c);
        childNodeOpt = node->findChild(c);
    }

    addHelper(*childNodeOpt, word, index + 1);
}

bool WordTree::find(const std::string& word) const
{
    if (!isAlphaOnly(word) || word.empty())
    {
        return false;
    }

    std::string lowerWord = word;
    tolower(lowerWord);

    if (!m_root)
    {
        return false;
    }

    return findHelper(*m_root, lowerWord, 0);
}

bool WordTree::findHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, const size_t index) const
{
    if (index == word.length())
    {
        return node->isEndOfWord();
    }

    const char c = word[index];
    if (const auto childNodeOpt = node->findChild(c))
    {
        return findHelper(*childNodeOpt, word, index + 1);
    }

    return false;
}

std::vector<std::string> WordTree::predict(const std::string& partial, const std::uint8_t howMany) const
{
    std::vector<std::string> results;
    if (!isAlphaOnly(partial) || partial.empty() || howMany <= 0)
    {
        return results;
    }

    std::string lowerPartial = partial;
    tolower(lowerPartial);

    if (!m_root)
    {
        return results;
    }

    const auto currentNode = traverseToEndOfPartial(lowerPartial);
    if (!currentNode)
    {
        return results;
    }

    collectPredictions(*currentNode, lowerPartial, results, howMany);
    return results;
}

std::optional<std::shared_ptr<TreeNode>> WordTree::traverseToEndOfPartial(const std::string& lowerPartial) const
{
    auto currentNode = *m_root;

    for (const char c : lowerPartial)
    {
        auto childNodeOpt = currentNode->findChild(c);
        if (!childNodeOpt)
        {
            return std::nullopt;
        }
        currentNode = *childNodeOpt;
    }

    return currentNode;
}

void WordTree::collectPredictions(
    const std::shared_ptr<TreeNode>& currentNode,
    const std::string& lowerPartial,
    std::vector<std::string>& results,
    const std::uint8_t howMany)
{
    std::queue<std::pair<std::shared_ptr<TreeNode>, std::string>> bfsQueue;
    bfsQueue.emplace(currentNode, lowerPartial);

    while (!bfsQueue.empty() && results.size() < howMany)
    {
        auto [node, currentWord] = bfsQueue.front();
        bfsQueue.pop();

        if (node->isEndOfWord() && currentWord != lowerPartial)
        {
            results.push_back(currentWord);
        }

        for (const auto& [childChar, childNode] : node->getChildren())
        {
            bfsQueue.emplace(childNode, currentWord + childChar);
        }
    }
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