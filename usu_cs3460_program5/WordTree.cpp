#include "WordTree.hpp"

#include <algorithm>
#include <cctype>
#include <queue>
#include <ranges>

/**
 * @brief Default constructor for TreeNode.
 * Initializes the end-of-word flag to false.
 */
WordTree::TreeNode::TreeNode() :
    m_endOfWord(false) // Initialize endOfWord to false
{
}

/**
 * @brief Checks if this node marks the end of a word.
 * @return True if this node is the end of a word, false otherwise.
 */
bool WordTree::TreeNode::isEndOfWord() const
{
    return m_endOfWord; // Return the end-of-word status
}

/**
 * @brief Marks this node as the end of a word.
 */
void WordTree::TreeNode::setEndOfWord()
{
    m_endOfWord = true; // Set the end-of-word flag to true
}

/**
 * @brief Finds a child node corresponding to the given character.
 * @param c The character to find the child node for.
 * @return An optional shared pointer to the child node if it exists, otherwise nullopt.
 */
std::optional<std::shared_ptr<WordTree::TreeNode>> WordTree::TreeNode::findChild(const char c) const
{
    if (m_children.contains(c))
    {
        return m_children.at(c); // Return the child node if found
    }
    return std::nullopt; // Return nullopt if the child does not exist
}

/**
 * @brief Adds a child node for the given character if it does not already exist.
 * @param c The character for which to add a child node.
 */
void WordTree::TreeNode::addChild(const char c)
{
    if (!m_children.contains(c))
    {
        m_children[c] = std::make_shared<TreeNode>(); // Create a new child node
    }
}

/**
 * @brief Retrieves all child nodes of this TreeNode.
 * @return A constant reference to a map of child nodes.
 */
const std::map<char, std::shared_ptr<WordTree::TreeNode>>& WordTree::TreeNode::getChildren() const
{
    return m_children; // Return the map of child nodes
}

/**
 * @brief Default constructor for WordTree.
 * Initializes the root of the tree to null.
 */
WordTree::WordTree() :
    m_rootOpt(std::nullopt) // Initialize root to null
{
}

/**
 * @brief Adds a word to the WordTree.
 * @param word The word to be added.
 */
void WordTree::add(const std::string& word)
{
    // Validate the word: must be alphabetic and not empty
    if (!isAlphaOnly(word) || word.empty())
    {
        return; // Exit if the word is invalid
    }

    // Convert the word to lowercase
    std::string lowerWord = word;
    tolower(lowerWord);

    // Initialize the root if it doesn't exist
    if (!m_rootOpt)
    {
        m_rootOpt = std::make_shared<TreeNode>();
    }

    // Add the word to the tree
    addHelper(*m_rootOpt, lowerWord, 0);
}

/**
 * @brief Helper function to recursively add a word to the tree.
 * @param node The current node.
 * @param word The word being added.
 * @param index The current index in the word.
 */
void WordTree::addHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, const size_t index)
{
    // If we reached the end of the word, set the end-of-word flag
    if (index == word.length())
    {
        if (!node->isEndOfWord())
        {
            node->setEndOfWord();
        }
        return; // Exit the function
    }

    const char c = word[index];             // Get the current character
    auto childNodeOpt = node->findChild(c); // Check for existing child node

    // If the child node doesn't exist, create it
    if (!childNodeOpt)
    {
        node->addChild(c);
        childNodeOpt = node->findChild(c); // Retrieve the newly created child
    }

    // Recursively add the next character in the word
    addHelper(*childNodeOpt, word, index + 1);
}

/**
 * @brief Finds a word in the WordTree.
 * @param word The word to be searched for.
 * @return True if the word is found, false otherwise.
 */
bool WordTree::find(const std::string& word) const
{
    // Validate the word: must be alphabetic and not empty
    if (!isAlphaOnly(word) || word.empty())
    {
        return false; // Invalid word
    }

    std::string lowerWord = word; // Convert the word to lowercase
    tolower(lowerWord);

    // If the root doesn't exist, the word cannot be found
    if (!m_rootOpt)
    {
        return false;
    }

    // Search for the word in the tree
    return findHelper(*m_rootOpt, lowerWord, 0);
}

/**
 * @brief Helper function to recursively find a word in the tree.
 * @param node The current node.
 * @param word The word being searched for.
 * @param index The current index in the word.
 * @return True if the word is found, false otherwise.
 */
bool WordTree::findHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, const size_t index) const
{
    // If we reached the end of the word, check if it's marked as an end-of-word
    if (index == word.length())
    {
        return node->isEndOfWord();
    }

    const char c = word[index];                       // Get the current character
    if (const auto childNodeOpt = node->findChild(c)) // Check for the child node
    {
        // Continue searching in the child node
        return findHelper(*childNodeOpt, word, index + 1);
    }

    return false; // Return false if the character is not found
}

/**
 * @brief Predicts words based on a partial input.
 * @param partial The partial word input by the user.
 * @param howMany The maximum number of predictions to return.
 * @return A vector of predicted words.
 */
std::vector<std::string> WordTree::predict(const std::string& partial, const std::uint8_t howMany) const
{
    std::vector<std::string> results; // Store the prediction results
    // Validate the partial word and the number of predictions
    if (!isAlphaOnly(partial) || partial.empty() || howMany <= 0)
    {
        return results; // Exit with an empty result
    }

    std::string lowerPartial = partial; // Convert the partial word to lowercase
    tolower(lowerPartial);

    // If the root doesn't exist, return empty results
    if (!m_rootOpt)
    {
        return results;
    }

    const auto currentNode = traverseToEndOfPartial(lowerPartial); // Traverse to the end of the partial input
    if (!currentNode)
    {
        return results; // Exit if no matching node is found
    }

    // Collect predictions from the current node
    collectPredictions(*currentNode, lowerPartial, results, howMany);
    return results;
}

/**
 * @brief Traverses the tree to find the node corresponding to the end of the partial input.
 * @param lowerPartial The lowercase partial input.
 * @return An optional shared pointer to the end node if found.
 */
std::optional<std::shared_ptr<WordTree::TreeNode>> WordTree::traverseToEndOfPartial(const std::string& lowerPartial) const
{
    if (!m_rootOpt)
    {
        return std::nullopt; // Return nullopt if the root doesn't exist
    }
    auto currentNode = *m_rootOpt; // Start at the root node

    // Traverse the tree character by character
    for (const char c : lowerPartial)
    {
        auto childNodeOpt = currentNode->findChild(c); // Find the child node
        if (!childNodeOpt)
        {
            return std::nullopt; // Return nullopt if the character is not found
        }
        currentNode = *childNodeOpt; // Move to the child node
    }

    return currentNode; // Return the node at the end of the partial input
}

/**
 * @brief Collects predicted words using a breadth-first search approach.
 * @param currentNode The starting node for predictions.
 * @param lowerPartial The partial word.
 * @param results The vector to store results.
 * @param howMany The maximum number of predictions to collect.
 */
void WordTree::collectPredictions(
    const std::shared_ptr<TreeNode>& currentNode,
    const std::string& lowerPartial,
    std::vector<std::string>& results,
    const std::uint8_t howMany)
{
    std::queue<std::pair<std::shared_ptr<TreeNode>, std::string>> bfsQueue; // Queue for BFS
    bfsQueue.emplace(currentNode, lowerPartial);                            // Start with the current node and partial word

    // Continue until the queue is empty, or we have enough predictions
    while (!bfsQueue.empty() && results.size() < howMany)
    {
        auto [node, currentWord] = bfsQueue.front(); // Get the front of the queue
        bfsQueue.pop();                              // Remove it from the queue

        // If this node marks the end of a word, add it to results
        if (node->isEndOfWord() && currentWord != lowerPartial)
        {
            results.push_back(currentWord);
        }

        // Enqueue all child nodes
        for (const auto& [childChar, childNode] : node->getChildren())
        {
            bfsQueue.emplace(childNode, currentWord + childChar); // Append the character and add to queue
        }
    }
}

/**
 * @brief Counts the total number of words in the tree.
 * @return The total number of words.
 */
std::size_t WordTree::size() const
{
    return m_rootOpt ? countWords(*m_rootOpt) : 0; // Return the word count if root exists
}

/**
 * @brief Helper function to count words recursively.
 * @param node The current node.
 * @return The count of words in the subtree.
 */
std::size_t WordTree::countWords(const std::shared_ptr<TreeNode>& node)
{
    if (!node)
        return 0; // Base case: return 0 if node is null

    std::size_t count = node->isEndOfWord() ? 1 : 0; // Count this node if it's an end of word

    // Recursively count words in all children
    for (const auto& childNode : node->getChildren() | std::views::values)
    {
        count += countWords(childNode); // Accumulate the count from child nodes
    }

    return count; // Return the total count
}

/**
 * @brief Converts a word to lowercase.
 * @param word The word to convert.
 */
void WordTree::tolower(std::string& word)
{
    std::ranges::transform(word.begin(), word.end(), word.begin(), ::tolower); // Transform to lowercase
}

/**
 * @brief Checks if a string contains only alphabetic characters.
 * @param word The string to check.
 * @return True if all characters are alphabetic, false otherwise.
 */
bool WordTree::isAlphaOnly(const std::string& word)
{
    return std::ranges::all_of(word, [](const char c)
                               {
                                   return std::isalpha(c); // Check each character
                               });
}
