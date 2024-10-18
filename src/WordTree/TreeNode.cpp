#include "TreeNode.hpp"

/**
 * @brief Default constructor for TreeNode.
 * Initializes the end-of-word flag to false.
 */
TreeNode::TreeNode() :
    m_endOfWord(false) // Initialize endOfWord to false
{
}

/**
 * @brief Checks if this node marks the end of a word.
 * @return True if this node is the end of a word, false otherwise.
 */
bool TreeNode::isEndOfWord() const
{
    return m_endOfWord; // Return the end-of-word status
}

/**
 * @brief Marks this node as the end of a word.
 */
void TreeNode::setEndOfWord()
{
    m_endOfWord = true; // Set the end-of-word flag to true
}

/**
 * @brief Finds a child node corresponding to the given character.
 * @param c The character to find the child node for.
 * @return An optional shared pointer to the child node if it exists, otherwise nullopt.
 */
std::optional<std::shared_ptr<TreeNode>> TreeNode::findChild(const char c) const
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
void TreeNode::addChild(const char c)
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
const std::map<char, std::shared_ptr<TreeNode>>& TreeNode::getChildren() const
{
    return m_children; // Return the map of child nodes
}
