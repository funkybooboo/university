#include "TreeNode.hpp"

TreeNode::TreeNode() :
    m_endOfWord(false)
{
}

bool TreeNode::isEndOfWord() const
{
    return m_endOfWord;
}

void TreeNode::setEndOfWord()
{
    m_endOfWord = true;
}

std::optional<std::shared_ptr<TreeNode>> TreeNode::findChild(const char c) const
{
    if (m_children.contains(c))
    {
        return m_children.at(c);
    }
    return std::nullopt;
}

void TreeNode::addChild(const char c)
{
    if (!m_children.contains(c))
    {
        m_children[c] = std::make_shared<TreeNode>();
    }
}

const std::unordered_map<char, std::shared_ptr<TreeNode>>& TreeNode::getChildren() const
{
    return m_children;
}
