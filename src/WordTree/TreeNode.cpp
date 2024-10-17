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

std::optional<std::shared_ptr<TreeNode>> TreeNode::findChild(char c) const
{
    if (const auto it = m_children.find(c); it != m_children.end())
    {
        return it->second;
    }
    return std::nullopt;
}

void TreeNode::addChild(char c)
{
    m_children[c] = std::make_shared<TreeNode>();
}
