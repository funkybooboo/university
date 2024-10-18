#pragma once

#include <memory>
#include <optional>
#include <map>

class TreeNode
{
public:
    TreeNode();

    [[nodiscard]] bool isEndOfWord() const;
    void setEndOfWord();

    [[nodiscard]] std::optional<std::shared_ptr<TreeNode>> findChild(char c) const;
    void addChild(char c);

    [[nodiscard]] const std::map<char, std::shared_ptr<TreeNode>>& getChildren() const;

private:
    bool m_endOfWord;
    std::map<char, std::shared_ptr<TreeNode>> m_children;
};
