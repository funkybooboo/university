#pragma once

#include <unordered_map>
#include <memory>

class TreeNode {

    private:
        bool endOfWord;
        std::unordered_map<char, std::shared_ptr<TreeNode>> children;
};
