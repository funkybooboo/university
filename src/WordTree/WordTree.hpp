#pragma once

#include "TreeNode.hpp"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

class WordTree
{
  public:
    WordTree();
    void add(const std::string& word);
    [[nodiscard]] bool find(const std::string& word) const;
    [[nodiscard]] std::vector<std::string> predict(const std::string& partial, std::uint8_t howMany) const;
    [[nodiscard]] std::size_t size() const;

  private:
    std::uint32_t m_size;
    std::optional<std::shared_ptr<TreeNode>> m_root;

    static void tolower(std::string& word);
    static bool isAlphaOnly(const std::string& word);
    void addHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, size_t index);
    [[nodiscard]] bool findHelper(const std::shared_ptr<TreeNode>& node, const std::string& word, size_t index) const;
    [[nodiscard]] std::optional<std::shared_ptr<TreeNode>> traverseToEndOfPartial(const std::string& lowerPartial) const;
    static void collectPredictions(
        const std::shared_ptr<TreeNode>& currentNode,
        const std::string& lowerPartial,
        std::vector<std::string>& results,
        std::uint8_t howMany);
};
