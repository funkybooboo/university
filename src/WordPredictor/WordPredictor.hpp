#pragma once

#include "../WordTree/WordTree.hpp"

#include <memory>
#include <string>
#include <vector>

class WordPredictor
{
  public:
    explicit WordPredictor(const std::shared_ptr<WordTree>& wordTree);

    [[noreturn]] void render() const;

  private:
    std::shared_ptr<WordTree> wordTree;

    static std::vector<std::string> split(const std::string& s, char delim);

    template <typename Out>
    static void splitHelper(const std::string& s, char delim, Out result);

    static void displayUserInput(const std::string& userInput);

    static std::string getLastWord(const std::string& userInput);

    static void displayPredictions(const std::vector<std::string>& predictions);

    static void handleUserInput(std::string& userInput);
};