#pragma once

#include <cstdint>
#include <initializer_list>
#include <memory>

class Matrix
{
  public:
    Matrix();                                                                // Default, and delegating
    Matrix(std::size_t cols, std::size_t rows);                              // Overloaded
    Matrix(std::initializer_list<std::initializer_list<std::int32_t>> list);
    Matrix(const Matrix& matrix);
    Matrix(Matrix&& matrix);
    ~Matrix();

    Matrix& operator=(const Matrix& rhs);
    Matrix& operator=(Matrix&& rhs);
    std::int32_t& operator()(std::size_t row, std::size_t col); // Array-like access, but using () operator

    std::size_t getColumns() const { return m_cols; }
    std::size_t getRows() const { return m_rows; }

  private:
    std::size_t m_rows; // std::size_t because std::initializer_list uses it for the size.
    std::size_t m_cols;
    std::int32_t** m_data;

    void buildMemory(std::size_t rows, std::size_t cols);
    void cleanupMemory();
};
