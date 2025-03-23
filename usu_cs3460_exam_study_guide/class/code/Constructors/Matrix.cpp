#include "Matrix.hpp"

#include <cstring>
#include <iostream>

Matrix::Matrix() :
    Matrix(2, 2) // Delegating to an overloaded constructor
{
}

Matrix::Matrix(std::size_t rows, std::size_t cols)
{
    std::cout << "-- Overloaded Constructor --\n";
    // TODO: Should validate rows and cols are both non-zero

    buildMemory(rows, cols);
}

Matrix::Matrix(std::initializer_list<std::initializer_list<std::int32_t>> list) :
    Matrix(list.size(), list.begin()->size())
{
    std::size_t r = 0;
    for (auto row = list.begin(); row != list.end(); row++, r++)
    {
        std::size_t c = 0;
        for (auto column = row->begin(); column != row->end(); column++, c++)
        {
            m_data[r][c] = *column;
        }
    }
}

Matrix::Matrix(const Matrix& matrix)
{
    std::cout << "-- Copy Constructor --\n";
    buildMemory(matrix.m_rows, matrix.m_cols);
    for (std::size_t row = 0; row < m_rows; row++)
    {
        std::memcpy(m_data[row], matrix.m_data[row], sizeof(int32_t) * m_cols);
    }
}

Matrix::Matrix(Matrix&& matrix)
{
    std::cout << "-- Move Constructor --\n";
    m_rows = matrix.m_rows;
    m_cols = matrix.m_cols;
    m_data = matrix.m_data;

    matrix.m_rows = 0;
    matrix.m_cols = 0;
    matrix.m_data = nullptr;
}

Matrix::~Matrix()
{
    cleanupMemory();
}

Matrix& Matrix::operator=(const Matrix& rhs)
{
    std::cout << "-- Copy Assignment --\n";

    cleanupMemory();
    buildMemory(rhs.m_rows, rhs.m_cols);
    for (std::size_t row = 0; row < m_rows; row++)
    {
        std::memcpy(m_data[row], rhs.m_data[row], sizeof(int32_t) * m_cols);
    }
    return *this;
}

Matrix& Matrix::operator=(Matrix&& rhs)
{
    std::cout << "-- Move Assignment --\n";
    if (this != &rhs)
    {
        std::swap(m_rows, rhs.m_rows);
        std::swap(m_cols, rhs.m_cols);
        std::swap(m_data, rhs.m_data);
    }
    return *this;
}

// ------------------------------------------------------------------
//
// Array-like access to the data using the () operator
//
// ------------------------------------------------------------------
std::int32_t& Matrix::operator()(std::size_t row, std::size_t col)
{
    return m_data[row][col];
}

// ------------------------------------------------------------------
//
// We are doing our own memory management, as a means to show how to
// go about implementing the various constructor types and usages.
//
// ------------------------------------------------------------------
void Matrix::buildMemory(std::size_t rows, std::size_t cols)
{
    m_rows = rows;
    m_cols = cols;
    std::int32_t** data = new std::int32_t*[rows];
    for (decltype(rows) row = 0; row < rows; row++)
    {
        data[row] = new std::int32_t[cols];
        std::memset(data[row], 0, sizeof(std::int32_t) * cols);
    }

    m_data = data;
}

// ------------------------------------------------------------------
//
// Again, doing our own memory management, cleaning up any memory
// we have allocated to ourselves.
//
// ------------------------------------------------------------------
void Matrix::cleanupMemory()
{
    if (m_data != nullptr)
    {
        for (decltype(m_rows) row = 0; row < m_rows; row++)
        {
            delete[] m_data[row];
        }
        delete[] m_data;
        m_data = nullptr;
    }
}
