#include "Matrix.hpp"

#include <iostream>
#include <string>
#include <vector>

Matrix makeMatrix(std::size_t cols, std::size_t rows);
void reportMatrix(std::string name, Matrix& m);
Matrix copyMatrix(Matrix m);

int main()
{
    //Matrix m1; // Default constructor
    //Matrix m2(4, 4);        // Overload constructor
    //Matrix m3({ { 0, 1, 2 },
    //           { 3, 4, 5 },
    //           { 6, 7, 8 } });
    //Matrix m4 = m3; // Copy construction
    //Matrix m6 = makeMatrix(3, 2);       // What happens here?
    //m4 = m2;    // Assignment operator

    //Matrix m5 = copyMatrix(m1);
    //m5 = copyMatrix(m2);

    //m4(0, 0) = 10;

    //reportMatrix("Initializer List", m3);
    //reportMatrix("Copy Constucted", m4);

    Matrix m1(2, 2);
    Matrix m2 = Matrix(4, 4);
    m2 = copyMatrix(Matrix(3,6));

    return 0;
}

Matrix copyMatrix(Matrix m)
{
    return m;
}

// ------------------------------------------------------------------
//
// Demonstrate move constructor
//
// ------------------------------------------------------------------
Matrix makeMatrix(std::size_t rows, std::size_t cols)
{
    std::cout << "--- makeMatrix ---" << std::endl;
    Matrix m(rows, cols);

    int32_t value = 0;
    for (std::size_t row = 0; row < rows; row++)
    {
        for (std::size_t col = 0; col < cols; col++)
        {
            m(row, col) = value;
            value++;
        }
    }

    return m; // Move constructor (possibly) invoked
}

void reportMatrix(std::string name, Matrix& m)
{
    std::cout << "--- " << name << " ---" << std::endl;
    for (std::size_t row = 0; row < m.getRows(); row++)
    {
        for (std::size_t col = 0; col < m.getColumns(); col++)
        {
            std::cout << m(row, col) << ", ";
        }
        std::cout << std::endl;
    }

    std::cout << std::endl;
}
