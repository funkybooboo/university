#pragma once

#include <memory>
#include <functional>

namespace usu
{
    template <typename T, size_t InitialCapacity = 10>
    class vector
    {
    public:
        // The data type returned for the number of items in the vector
        using size_type = std::size_t;
        using reference = T&;
        using pointer = std::shared_ptr<T[]>;
        // The data type of the values stored in the vector
        using value_type = T;
        using resize_type = std::function<size_type(size_type)>;

        // Default constructor that initializes an empty vector,
        // using the default non-type template parameter initial capacity
        vector()
        {
            
        }

        // Overloaded constructor that takes a size_type and creates a vector of that size
        vector(size_type size)
        {
            
        }

        // Overloaded constructor that take a resize_type
        // and creates an empty vector using the resize_type parameter as the function
        // to determine how to update the capacity of the internal storage.
        vector(resize_type resize)
        {
            
        }

        // I know you can figure this one out
        vector(size_type size, resize_type resize)
        {
            
        }

        // Overloaded constructor
        // that takes an std::initializer_list of values and initializes the vector with those values.
        vector(std::initializer_list<T> list)
        {
            
        }

        // I know you can figure this one out
        vector(std::initializer_list<T> list, resize_type resize)
        {
            
        }

        // Returns a reference to the value at the position index.
        // If an invalid index is specified throw a std::range_error exception.
        reference operator[](size_type index)
        {
            
        }

        // Adds a new value at the end of the vector
        void add(T value)
        {
            
        }

        // Inserts a value before the position index in the vector.
        // If an invalid position is specified, throw a std::range_error exception.
        void insert(size_type index, T value)
        {
            
        }

        //  Removes the item at position index from the vector.
        //  If an invalid position is specified, throw a std::range_error exception.
        void remove(size_type index)
        {
            
        }

        // Removes all items in the vector (just sets the size to 0), does not change the capacity
        void clear()
        {
            
        }

        // Returns the number of items in the vector
        size_type size()
        {
            
        }

        // Returns the total capacity in the vector
        size_type capacity()
        {
            
        }

        //  Applies the function to all items in the vector
        void map(std::function<void(reference)> func)
        {
            
        }

        class iterator
        {
            
        };

        // Returns an iterator to the first item in the vector (or .end() if nothing in the vector)
        iterator begin()
        {
            
        }

        // Returns an iterator to the "end" of the vector
        iterator end()
        {
            
        }
    private:
        size_type m_size;
        size_type m_capacity;
        pointer m_data;
        resize_type m_resize = [](const size_type currentCapacity) -> size_type
        {
            return currentCapacity * 2;
        };
    };
} // namespace usu
