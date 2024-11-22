#pragma once

#include <memory>
#include <functional>

namespace usu
{
    template <typename T>
    class vector
    {
    public:
        using size_type = std::size_t;
        using reference = T&;
        using pointer = std::shared_ptr<T[]>;
        using value_type = T;
        using resize_type = std::function<size_type(size_type)>;

        vector()
        {
            
        }

        vector(size_type size)
        {
            
        }

        vector(resize_type resize)
        {
            
        }

        vector(size_type size, resize_type resize)
        {
            
        }

        vector(std::initializer_list<T> list)
        {
            
        }

        vector(std::initializer_list<T> list, resize_type resize)
        {
            
        }

        reference operator[](size_type index)
        {
            
        }

        void add(T value)
        {
            
        }

        void insert(size_type index, T value)
        {
            
        }

        void remove(size_type index)
        {
            
        }

        void clear()
        {
            
        }

        size_type size()
        {
            
        }

        size_type capacity()
        {
            
        }

        iterator begin()
        {
            
        }

        iterator end()
        {
            
        }

        void map(std::function<void(reference)> func)
        {
            
        }

        class iterator
        {
            
        };
    private:
        
    };
} // namespace usu
