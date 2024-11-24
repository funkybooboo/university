#pragma once

#include <exception>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <utility>

namespace usu
{
    template <typename T, size_t InitialCapacity = 10>
    class vector
    {
      public:
        using size_type = std::size_t;
        using value_type = T;
        using reference = value_type&;
        using pointer = std::shared_ptr<value_type[]>;
        using resize_type = std::function<size_type(size_type)>;

        // Default constructor that initializes an empty vector,
        // using the default non-type template parameter initial capacity
        vector()
        {
            vector(0);
            m_capacity = InitialCapacity;
        }

        // Overloaded constructor that takes a size_type and creates a vector of that size
        explicit vector(const size_type size)
        {
            m_size = size;
            m_capacity = m_resize(m_size);
            m_data = std::make_shared<T[]>(m_capacity);
        }

        // Overloaded constructor that take a resize_type
        // and creates an empty vector using the resize_type parameter as the function
        // to determine how to update the capacity of the internal storage.
        explicit vector(resize_type resize) :
            vector()
        {
            m_resize = std::move(resize);
        }

        // I know you can figure this one out
        vector(const size_type size, resize_type resize) :
            vector(size)
        {
            m_resize = std::move(resize);
        }

        // Overloaded constructor
        // that takes an std::initializer_list of values and initializes the vector with those values.
        vector(std::initializer_list<value_type> list) :
            vector(list.size())
        {
            for (const value_type item : list)
            {
                add(item);
            }
        }

        // I know you can figure this one out
        vector(std::initializer_list<value_type> list, resize_type resize) :
            vector(list.size())
        {
            for (const value_type item : list)
            {
                add(item);
            }
            m_resize = std::move(resize);
        }

        // Returns a reference to the value at the position index.
        // If an invalid index is specified throw a std::range_error exception.
        reference operator[](size_type index)
        {
            if (index >= m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            return m_data[index];
        }

        // Adds a new value at the end of the vector
        void add(value_type value)
        {
            ensure_capacity();
            m_data[m_size] = value;
            m_size += 1;
        }

        // Inserts a value before the position index in the vector.
        // If an invalid position is specified, throw a std::range_error exception.
        void insert(const size_type index, const value_type value)
        {
            if (index >= m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            m_size += 1;
            ensure_capacity();
            for (size_type i = index; i < m_size; i++)
            {
                m_data[i + 1] = m_data[i];
            }
            m_data[index] = value;
        }

        //  Removes the item at position index from the vector.
        //  If an invalid position is specified, throw a std::range_error exception.
        void remove(const size_type index)
        {
            if (index >= m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            for (size_type i = index; i < m_size; i++)
            {
                m_data[i] = m_data[i + 1];
            }
            m_size -= 1;
        }

        // thought it would be fun to make this pop method
        value_type pop()
        {
            value_type value = m_data[m_size - 1];
            remove(m_size - 1);
            return value;
        }

        // Removes all items in the vector (just sets the size to 0), does not change the capacity
        void clear()
        {
            m_size = 0;
        }

        // Returns the number of items in the vector
        [[nodiscard]] size_type size() const
        {
            return m_size;
        }

        // Returns the total capacity in the vector
        [[nodiscard]] size_type capacity() const
        {
            return m_capacity;
        }

        //  Applies the function to all items in the vector
        void map(std::function<void(reference)> func)
        {
            for (size_type i = 0; i < m_size; ++i)
            {
                func(m_data[i]);
            }
        }

        // Got from professors class notes
        class iterator
        {
          public:
            using iterator_category = std::forward_iterator_tag;

            iterator() :
                iterator(nullptr) // DefaultConstructable
            {
            }

            explicit iterator(pointer ptr) :
                m_pos(0),
                m_data(ptr)
            {
            }

            iterator(const size_type pos, pointer ptr) :
                m_pos(pos),
                m_data(ptr)
            {
            }

            iterator(const iterator& obj) // CopyConstructable
            {
                this->m_pos = obj.m_pos;
                this->m_data = obj.m_data;
            }

            iterator(iterator&& obj) noexcept // MoveConstructable
            {
                this->m_pos = obj.m_pos;
                this->m_data = obj.m_data;
                obj.m_pos = 0;
                obj.m_data = nullptr;
            }

            iterator operator++() // incrementable e.g., ++r
            {
                m_pos++;
                return *this;
            }

            iterator operator++(int) // incrementable e.g., r++
            {
                iterator i = *this;
                m_pos++;
                return i;
            }

            iterator operator--() // incrementable e.g., --r
            {
                m_pos--;
                return *this;
            }

            iterator operator--(int) // incrementable e.g., r--
            {
                iterator i = *this;
                m_pos--;
                return i;
            }

            iterator& operator=(const iterator& rhs) // CopyAssignable
            {
                this->m_pos = rhs.m_pos;
                this->m_data = rhs.m_data;
                return *this;
            }

            iterator& operator=(iterator&& rhs) noexcept // MoveAssignable
            {
                if (this != &rhs)
                {
                    std::swap(this->m_pos, rhs.m_pos);
                    std::swap(this->m_data, rhs.m_data);
                }
                return *this;
            }

            T* operator->()
            {
                return &m_data[m_pos];
            }

            reference operator*() // Derefrenceable
            {
                return m_data[m_pos];
            }

            bool operator==(const iterator& rhs)
            {
                return m_pos == rhs.m_pos;
            }

            bool operator!=(const iterator& rhs)
            {
                return m_pos != rhs.m_pos;
            }

          private:
            size_type m_pos;
            pointer m_data;
        };

        // Returns an iterator to the first item in the vector (or .end() if nothing in the vector)
        iterator begin()
        {
            return iterator(m_data);
        }

        // Returns an iterator to the "end" of the vector
        iterator end()
        {
            return iterator(m_size, m_data);
        }

      private:
        size_type m_size{ 0 };
        size_type m_capacity{ InitialCapacity };
        pointer m_data{ std::make_shared<value_type[]>(m_capacity) };
        resize_type m_resize = [](const size_type capacity) -> size_type
        {
            return capacity * 2;
        };

        void ensure_capacity()
        {
            while (m_size >= m_capacity)
            {
                m_capacity = m_resize(m_capacity);
                pointer new_data = std::make_shared<value_type[]>(m_capacity);
                for (size_type i = 0; i < m_size; i++)
                {
                    new_data[i] = m_data[i];
                }
                m_data = new_data;
            }
        }
    };
} // namespace usu
