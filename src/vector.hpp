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
        vector() noexcept
        {
            vector(0);
        }

        // Overloaded constructor that takes a size_type and creates a vector of that size
        explicit vector(const size_type size) noexcept
        {
            m_size = size;
            m_capacity = std::max(InitialCapacity, m_resize(m_size));
            m_data = std::make_shared<T[]>(m_capacity);
        }

        // Overloaded constructor that takes a resize_type
        explicit vector(resize_type resize) noexcept :
            vector()
        {
            m_resize = std::move(resize);
        }

        // Overloaded constructor that takes size and resize_type
        vector(const size_type size, resize_type resize) noexcept :
            vector(size)
        {
            m_resize = std::move(resize);
        }

        // Overloaded constructor that takes an std::initializer_list
        vector(std::initializer_list<value_type> list) noexcept :
            vector()
        {
            for (const value_type item : list)
            {
                add(item);
            }
        }

        // Overloaded constructor with initializer_list and resize_type
        vector(std::initializer_list<value_type> list, resize_type resize) noexcept :
            vector()
        {
            for (const value_type item : list)
            {
                add(item);
            }
            m_resize = std::move(resize);
        }

        // Returns a reference to the value at the position index.
        // If an invalid index is specified throw a std::range_error exception.
        [[nodiscard]] reference operator[](size_type index) noexcept(false)
        {
            if (index >= m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            return m_data[index];
        }

        // Adds a new value at the end of the vector
        void add(value_type value) noexcept
        {
            ensure_capacity();
            m_data[m_size] = value;
            m_size += 1;
        }

        // Inserts a value before the position index in the vector.
        // If an invalid position is specified, throw a std::range_error exception.
        void insert(const size_type index, const value_type value) noexcept(false)
        {
            if (index > m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            ensure_capacity();
            m_size += 1;
            for (size_type i = m_size - 1; i > index; --i)
            {
                m_data[i] = m_data[i - 1];
            }
            m_data[index] = value;
        }

        // Removes the item at position index from the vector.
        // If an invalid position is specified, throw a std::range_error exception.
        void remove(const size_type index) noexcept(false)
        {
            if (index >= m_size)
            {
                throw std::range_error("Index out of bounds");
            }
            for (size_type i = index; i < m_size - 1; i++)
            {
                m_data[i] = m_data[i + 1];
            }
            m_size -= 1;
        }

        // Pops the last element and returns it
        [[nodiscard]] value_type pop() noexcept
        {
            value_type value = m_data[m_size - 1];
            remove(m_size - 1);
            return value;
        }

        // Removes all items in the vector (just sets the size to 0), does not change the capacity
        void clear() noexcept
        {
            m_size = 0;
        }

        // Returns the number of items in the vector
        [[nodiscard]] size_type size() const noexcept
        {
            return m_size;
        }

        // Returns the total capacity in the vector
        [[nodiscard]] size_type capacity() const noexcept
        {
            return m_capacity;
        }

        // Applies the function to all items in the vector
        void map(std::function<void(reference)> func) noexcept
        {
            for (size_type i = 0; i < m_size; ++i)
            {
                func(m_data[i]);
            }
        }

        // Iterator class
        class iterator
        {
          public:
            using iterator_category = std::forward_iterator_tag;

            iterator() noexcept :
                iterator(nullptr) // DefaultConstructable
            {
            }

            explicit iterator(pointer ptr) noexcept :
                m_pos(0),
                m_data(ptr)
            {
            }

            iterator(const size_type pos, pointer ptr) noexcept :
                m_pos(pos),
                m_data(ptr)
            {
            }

            iterator(const iterator& obj) noexcept // CopyConstructable
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

            iterator& operator++() noexcept // incrementable e.g., ++r
            {
                m_pos++;
                return *this;
            }

            iterator operator++(int) noexcept // incrementable e.g., r++
            {
                iterator i = *this;
                m_pos++;
                return i;
            }

            iterator& operator--() noexcept // decrementable e.g., --r
            {
                m_pos--;
                return *this;
            }

            iterator operator--(int) noexcept // decrementable e.g., r--
            {
                iterator i = *this;
                m_pos--;
                return i;
            }

            iterator& operator=(const iterator& rhs) noexcept // CopyAssignable
            = default;

            iterator& operator=(iterator&& rhs) noexcept // MoveAssignable
            {
                if (this != &rhs)
                {
                    std::swap(this->m_pos, rhs.m_pos);
                    std::swap(this->m_data, rhs.m_data);
                }
                return *this;
            }

            T* operator->() noexcept
            {
                return &m_data[m_pos];
            }

            reference operator*() noexcept // Dereferenceable
            {
                return m_data[m_pos];
            }

            bool operator==(const iterator& rhs) const noexcept
            {
                return m_pos == rhs.m_pos && m_data == rhs.m_data;
            }

            bool operator!=(const iterator& rhs) const noexcept
            {
                return !(*this == rhs);
            }

          private:
            size_type m_pos;
            pointer m_data;
        };

        // Returns an iterator to the first item in the vector (or .end() if nothing in the vector)
        [[nodiscard]] iterator begin() noexcept
        {
            return iterator(m_data);
        }

        // Returns an iterator to the "end" of the vector
        [[nodiscard]] iterator end() noexcept
        {
            return iterator(m_size, m_data);
        }

      private:
        size_type m_size{ 0 };
        size_type m_capacity{ InitialCapacity };
        pointer m_data{ std::make_shared<value_type[]>(m_capacity) };
        resize_type m_resize = [](const size_type capacity) noexcept -> size_type
        {
            return capacity * 2;
        };

        // Ensures that the vector has enough capacity for new elements
        void ensure_capacity() noexcept
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
