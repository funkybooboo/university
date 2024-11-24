#pragma once

#include <exception>
#include <functional>
#include <initializer_list>
#include <memory>

namespace usu
{
    /**
     * @brief A custom implementation of a dynamic array (vector) with automatic resizing.
     *
     * This class implements a simple vector-like container that supports dynamic resizing. It provides
     * operations such as adding, inserting, removing elements, and accessing elements by index.
     * The class also supports iterators, which can be used to traverse the vector.
     *
     * @tparam T The type of elements stored in the vector.
     * @tparam InitialCapacity The initial capacity of the vector (default is 10).
     */
    template <typename T, size_t InitialCapacity = 10>
    class vector
    {
      public:
        using size_type = std::size_t;                           ///< Type alias for size type (typically `std::size_t`).
        using value_type = T;                                    ///< Type alias for the element type.
        using reference = value_type&;                           ///< Type alias for reference to an element.
        using pointer = std::shared_ptr<value_type[]>;           ///< Type alias for shared pointer to the array.
        using resize_type = std::function<size_type(size_type)>; ///< Function type for resizing logic.

        /**
         * @brief Iterator class for traversing elements in the vector.
         *
         * This class allows for iterating over elements in the vector, providing
         * typical iterator functionality such as increment, comparison, and dereferencing.
         */
        class iterator
        {
          public:
            using iterator_category = std::forward_iterator_tag; ///< Iterator category (forward iterator).

            /**
             * @brief Default constructor for the iterator.
             */
            iterator() noexcept;

            /**
             * @brief Constructs an iterator from a pointer.
             * @param ptr A pointer to the first element in the vector.
             */
            explicit iterator(pointer ptr) noexcept;

            /**
             * @brief Constructs an iterator with a specific position.
             * @param pos The position (index) of the element.
             * @param ptr A pointer to the start of the vector.
             */
            iterator(size_type pos, pointer ptr) noexcept;

            /**
             * @brief Copy constructor.
             * @param obj The iterator to copy from.
             */
            iterator(const iterator& obj) noexcept;

            /**
             * @brief Move constructor.
             * @param obj The iterator to move from.
             */
            iterator(iterator&& obj) noexcept;

            /**
             * @brief Pre-increment operator (move to the next element).
             * @return A reference to the incremented iterator.
             */
            iterator& operator++() noexcept;

            /**
             * @brief Post-increment operator (move to the next element).
             * @return A copy of the iterator before incrementing.
             */
            iterator operator++(int) noexcept;

            /**
             * @brief Pre-decrement operator (move to the previous element).
             * @return A reference to the decremented iterator.
             */
            iterator& operator--() noexcept;

            /**
             * @brief Post-decrement operator (move to the previous element).
             * @return A copy of the iterator before decrementing.
             */
            iterator operator--(int) noexcept;

            /**
             * @brief Copy assignment operator.
             * @param rhs The iterator to copy from.
             * @return A reference to this iterator.
             */
            iterator& operator=(const iterator& rhs) noexcept;

            /**
             * @brief Move assignment operator.
             * @param rhs The iterator to move from.
             * @return A reference to this iterator.
             */
            iterator& operator=(iterator&& rhs) noexcept;

            /**
             * @brief Arrow operator for dereferencing the iterator.
             * @return A pointer to the element.
             */
            T* operator->() noexcept;

            /**
             * @brief Dereference operator for accessing the element.
             * @return A reference to the element.
             */
            reference operator*() noexcept;

            /**
             * @brief Equality comparison operator.
             * @param rhs The iterator to compare with.
             * @return True if both iterators are pointing to the same element.
             */
            bool operator==(const iterator& rhs) const noexcept;

            /**
             * @brief Inequality comparison operator.
             * @param rhs The iterator to compare with.
             * @return True if both iterators are pointing to different elements.
             */
            bool operator!=(const iterator& rhs) const noexcept;

          private:
            size_type m_pos; ///< Current position (index) of the iterator.
            pointer m_data;  ///< Pointer to the vector's data.
        };

        /**
         * @brief Default constructor that initializes an empty vector.
         */
        vector() noexcept;

        /**
         * @brief Constructs a vector with a specific size.
         * @param size The initial size of the vector.
         */
        explicit vector(size_type size) noexcept;

        /**
         * @brief Constructs a vector with a custom resizing function.
         * @param resize The function that defines how the vector should resize.
         */
        explicit vector(resize_type resize) noexcept;

        /**
         * @brief Constructs a vector with a specific size and a custom resizing function.
         * @param size The initial size of the vector.
         * @param resize The function that defines how the vector should resize.
         */
        vector(size_type size, resize_type resize) noexcept;

        /**
         * @brief Constructs a vector from an initializer list.
         * @param list The initializer list containing elements to initialize the vector with.
         */
        vector(std::initializer_list<value_type> list) noexcept;

        /**
         * @brief Constructs a vector from an initializer list and a custom resizing function.
         * @param list The initializer list containing elements to initialize the vector with.
         * @param resize The function that defines how the vector should resize.
         */
        vector(std::initializer_list<value_type> list, resize_type resize) noexcept;

        /**
         * @brief Accesses an element at a given index.
         * @param index The index of the element to access.
         * @return A reference to the element at the specified index.
         * @throws std::range_error if the index is out of bounds.
         */
        [[nodiscard]] reference operator[](size_type index) noexcept(false);

        /**
         * @brief Adds a new value to the end of the vector.
         * @param value The value to add.
         */
        void add(value_type value) noexcept;

        /**
         * @brief Inserts a value at a specific position in the vector.
         * @param index The position to insert the value.
         * @param value The value to insert.
         * @throws std::range_error if the index is out of bounds.
         */
        void insert(size_type index, value_type value) noexcept(false);

        /**
         * @brief Removes the element at a specific position.
         * @param index The position of the element to remove.
         * @throws std::range_error if the index is out of bounds.
         */
        void remove(size_type index) noexcept(false);

        /**
         * @brief Pops the last element from the vector.
         * @return The popped value.
         */
        [[nodiscard]] value_type pop() noexcept;

        /**
         * @brief Clears the vector, removing all elements.
         */
        void clear() noexcept;

        /**
         * @brief Returns the number of elements in the vector.
         * @return The size of the vector.
         */
        [[nodiscard]] size_type size() const noexcept;

        /**
         * @brief Returns the total capacity of the vector (the maximum number of elements it can hold without resizing).
         * @return The capacity of the vector.
         */
        [[nodiscard]] size_type capacity() const noexcept;

        /**
         * @brief Applies a function to each element in the vector.
         * @param func The function to apply to each element.
         */
        void map(std::function<void(reference)> func) noexcept;

        /**
         * @brief Returns an iterator to the first element in the vector.
         * @return An iterator pointing to the first element.
         */
        [[nodiscard]] iterator begin() noexcept;

        /**
         * @brief Returns an iterator to one past the last element in the vector.
         * @return An iterator pointing past the last element.
         */
        [[nodiscard]] iterator end() noexcept;

      private:
        size_type m_size{ 0 };                                        ///< The current size of the vector.
        size_type m_capacity{ InitialCapacity };                      ///< The current capacity of the vector.
        pointer m_data{ std::make_shared<value_type[]>(m_capacity) }; ///< The pointer to the vector's data.
        resize_type m_resize = [](const size_type capacity) noexcept -> size_type
        {
            return capacity * 2; ///< Default resizing strategy (double the capacity).
        };

        /**
         * @brief Ensures that the vector has enough capacity for additional elements.
         */
        void ensure_capacity() noexcept;
    };

    // --- iterator methods ---

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::iterator::iterator() noexcept :
        m_pos(0), m_data(nullptr)
    {
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::iterator::iterator(pointer ptr) noexcept :
        m_pos(0), m_data(ptr)
    {
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::iterator::iterator(const size_type pos, pointer ptr) noexcept :
        m_pos(pos), m_data(ptr)
    {
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::iterator::iterator(const iterator& obj) noexcept 
    {
        m_pos = obj.m_pos;
        m_data = obj.m_data;
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::iterator::iterator(iterator&& obj) noexcept 
    {
        m_pos = obj.m_pos;
        m_data = obj.m_data;
        obj.m_pos = 0;
        obj.m_data = nullptr;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator& vector<T, InitialCapacity>::iterator::operator++() noexcept 
    {
        ++m_pos;
        return *this;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator vector<T, InitialCapacity>::iterator::operator++(int) noexcept 
    {
        iterator tmp = *this;
        ++m_pos;
        return tmp;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator& vector<T, InitialCapacity>::iterator::operator--() noexcept 
    {
        --m_pos;
        return *this;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator vector<T, InitialCapacity>::iterator::operator--(int) noexcept 
    {
        iterator tmp = *this;
        --m_pos;
        return tmp;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator& vector<T, InitialCapacity>::iterator::operator=(const iterator& rhs) noexcept
    {
        if (this != &rhs)
        {
            m_pos = rhs.m_pos;
            m_data = rhs.m_data;
        }
        return *this;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator& vector<T, InitialCapacity>::iterator::operator=(iterator&& rhs) noexcept
    {
        if (this != &rhs)
        {
            std::swap(m_pos, rhs.m_pos);
            std::swap(m_data, rhs.m_data);
        }
        return *this;
    }

    template <typename T, size_t InitialCapacity>
    T* vector<T, InitialCapacity>::iterator::operator->() noexcept
    {
        return &m_data[m_pos];
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::reference vector<T, InitialCapacity>::iterator::operator*() noexcept
    {
        return m_data[m_pos];
    }

    template <typename T, size_t InitialCapacity>
    bool vector<T, InitialCapacity>::iterator::operator==(const iterator& rhs) const noexcept
    {
        return m_pos == rhs.m_pos && m_data == rhs.m_data;
    }

    template <typename T, size_t InitialCapacity>
    bool vector<T, InitialCapacity>::iterator::operator!=(const iterator& rhs) const noexcept
    {
        return !(*this == rhs);
    }

    // --- end iterator methods ---

    // --- vector methods ---

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector() noexcept
    {
        vector(0);
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector(const size_type size) noexcept
    {
        m_size = size;
        m_capacity = std::max(InitialCapacity, m_resize(m_size));
        m_data = std::make_shared<T[]>(m_capacity);
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector(resize_type resize) noexcept :
        vector()
    {
        m_resize = std::move(resize);
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector(const size_type size, resize_type resize) noexcept :
        vector(size)
    {
        m_resize = std::move(resize);
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector(std::initializer_list<value_type> list) noexcept :
        vector()
    {
        for (const value_type& item : list)
        {
            add(item);
        }
    }

    template <typename T, size_t InitialCapacity>
    vector<T, InitialCapacity>::vector(std::initializer_list<value_type> list, resize_type resize) noexcept :
        vector()
    {
        for (const value_type item : list)
        {
            add(item);
        }
        m_resize = std::move(resize);
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::reference vector<T, InitialCapacity>::operator[](size_type index) noexcept(false)
    {
        if (index >= m_size)
        {
            throw std::range_error("Index out of bounds");
        }
        return m_data[index];
    }

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::add(value_type value) noexcept
    {
        ensure_capacity();
        m_data[m_size] = value;
        m_size += 1;
    }

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::insert(const size_type index, const value_type value) noexcept(false)
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

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::remove(const size_type index) noexcept(false)
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

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::value_type vector<T, InitialCapacity>::pop() noexcept
    {
        value_type value = m_data[m_size - 1];
        remove(m_size - 1);
        return value;
    }

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::clear() noexcept
    {
        m_size = 0;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::size_type vector<T, InitialCapacity>::size() const noexcept
    {
        return m_size;
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::size_type vector<T, InitialCapacity>::capacity() const noexcept
    {
        return m_capacity;
    }

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::map(std::function<void(reference)> func) noexcept
    {
        for (size_type i = 0; i < m_size; ++i)
        {
            func(m_data[i]);
        }
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator vector<T, InitialCapacity>::begin() noexcept
    {
        return iterator(m_data);
    }

    template <typename T, size_t InitialCapacity>
    typename vector<T, InitialCapacity>::iterator vector<T, InitialCapacity>::end() noexcept
    {
        return iterator(m_size, m_data);
    }

    template <typename T, size_t InitialCapacity>
    void vector<T, InitialCapacity>::ensure_capacity() noexcept
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

    // --- end vector methods ---

} // namespace usu
