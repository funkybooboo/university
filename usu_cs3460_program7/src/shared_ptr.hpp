#pragma once

#include <cstdint>
#include <utility>

namespace usu
{
    /**
     * A reference-counted smart pointer for managing a single dynamically allocated object.
     *
     * @tparam T The type of the object being managed.
     */
    template <typename T>
    class shared_ptr
    {
      public:
        /**
         * Constructs a shared pointer that manages the given raw pointer to an object.
         *
         * @param data A raw pointer to the object to be managed.
         */
        explicit shared_ptr(T* data) noexcept
            :
            m_data(data), m_count(new std::uint32_t(1)) {}

        /**
         * Copy constructor that increments the reference count.
         *
         * @param other The shared pointer to copy from.
         */
        shared_ptr(const shared_ptr& other) noexcept
            :
            m_data(other.m_data), m_count(other.m_count)
        {
            if (m_count)
            {
                ++(*m_count);
            }
        }

        /**
         * Move constructor that transfers ownership of the object and reference count.
         *
         * @param other The shared pointer to move from.
         */
        shared_ptr(shared_ptr&& other) noexcept
            :
            m_data(other.m_data), m_count(other.m_count)
        {
            other.m_data = nullptr;
            other.m_count = nullptr;
        }

        /**
         * Destructor that decrements the reference count and deletes the object when the count reaches zero.
         */
        ~shared_ptr() noexcept
        {
            if (m_count && --(*m_count) == 0)
            {
                delete m_data;
                delete m_count;
            }
        }

        /**
         * Copy assignment operator that manages reference counting properly.
         *
         * @param other The shared pointer to copy from.
         * @return A reference to this shared pointer.
         */
        shared_ptr& operator=(const shared_ptr& other) noexcept
        {
            if (this != &other)
            {
                if (m_count && --(*m_count) == 0)
                {
                    delete m_data;
                    delete m_count;
                }

                m_data = other.m_data;
                m_count = other.m_count;
                if (m_count)
                {
                    ++(*m_count);
                }
            }
            return *this;
        }

        /**
         * Move assignment operator that transfers ownership of the object and reference count.
         *
         * @param other The shared pointer to move from.
         * @return A reference to this shared pointer.
         */
        shared_ptr& operator=(shared_ptr&& other) noexcept
        {
            if (this != &other)
            {
                if (m_count && --(*m_count) == 0)
                {
                    delete m_data;
                    delete m_count;
                }

                m_data = other.m_data;
                m_count = other.m_count;
                other.m_data = nullptr;
                other.m_count = nullptr;
            }
            return *this;
        }

        /**
         * Arrow operator to access members of the managed object.
         *
         * @return A pointer to the managed object.
         */
        T* operator->() const noexcept
        {
            return m_data;
        }

        /**
         * Dereference operator to access the managed object itself.
         *
         * @return A reference to the managed object.
         */
        T& operator*() const noexcept
        {
            return *m_data;
        }

        /**
         * Returns the raw pointer to the managed object.
         *
         * @return A raw pointer to the managed object.
         */
        [[nodiscard]] T* get() const noexcept
        {
            return m_data;
        }

        /**
         * Returns the current reference-count for the managed object.
         *
         * @return The reference count.
         */
        [[nodiscard]] std::uint32_t use_count() const noexcept
        {
            return m_count ? *m_count : 0;
        }

      private:
        T* m_data = nullptr;              ///< Pointer to the managed object.
        std::uint32_t* m_count = nullptr; ///< Reference count.
    };

    /**
     * A specialized version of shared_ptr for managing arrays.
     *
     * @tparam T The type of the array elements.
     */
    template <typename T>
    class shared_ptr<T[]>
    {
      public:
        /**
         * Constructor that accepts a raw pointer to an array and its size.
         *
         * @param data A raw pointer to the array to be managed.
         * @param size The size of the array.
         */
        explicit shared_ptr(T* data, const std::size_t size) noexcept
            :
            m_data(data), m_size(size), m_count(new std::uint32_t(1)) {}

        /**
         * Copy constructor that increments the reference count for the array.
         *
         * @param other The shared pointer to copy from.
         */
        shared_ptr(const shared_ptr& other) noexcept
            :
            m_data(other.m_data), m_size(other.m_size), m_count(other.m_count)
        {
            if (m_count)
            {
                ++(*m_count);
            }
        }

        /**
         * Move constructor that transfers ownership of the array and reference count.
         *
         * @param other The shared pointer to move from.
         */
        shared_ptr(shared_ptr&& other) noexcept
            :
            m_data(other.m_data), m_size(other.m_size), m_count(other.m_count)
        {
            other.m_data = nullptr;
            other.m_size = 0;
            other.m_count = nullptr;
        }

        /**
         * Destructor that decrements the reference count and deletes the array when the count reaches zero.
         */
        ~shared_ptr() noexcept
        {
            if (m_count && --(*m_count) == 0)
            {
                delete[] m_data;
                delete m_count;
            }
        }

        /**
         * Copy assignment operator that properly manages reference counting for the array.
         *
         * @param other The shared pointer to copy from.
         * @return A reference to this shared pointer.
         */
        shared_ptr& operator=(const shared_ptr& other) noexcept
        {
            if (this != &other)
            {
                if (m_count && --(*m_count) == 0)
                {
                    delete[] m_data;
                    delete m_count;
                }

                m_data = other.m_data;
                m_size = other.m_size;
                m_count = other.m_count;
                if (m_count)
                {
                    ++(*m_count);
                }
            }
            return *this;
        }

        /**
         * Move assignment operator that transfers ownership of the array and reference count.
         *
         * @param other The shared pointer to move from.
         * @return A reference to this shared pointer.
         */
        shared_ptr& operator=(shared_ptr&& other) noexcept
        {
            if (this != &other)
            {
                if (m_count && --(*m_count) == 0)
                {
                    delete[] m_data;
                    delete m_count;
                }

                m_data = other.m_data;
                m_size = other.m_size;
                m_count = other.m_count;
                other.m_data = nullptr;
                other.m_size = 0;
                other.m_count = nullptr;
            }
            return *this;
        }

        /**
         * Array access operator to access elements of the array.
         *
         * @param index The index of the element.
         * @return A reference to the element at the specified index.
         */
        T& operator[](std::size_t index) const noexcept
        {
            return m_data[index];
        }

        /**
         * Returns the size of the array.
         *
         * @return The number of elements in the array.
         */
        [[nodiscard]] std::size_t size() const noexcept
        {
            return m_size;
        }

      private:
        T* m_data;              ///< Pointer to the managed array.
        std::size_t m_size;     ///< The number of elements in the array.
        std::uint32_t* m_count; ///< Reference count.
    };

    /**
     * Factory function to create a shared_ptr for managing a single object.
     *
     * @tparam T The type of the object to create.
     * @tparam Args Variadic template for the constructor arguments of the object.
     * @param args Arguments to be passed to the constructor of the object.
     * @return A shared_ptr managing the newly created object.
     */
    template <typename T, typename... Args>
    shared_ptr<T> make_shared(Args&&... args)
    {
        return shared_ptr<T>(new T(std::forward<Args>(args)...));
    }

    /**
     * Factory function to create a shared_ptr for managing an array of objects.
     *
     * @tparam T The type of the array elements.
     * @tparam N The number of elements in the array.
     * @return A shared_ptr managing the newly created array.
     */
    template <typename T, unsigned int N>
    shared_ptr<T[]> make_shared_array()
    {
        return shared_ptr<T[]>(new T[N], N);
    }
} // namespace usu
