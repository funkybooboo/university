#pragma once

#include <utility>

namespace usu
{
    /**
     * A unique pointer implementation for managing a single dynamically allocated object.
     * The unique_ptr is a smart pointer that exclusively owns the object it points to.
     *
     * @tparam T The type of the object managed by the unique_ptr.
     */
    template <typename T>
    class unique_ptr
    {
      public:
        /**
         * Default constructor that initializes the unique_ptr to nullptr.
         */
        unique_ptr() noexcept :
            m_data(nullptr) {}

        /**
         * Constructor that takes a raw pointer to manage.
         *
         * @param data A raw pointer to the object to be managed.
         */
        explicit unique_ptr(T* data) noexcept :
            m_data(data) {}

        /**
         * Move constructor that transfers ownership of the managed object.
         *
         * @param other The unique_ptr from which ownership is transferred.
         */
        unique_ptr(unique_ptr&& other) noexcept :
            m_data(other.m_data)
        {
            other.m_data = nullptr; // Invalidate the other unique_ptr
        }

        /**
         * Destructor that deletes the managed object when the unique_ptr goes out of scope.
         */
        ~unique_ptr() noexcept
        {
            delete m_data;
        }

        /**
         * Move assignment operator that transfers ownership of the managed object.
         *
         * @param other The unique_ptr from which ownership is transferred.
         * @return A reference to this unique_ptr after the assignment.
         */
        unique_ptr& operator=(unique_ptr&& other) noexcept
        {
            if (this != &other)
            {
                // Delete the existing resource (if any)
                delete m_data;
                // Transfer ownership
                m_data = other.m_data;
                other.m_data = nullptr;
            }
            return *this;
        }

        /**
         * Dereference operator to access the managed object.
         *
         * @return A reference to the managed object.
         */
        T& operator*() const noexcept
        {
            return *m_data;
        }

        /**
         * Arrow operator to access the managed object's members.
         *
         * @return A pointer to the managed object.
         */
        T* operator->() const noexcept
        {
            return m_data;
        }

        /**
         * Equality operator to check if two unique_ptrs are pointing to the same object.
         *
         * @param other The unique_ptr to compare with.
         * @return True if both unique_ptrs manage the same object, false otherwise.
         */
        bool operator==(const unique_ptr& other) const noexcept
        {
            return m_data == other.m_data;
        }

        /**
         * Inequality operator to check if two unique_ptrs are pointing to different objects.
         *
         * @param other The unique_ptr to compare with.
         * @return True if the unique_ptrs manage different objects, false otherwise.
         */
        bool operator!=(const unique_ptr& other) const noexcept
        {
            return m_data != other.m_data;
        }

        /**
         * Returns a raw pointer to the managed object.
         *
         * @return A raw pointer to the managed object.
         */
        [[nodiscard]] T* get() const noexcept
        {
            return m_data;
        }

        /**
         * Releases ownership of the managed object and returns the raw pointer.
         * After this call, the unique_ptr no longer manages the object.
         *
         * @return The raw pointer to the managed object.
         */
        T* release() noexcept
        {
            T* temp = m_data;
            m_data = nullptr;
            return temp;
        }

        // Deleted copy constructor (prevents copying of unique_ptr)
        unique_ptr(const unique_ptr& other) = delete;

        // Deleted copy assignment operator (prevents copying of unique_ptr)
        unique_ptr& operator=(const unique_ptr& other) = delete;

      private:
        T* m_data; ///< Raw pointer to the managed object
    };

    /**
     * A specialized version of unique_ptr for managing dynamically allocated arrays.
     *
     * @tparam T The type of the array elements.
     */
    template <typename T>
    class unique_ptr<T[]>
    {
      public:
        /**
         * Constructor that accepts a raw pointer to an array.
         *
         * @param data A raw pointer to the array to be managed.
         */
        explicit unique_ptr(T* data) noexcept :
            m_data(data) {}

        /**
         * Move constructor that transfers ownership of the managed array.
         *
         * @param other The unique_ptr from which ownership is transferred.
         */
        unique_ptr(unique_ptr&& other) noexcept :
            m_data(other.m_data)
        {
            other.m_data = nullptr;
        }

        /**
         * Destructor that deletes the managed array.
         */
        ~unique_ptr() noexcept
        {
            delete[] m_data;
        }

        /**
         * Move assignment operator that transfers ownership of the managed array.
         *
         * @param other The unique_ptr from which ownership is transferred.
         * @return A reference to this unique_ptr after the assignment.
         */
        unique_ptr& operator=(unique_ptr&& other) noexcept
        {
            if (this != &other)
            {
                delete[] m_data; // Delete the existing array (if any)
                m_data = other.m_data;
                other.m_data = nullptr;
            }
            return *this;
        }

        /**
         * Array access operator to access elements of the managed array.
         *
         * @param index The index of the element to access.
         * @return A reference to the element at the specified index.
         */
        T& operator[](std::size_t index) const noexcept
        {
            return m_data[index];
        }

        /**
         * Returns a raw pointer to the managed array.
         *
         * @return A raw pointer to the managed array.
         */
        [[nodiscard]] T* get() const noexcept
        {
            return m_data;
        }

        /**
         * Releases ownership of the managed array and returns the raw pointer.
         * After this call, the unique_ptr no longer manages the array.
         *
         * @return The raw pointer to the managed array.
         */
        T* release() noexcept
        {
            T* temp = m_data;
            m_data = nullptr;
            return temp;
        }

        // Deleted copy constructor (prevents copying of unique_ptr)
        unique_ptr(const unique_ptr& other) = delete;

        // Deleted copy assignment operator (prevents copying of unique_ptr)
        unique_ptr& operator=(const unique_ptr& other) = delete;

      private:
        T* m_data; ///< Raw pointer to the managed array
    };

    /**
     * Factory function for creating a unique_ptr for managing a single object.
     *
     * @tparam T The type of the object to create.
     * @tparam Args Variadic template for the constructor arguments of the object.
     * @param args Arguments to be forwarded to the constructor of the object.
     * @return A unique_ptr managing the newly created object.
     */
    template <typename T, typename... Args>
    unique_ptr<T> make_unique(Args&&... args)
    {
        return unique_ptr<T>(new T(std::forward<Args>(args)...));
    }

    /**
     * Factory function for creating a unique_ptr for managing an array of objects.
     *
     * @tparam T The type of the array elements.
     * @tparam N The number of elements in the array.
     * @return A unique_ptr managing the newly created array.
     */
    template <typename T, unsigned int N>
    unique_ptr<T[]> make_unique_array()
    {
        return unique_ptr<T[]>(new T[N]);
    }
} // namespace usu
