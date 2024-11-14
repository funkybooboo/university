#pragma once

#include <utility>
#include <cstdint>
#include <atomic>

namespace usu
{
    template<typename T>
    class shared_ptr<T> {
    public:
        explicit shared_ptr(T* data) noexcept
            : m_data(data), m_count(new std::atomic<std::uint32_t>(1)) {
        }

        // Copy constructor
        shared_ptr(const shared_ptr& other) noexcept
            : m_data(other.m_data), m_count(other.m_count) {
            m_count->fetch_add(1, std::memory_order_relaxed);  // Increment reference count
        }

        // Move constructor
        shared_ptr(shared_ptr&& other) noexcept
            : m_data(other.m_data), m_count(other.m_count) {
            other.m_data = nullptr;
            other.m_count = nullptr;
        }

        // Destructor
        ~shared_ptr() noexcept {
            if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                delete m_data;
                delete m_count;
            }
        }

        // Copy assignment operator
        shared_ptr& operator=(const shared_ptr& other) noexcept {
            if (this != &other) {
                // Decrease reference count of current object
                if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                    delete m_data;
                    delete m_count;
                }
                // Copy data and update reference count
                m_data = other.m_data;
                m_count = other.m_count;
                m_count->fetch_add(1, std::memory_order_relaxed);
            }
            return *this;
        }

        // Move assignment operator
        shared_ptr& operator=(shared_ptr&& other) noexcept {
            if (this != &other) {
                // Decrease reference count of current object
                if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                    delete m_data;
                    delete m_count;
                }
                // Transfer ownership
                m_data = other.m_data;
                m_count = other.m_count;
                other.m_data = nullptr;
                other.m_count = nullptr;
            }
            return *this;
        }

        // Arrow operator (access members of the managed object)
        T* operator->() const noexcept {
            return m_data;
        }

        // Dereference operator (access the managed object itself)
        T& operator*() const noexcept {
            return *m_data;
        }

        // Get the raw pointer to the managed object
        [[nodiscard]] T* get() const noexcept {
            return m_data;
        }

        // Return the current reference count
        [[nodiscard]] std::uint32_t use_count() const noexcept {
            return m_count ? m_count->load(std::memory_order_relaxed) : 0;
        }

    private:
        T* m_data;
        std::atomic<std::uint32_t>* m_count;  // Atomic reference count
    };

    template<typename T>
    class shared_ptr<T[]> {
    public:
        // Constructor that accepts a raw pointer to the array and its size
        explicit shared_ptr(T* data, const std::size_t size) noexcept
            : m_data(data), m_size(size), m_count(new std::atomic<std::uint32_t>(1)) {
        }

        // Copy constructor
        shared_ptr(const shared_ptr& other) noexcept
            : m_data(other.m_data), m_size(other.m_size), m_count(other.m_count) {
            m_count->fetch_add(1, std::memory_order_relaxed);  // Increment reference count
        }

        // Move constructor
        shared_ptr(shared_ptr&& other) noexcept
            : m_data(other.m_data), m_size(other.m_size), m_count(other.m_count) {
            other.m_data = nullptr;
            other.m_size = 0;
            other.m_count = nullptr;
        }

        // Destructor
        ~shared_ptr() noexcept {
            if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                delete[] m_data;
                delete m_count;
            }
        }

        // Copy assignment operator
        shared_ptr& operator=(const shared_ptr& other) noexcept {
            if (this != &other) {
                // Decrease reference count of the current object
                if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                    delete[] m_data;
                    delete m_count;
                }
                // Copy data and update reference count
                m_data = other.m_data;
                m_size = other.m_size;
                m_count = other.m_count;
                m_count->fetch_add(1, std::memory_order_relaxed);
            }
            return *this;
        }

        // Move assignment operator
        shared_ptr& operator=(shared_ptr&& other) noexcept {
            if (this != &other) {
                // Decrease reference count of the current object
                if (m_count && m_count->fetch_sub(1, std::memory_order_release) == 1) {
                    delete[] m_data;
                    delete m_count;
                }
                // Transfer ownership
                m_data = other.m_data;
                m_size = other.m_size;
                m_count = other.m_count;
                other.m_data = nullptr;
                other.m_size = 0;
                other.m_count = nullptr;
            }
            return *this;
        }

        // Array access via operator[]
        T& operator[](std::size_t index) const noexcept {
            return m_data[index];
        }

        // Method to return the size of the array
        [[nodiscard]] std::size_t size() const noexcept {
            return m_size;
        }

    private:
        T* m_data;
        std::size_t m_size;  // The number of elements in the array
        std::atomic<std::uint32_t>* m_count;  // Reference count
    };
        
    template <typename T, typename... Args>
    shared_ptr<T> make_shared(Args&&... args)
    {
        return shared_ptr<T>(new T(std::forward<Args>(args)...));
    }

    template <typename T, unsigned int N>
    shared_ptr<T[]> make_shared_array()
    {
        return shared_ptr<T[]>(new T[N], N);
    }

}

