#pragma once
#include <cstdint>
#include <exception>
#include <memory>

namespace usu
{
    template <typename T>
    class stack
    {
      public:
        stack(std::uint16_t size);

        bool push(T value);
        T pop();

        [[nodiscard]] bool isFull() noexcept { return m_top == m_size; }
        [[nodiscard]] bool isEmpty() noexcept { return m_top == 0; }

      private:
        std::uint16_t m_size;
        std::uint16_t m_top;
        std::unique_ptr<T[]> m_data;
    };

    template <typename T>
    stack<T>::stack(std::uint16_t size) :
        m_size(size),
        m_top(0),
        m_data(std::make_unique<T[]>(size))
    {
    }

    template <typename T>
    bool stack<T>::push(T value)
    {
        if (isFull())
        {
            return false;
        }

        m_data[m_top++] = value;

        return true;
    }

    template <typename T>
    T stack<T>::pop()
    {
        if (isEmpty())
        {
            throw std::exception("Hey, the stack is empty!");
        }

        return m_data[--m_top];
    }

} // namespace usu
