#pragma once

#include <utility>

namespace usu
{
    template <typename T>
    class unique_ptr
    {
        
    };
    
    template <typename T, typename... Args>
    unique_ptr<T> make_unique(Args&&... args)
    {
        return unique_ptr<T>(new T(std::forward<Args>(args)...));
    }
}


