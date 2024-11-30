#pragma once

template <typename T>
class MyClass
{
  public:
    MyClass(T data) :
        m_data(data)
    {
    }

    T getData() { return m_data; }

  private:
    T m_data;
};

template <typename T>
class MyOtherClass
{
  public:
    MyOtherClass(T data);

    T getData();

  private:
    T m_data;
};

template <typename T>
MyOtherClass<T>::MyOtherClass(T data) :
    m_data(data)
{
}

template <typename T>
T MyOtherClass<T>::getData()
{
    return m_data;
}
