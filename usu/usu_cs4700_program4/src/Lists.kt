// Do not remove or rename the package
package lists

/*
* The following functions are helper functions that I am providing
*/

/*
* Extend the List class with a "tail" getter to get the tail of a list.
* Below is an example of how you would use tail
*    val a = listOf(1,2,3)
*    val t = a.tail
*    println("tail of $a is $t") // prints [2,3]
*/
val <T> List<T>.tail: List<T>
    get() = drop(1)

/*
* Extend the List class with a "head" getter to get the head of a list.
* Below is an example of how you would use head
*    val a = listOf(1,2,3)
*    val h = a.head
*    println("head of $a is $h") // prints 1
*/
val <T> List<T>.head: T
    get() = first()

/*
* The isPrime function takes as input an Int
*      x : an Int object to test
* and returns a Boolean
*      true  if x is a prime
*      false if x is not a prime
*/
fun isPrime(x : Int) : Boolean {
    if (x < 2) { return false }
    for (i in 2..<x) {
        if (x % i == 0) {
            return false
        }
    }
    return true
}

/* The compose function takes as input
*     f - A function that takes as input a value of type T and returns a value of type T
*     g - A function that takes as input a value of type T and returns a value of type T
*  and returns as output the composition of the functions
*     f(g(x))
*/
fun<T> compose(f: (T)->T,  g:(T) -> T) : (T) -> T = { f(g(it)) }


// Helper code provided by the professor above this line. Below this line is my code.
// Instruction and example input and output came from the professor.


// Instruction
// Write a function to build a bounded list of counting numbers (counting numbers start at 1) up to (and including) n.
// The counting numbers up to n are the list [1, 2, 3, ..., n].
// You may assume n is an Int or a null, that is, the function must ensure null safety.
// Example input and output
//  countingNumbers(3) -> returns the list [1,2,3]
//  countingNumbers(5) -> returns the list [1,2,3,4,5]
//  countingNumbers(0) -> returns the list []
//  countingNumbers(null) -> returns null
fun countingNumbers(limit : Int?) : List<Int>? {
    if (limit == null) return null
    if (limit == 0) return listOf()
    return (1..limit).toList() // make a range object then convert it into a list object then return that list object
}


// Instruction
// Using a lambda expression, i.e., list.filter{}, write a function to build a list of the even counting numbers up to n.
// You may assume n is an Int or a null, that is, the function must ensure null safety.
// Example input and output
//  evenNumbers(3) -> returns the list [2]
//  evenNumbers(5) -> returns the list [2,4]
//  evenNumbers(0) -> returns the list []
//  evenNumbers(null) -> returns null
fun evenNumbers(n: Int?): List<Int>? {
    if (n == null) return null
    if (n == 0) return listOf()
    return countingNumbers(n)?.filter { it % 2 == 0 } // get a list of the counting numbers. if null is given return null else create a new list and filter each element by if its even. Return the resulting list.
}


// Instruction
// Using a lambda expression, i.e., list.filter{}, write a function to build a list of the prime counting numbers up to n.
// A prime number is evenly divisible only by 1 and itself, though 1 is not a prime number.
// You may assume n is an Int or a null, that is, the function must ensure null safety.
// Example input and output
//  primeNumbers(3) -> returns the list [2,3]
//  primeNumbers(5) -> returns the list [2,3,5]
//  primeNumbers(0) -> returns the list []
//  primeNumbers(null) -> returns null
fun primeNumbers(n : Int?)  : List<Int>? {
    if (n == null) return null
    if (n == 0) return listOf()
    return countingNumbers(n)?.filter { isPrime(it) } // get a list of the counting numbers. if null is given return null else create a new list and filter each element by if its prime. Return the resulting list.
}


// Instruction
// Write a function to merge (in sorted order) two lists of comparable types and assuming both lists are already sorted.
// Either list may be empty or null, that is, the function must ensure null safety.
// If a parameter is null return null
// Example input and output
//  merge(listOf(1,2,3),listOf(1,7)) -> returns the list [1,1,2,3,7]
//  merge(listOf(),listOf(1,7)) -> returns the list [1,7]
//  merge(listOf(1,2,3),null) -> returns null
fun<T : Comparable<T>> merge(a : List<T>?, b : List<T>?) : List<T>? {
    if (a == null || b == null) return null
    val r = mutableListOf<T>()
    merge(a, b, r) // call recursive function to populate r
    return r.toList()
}
fun<T : Comparable<T>> merge(a : List<T>, b : List<T>, r : MutableList<T>) {
    if (a.isEmpty()) { // recurse until a or b is empty
        r.addAll(b)
        return
    }
    if (b.isEmpty()) {
        r.addAll(a)
        return
    }
    if (a.head < b.head) { // if a[0] is smaller than b[0]
        r.add(a.head) // add a[0] to r
        merge(a.tail, b, r) // recurse without a[0]
    }
    else {
        r.add(b.head) // add b[0] to r
        merge(a, b.tail, r) // recurse without b[0]
    }
}


// Instruction
// Write a function to build a list of sub-lists, where the nth sub-list is the elements from 1 to n in the list.
// The input list may be empty or null, that is, the function must ensure null safety. If the parameter is null return null
// Example input and output
//  subLists(listOf(1,2,3)) -> returns the list [[1],[1,2],[1,2,3]]
//  subLists(listOf(3,0)) -> returns the list [[3],[3,0]]
//  subLists(listOf()) -> returns the list []
//  subLists(null) -> returns null
fun subLists(a : List<Int>?) : List<List<Int>>? {
    if (a == null) return null
    if (a.isEmpty()) return listOf()
    val r = mutableListOf<List<Int>>()
    for (i in a.size downTo 1) { // keep getting smaller
        r.add(a.subList(0, i)) // get the sublist from the start to the end
    }
    return r.reversed().toList() // reverse the list and return it
}


// Instruction
// Write a function to count the total number of elements in a list of lists, that is all the elements in total of each sublist.
// An element in the list or the list itself may be null, count a null as 0.
// Example input and output
//  countElements(subLists(listOf(1,2,3))) -> returns 6
//  countElements(listOf(listOf(1,2,3),null)) -> returns 3
//  countElements(null) -> returns 0
fun countElements(a : List<List<Int>?>?) : Int? {
    if (a == null) return null
    if (a.isEmpty()) return 0
    return a.sumOf { it?.size ?: 0 } // Sum up the size of each list. If a null is in the list, then add 0 to the total.
}


// Instruction
// Write a function to apply a binary function f to the elements in a list of lists.
// Below is a template of an example call.
// listApply(f,[[x1, x2, x3] [y1, y2], [w1]]) = [f(x1, f(x2, x3)), f(y1, y2), w1]
// The second parameter, the list of lists, may be null, in which case the function should return null.
// You may assume that every sublist has at least one element.
// The result of applying a function to a one element sublist is the value of that element.
// Example input and output
//  fun add(x : Int, y : Int) : Int = x + y
//  listApply(::add, subLists(listOf(1,2,3))) -> returns [1, 3, 6]
//  listApply(::add, null) -> null
fun listApply(f: (Int, Int) -> Int, a: List<List<Int>>?) : List<Int>? {
    if (a == null) return null
    if (a.isEmpty()) return listOf()
    val result = mutableListOf<Int>()
    a.forEach{l -> result.add(processList(f, l))} // for each list in a apply f to each element in the list
    return result.toList()
}
fun processList(f: (Int, Int) -> Int, l: List<Int>) : Int {
    if (l.isEmpty()) return 0
    return process(f, l.head, l.tail, 0) // Apply f to the whole list. Peel the head off the list
}
fun process(f: (Int, Int) -> Int, h : Int, l: List<Int>, result : Int) : Int {
    if (l.isEmpty()) return f(h, result) // if the list is empty, then apply the head with the result
    return process(f, l.head, l.tail, f(h, result)) // apply f to head and result and keep recursing
}


// Instruction
// Write a function to build a function that is the composition of the functions in the list a.
// You may assume each function in the list is a unary function (takes one argument).
// This function need not be null safe, that is you may assume that the list of functions always contains at least one function.
// Example input and output
//  fun add1(x : Int) : Int = x + 1
//  fun add2(x : Int) : Int = x + 2
//  val f = composeList(listOf(::add1,::add2))
//  f(4) -> returns 7
fun composeList(a: List<(Int) -> Int>) : (Int) -> Int {
    if (a.size == 1) return a[0] // if there is only one function in a then return that function
    val f = compose(a.head, a.tail.head) // compose the first two items in a
    if (a.size == 2) return f // if there are no more items
    val l = a.tail.tail // get the list of function without the first two
    return compose(l.head, l.tail, f) // recursively compose the remaining functions with f
}
fun compose(h : (Int) -> Int, l: List<(Int) -> Int>, f : (Int) -> Int) : (Int) -> Int {
    if (l.isEmpty()) return compose(h, f) // if no more functions in l then compose h and f and return
    return compose(l.head, l.tail, compose(h, f)) // compose h and f and then keep recursing through the remaining functions
}