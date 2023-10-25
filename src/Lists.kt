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

// Write a function to build a bounded list of counting numbers (counting numbers start at 1) up to (and including) n.
// The counting numbers up to n are the list [1, 2, 3, ..., n].
// You may assume n is an Int or a null, that is, the function must ensure null safety.
fun countingNumbers(limit : Int?) : List<Int>? {
    if (limit == null) return null
    if (limit == 0) return listOf()
    val a = mutableListOf<Int>()
    for (i in 1..limit) {
        a.add(i)
    }
    return a
}

fun evenNumbers(n : Int?) : List<Int>? {

}

fun primeNumbers(n : Int?)  : List<Int>? {

}

fun<T : Comparable<T>> merge(a : List<T>?, b : List<T>?) : List<T>? {

}

fun subLists(a : List<Int>?) : List<Int>? {

}

fun countElements(a : List<Int>?) : Int {

}

fun listApply(f : (List<Int>) -> Int, a : List<List<Int>>) : List<Int> {

}

fun composeList(a : List<(Int, Int) -> Int>) : Int {

}