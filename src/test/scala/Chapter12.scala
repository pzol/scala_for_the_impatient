import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Chapter12 extends FlatSpec with ShouldMatchers {
  "Exercise 1" should "yield a collection of function inputs and outputs in a given range" in {
    def values(fun: (Int) => Int, low: Int, high: Int) = (low to high).map(i => (i, fun(i)))

    values(x => x * x, -5, 5) should be === Seq((-5,25), (-4,16), (-3,9), (-2,4), (-1,1), (0,0), (1,1), (2,4), (3,9), (4,16), (5,25))
  }

  "Exercise 2" should "get the largest element of an array with reduceLeft" in {
    Array(1,4,2,3).reduceLeft(Math.max(_,_)) should be === 4
  }

  "Exercise 3" should "implement the factorial function using to and reduceLeft" in {
    val factorial = (n: Int) => if (n < 1) 1 else (n to 1 by -1).reduceLeft(_ * _)

    factorial(4) should be === 24
    factorial(0) should be === 1
  }

  "Exercise 4" should "implement the factorial function using foldLeft" in {
    val factorial = (n: Int) => (n to 1 by -1).foldLeft(1)(_ * _)

    factorial(4) should be === 24
    factorial(0) should be === 1
  }

  "Exercise 5" should "implement a function largest that yields the largest value within a given sequence" in {
    val largest = (fun: (Int) => Int, inputs: Seq[Int]) => inputs.reduceLeft((x, y) => Math.max(x, fun(y)))

    largest(x => 10 * x - x * x, 1 to 10) should be === 25
  }

  "Exercise 6" should "return the input at which the output is largest" in {
    val largestAt =  (fun: Int => Int, inputs: Seq[Int]) => inputs.map(x => (x, fun(x))).reduceLeft((x, y) => if (x._2 > y._2) x else y)._1
    val largestAt2 = (fun: Int => Int, inputs: Seq[Int]) => inputs.maxBy(fun)
    val largestAt3 = (fun: Int => Int, inputs: Seq[Int]) => inputs.map(e => (fun(e), e)).max._2
    val fun = (x: Int) => 10 * x - x * x
    largestAt(fun, 1 to 10) should be === 5
    largestAt2(fun, 1 to 10) should be === 5
    largestAt3(fun, 1 to 10) should be === 5
  }

  "Exercise 7" should "add sequences with currying" in {
    def adjustToPair(fun: (Int,Int) => Int)(x: (Int,Int)) = fun(x._1, x._2)
    adjustToPair(_*_)((6,7)) should be === 42

    val add = adjustToPair(_+_)_
    ((1 to 10) zip (11 to 20)).map { add } should be === Vector(12, 14, 16, 18, 20, 22, 24, 26, 28, 30)
  }

  "Exercise 8" should "call corresponds to check whether the elements in an array of strings have the lengths given in an array of integers" in {
    val a = Array("Hello", "World")
    val b = Array(5, 5)
    a.corresponds(b) { _.length == _ } should be === true
  }

  // Exercise 9 ignored

  "Exercise 10" should "implement an unless control abstraction that works like if" in {
    def unless(condition: => Boolean)(block: => Unit) { if(!condition) block }
    val result = unless(false) { "foo" } 
  }
}
