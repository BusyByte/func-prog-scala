package net.nomadicalien.ch2

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Shawn on 2/4/2015.
 */
class FibonacciTest extends FunSuite with Matchers {
  import Fibonacci.fibbo
  test("fib(-1) should throw exception") {
    a [IllegalStateException] should be thrownBy { fibbo(-1) }
  }

  test("fib(0) = 0") {
    fibbo(0) should be(0)
  }

  test("fib(1) = 1") {
    fibbo(1) should be(1)
  }

  test("fib(2) = 1") {
    fibbo(2) should be(1)
  }

  test("fib(3) = 2") {
    fibbo(3) should be(2)
  }

  test("fib(4) = 3") {
    fibbo(4) should be(3)
  }

  test("fib(5) = 5") {
    fibbo(5) should be(5)
  }
}
