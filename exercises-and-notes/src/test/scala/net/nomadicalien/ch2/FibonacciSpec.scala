package net.nomadicalien.ch2

import org.specs2.mutable.Specification

class FibonacciSpec extends Specification {
  import Fibonacci.fibbo

  "fib(-1) should throw exception" in {
    { fibbo(-1) } must throwA[IllegalStateException]
  }

  "fib(0) = 0" in {
    fibbo(0) must_== (0)
  }

  "fib(1) = 1" in {
    fibbo(1) must_== (1)
  }

  "fib(2) = 1" in {
    fibbo(2) must_== (1)
  }

  "fib(3) = 2" in {
    fibbo(3) must_== (2)
  }

  "fib(4) = 3" in {
    fibbo(4) must_== (3)
  }

  "fib(5) = 5" in {
    fibbo(5) must_== (5)
  }
}
