package net.nomadicalien.ch6

import org.scalatest.{FunSuite, Matchers}


object SimpleRNGTest {
  val ITERATIONS = 1000
}

/**
 * Created by Shawn on 2/20/2015.
 */
class SimpleRNGTest extends FunSuite with Matchers {

  import RNG._
  import SimpleRNGTest._

  val rng = new SimpleRNG(20l)

  test("exercise 6.1, nonNegativeInt via nextInt") {
    validateN(rng, ITERATIONS, nonNegativeInt, validatePositiveInt)
  }

  private def validatePositiveInt(i: Int): Unit = {
    i should (be >= 0 and be <= Int.MaxValue)
  }

  private def validateN[A](r: RNG, n: Int, f: (RNG) => (A,RNG), v: (A) => Unit): Unit = {
    val (randNum, nextRNG) = f(r)
    println(randNum)
    v(randNum)
    if (n > 0) {
      validateN(nextRNG, n - 1, f, v)
    }
  }

  test("exercise 6.2, double") {
    validateN(rng, ITERATIONS, double, validateDoubleBetweenZeroAndOne)
  }

  private def validateDoubleBetweenZeroAndOne(d: Double): Unit = {
    d should (be >= 0.0d and be <= 1.0d)
  }

  private def validateIntDouble(p: (Int,Double)): Unit = {
    validatePositiveInt(p._1)
    validateDoubleBetweenZeroAndOne(p._2)
  }

  private def validateDoubleInt(p: (Double,Int)): Unit = {
    validateIntDouble(p.swap)
  }

  private def validateDouble3(p: (Double,Double,Double)): Unit = {
    validateDoubleBetweenZeroAndOne(p._1)
    validateDoubleBetweenZeroAndOne(p._2)
    validateDoubleBetweenZeroAndOne(p._3)
  }

  test("exercise 6.3, combinations") {
    validateN(rng, ITERATIONS, intDouble, validateIntDouble)
    validateN(rng, ITERATIONS, doubleInt, validateDoubleInt)
    validateN(rng, ITERATIONS, double3, validateDouble3)
  }

  test("exercise 6.4, ints") {
    ints(4)(rng)._1 should be(List(664650556, 1500047067, 1630622802, 7694978))
  }

  private def validateN2[A](r: RNG, n: Int, f: => Rand[A], v: (A) => Unit): Unit = {
    val (randNum, nextRNG) = f(r)
    println(randNum)
    v(randNum)
    if (n > 0) {
      validateN2(nextRNG, n - 1, f, v)
    }
  }

  test("exercise 6.5, double via map") {
    validateN2(rng, ITERATIONS, double2, validateDoubleBetweenZeroAndOne)
  }

  test("exercise 6.6, map2") {
    val f = map2(nonNegativeInt, nonNegativeEven){(a,b) =>
      a should be(7694978)
      b should be(1630622802)
      a + b
    }

    f(rng)._1 should be(1638317780)
  }
}
