package net.nomadicalien.ch6

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/20/2015.
 */
class SimpleRNGTest extends FunSuite with Matchers {
  import RNG._
  test("exercise 6.1, nonNegativeInt via nextInt") {
    val rng = new SimpleRNG(20l)

    def validatePositive(r: RNG, n: Int):Unit = {
      val (randN, newRNG) = nonNegativeInt(r)
      println(randN)
      randN should (be >= 0 and be <= Int.MaxValue)
      if(n > 0) {
        validatePositive(newRNG, n-1)
      }
    }

    validatePositive(rng, 1000)
  }



}
