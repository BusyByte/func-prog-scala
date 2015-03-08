package net.nomadicalien.ch6

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/20/2015.
 */
class SimpleRNGTest extends FunSuite with Matchers {
  import RNG._
  test("exercise 6.1, nonNegativeInt via nextInt") {
    val rng = new SimpleRNG(20l)
    val (n1, rnd1) = nonNegativeInt(rng)
    n1 should be(7694978)
    val (n2, rnd2) = nonNegativeInt(rnd1)
    n2 should be(1630622802)
  }



}
