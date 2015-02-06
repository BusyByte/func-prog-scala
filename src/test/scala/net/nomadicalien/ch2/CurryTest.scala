package net.nomadicalien.ch2

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class CurryTest extends FunSuite with Matchers {
  import net.nomadicalien.ch2.Curry._

  test("curry 2 addition") {
    val curried = curry[Int,Int,Int]((a,b) => a+b)
    val curriedA2 = curried(2)

    curriedA2(3) should be(5)
    curriedA2(9) should be(11)
  }

  test("curry 2 multiply") {
    val curried = curry[Int,Int,Int]((a,b) => a*b)
    val curriedA2 = curried(2)

    curriedA2(3) should be(6)
    curriedA2(9) should be(18)
  }

}
