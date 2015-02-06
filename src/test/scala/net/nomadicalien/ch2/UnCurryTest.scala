package net.nomadicalien.ch2

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class UnCurryTest extends FunSuite with Matchers {
  import net.nomadicalien.ch2.UnCurry._

  test("uncurry 2 addition") {
    val u = uncurry({a:Int => {b:Int => a + b}})

    u(2, 3) should be(5)
    u(2, 9) should be(11)
  }

  test("uncurry 2 multiply") {
    val u = uncurry({a:Int => {b:Int => a * b}})

    u(2,3) should be(6)
    u(2,9) should be(18)
  }

}
