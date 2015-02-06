package net.nomadicalien.ch2

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class ComposeTest extends FunSuite with Matchers {
  import net.nomadicalien.ch2.Compose._

  test("compose 2 mult, addition") {
    val c = compose[Int,Int,Int]({_ + 2 },{_ * 2})
    c(2) should be (6)
    c(4) should be (10)
  }

  test("compose 2 add, mult") {
    val c = compose[Int,Int,Int]({_ * 2 },{_ + 2})
    c(2) should be (8)
    c(4) should be (12)
  }
}
