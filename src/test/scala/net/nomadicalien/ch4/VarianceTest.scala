package net.nomadicalien.ch4

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class VarianceTest extends FunSuite with Matchers {
  import net.nomadicalien.ch4.Variance._
  test("exercise 4.2, variance of zero") {
      variance(Seq(5.0,5.0,5.0)) should be(Some(0.0))
  }

  test("exercise 4.2, variance") {
   variance(Seq(1.0,2.0,3.0,4.0,5.0,6.0)).map(v=> v should equal(2.916 +- 0.001)).getOrElse(fail("should be some"))
  }

  test("exercise 4.2, variance None") {
    variance(Seq[Double]()) should be(None)
  }
}
