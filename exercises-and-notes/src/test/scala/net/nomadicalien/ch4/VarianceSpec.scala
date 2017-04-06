package net.nomadicalien.ch4

import org.specs2.mutable.Specification

class VarianceSpec extends Specification {
  import net.nomadicalien.ch4.Variance._
  "exercise 4.2, variance of zero" in {
    variance(Seq(5.0,5.0,5.0)) must_== (Some(0.0))
  }

  "exercise 4.2, variance" in {
    variance(Seq(1.0,2.0,3.0,4.0,5.0,6.0)).getOrElse(0.0) must beCloseTo(2.916, 0.001)
  }

  "exercise 4.2, variance None" in {
    variance(Seq[Double]()) must_== (None)
  }
}
