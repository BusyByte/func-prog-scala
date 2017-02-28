package net.nomadicalien.ch2

import org.specs2.mutable.Specification

class CurrySpec extends Specification {
  import net.nomadicalien.ch2.Curry._

  "curry 2 addition" in {
    val curried = curry[Int,Int,Int]((a,b) => a+b)
    val curriedA2 = curried(2)

    curriedA2(3) must_== (5)
    curriedA2(9) must_== (11)
  }

  "curry 2 multiply" in {
    val curried = curry[Int,Int,Int]((a,b) => a*b)
    val curriedA2 = curried(2)

    curriedA2(3) must_== (6)
    curriedA2(9) must_== (18)
  }

}
