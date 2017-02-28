package net.nomadicalien.ch2

import org.specs2.mutable.Specification

class UnCurrySpec extends Specification {
  import net.nomadicalien.ch2.UnCurry._

  "uncurry 2 addition" in {
    val u = uncurry({a:Int => {b:Int => a + b}})

    u(2, 3) must_== (5)
    u(2, 9) must_== (11)
  }

  "uncurry 2 multiply" in {
    val u = uncurry({a:Int => {b:Int => a * b}})

    u(2,3) must_== (6)
    u(2,9) must_== (18)
  }

}
