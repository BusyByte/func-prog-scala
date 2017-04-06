package net.nomadicalien.ch2

import org.specs2.mutable.Specification

class ComposeSpec extends Specification {
  import net.nomadicalien.ch2.Compose._

  "compose 2 mult, addition" in {
    val c = compose[Int,Int,Int]({_ + 2 },{_ * 2})
    c(2) must_== (6)
    c(4) must_== (10)
  }

  "compose 2 add, mult" in {
    val c = compose[Int,Int,Int]({_ * 2 },{_ + 2})
    c(2) must_== (8)
    c(4) must_== (12)
  }
}
