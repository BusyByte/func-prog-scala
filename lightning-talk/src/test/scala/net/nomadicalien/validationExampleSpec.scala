package net.nomadicalien

import scalaz.Failure

class validationExampleSpec extends org.specs2.mutable.Specification {
  import lightning.validationExample._
  "combine results keeps only one failure because of semigroup" in {
    combineResults(0, 4) must_== Failure(DivisionByZeroError)
  }
}
