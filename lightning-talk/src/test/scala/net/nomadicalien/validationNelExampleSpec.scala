package net.nomadicalien

import scalaz.{Failure, NonEmptyList}

class validationNelExampleSpec extends org.specs2.mutable.Specification {
  import lightning.validationNelExample._
  "combine results keeps both failures" in {
    combineResults(0, 4) must_== Failure(NonEmptyList(DivisionByZeroError, ZeroResultError))
  }
}
