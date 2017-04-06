package net.nomadicalien.ch6

import org.specs2.mutable.Specification

class StateSpec extends Specification {
  val rng = SimpleRNG(20l)

  "exercise 6.10, gen number" in {
    State(RNG.nonNegativeInt).run(rng)._1 must_== (7694978)
  }

  "exercise 6.10, map" in {
    State(RNG.nonNegativeInt).map { i => i / 10 }.run(rng)._1 must_== (769497)
  }

  "exercise 6.10, unit" in {
    State.unit(10).run(rng)._1 must_== (10)
  }

  "exercise 6.10, map2" in {
    State(RNG.nonNegativeInt).map2(State.unit(10)) { (nonNeg, ten) => nonNeg / ten }.run(rng)._1 must_== (769497)
  }

  "exercise 6.10, sequence" in {
    State.sequence(List[State.Rand[Int]](State(RNG.nonNegativeInt), State(RNG.nonNegativeEven))).run(rng)._1 must_== (List(7694978, 1630622802))
  }
}