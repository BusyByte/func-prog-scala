package net.nomadicalien.ch6

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Shawn on 3/24/2015.
 */
class StateTest  extends FunSuite with Matchers {
  val rng = new SimpleRNG(20l)


  test("exercise 6.10, gen number") {
    State(RNG.nonNegativeInt).run(rng)._1 should be(7694978)
  }

  test("exercise 6.10, map") {
    State(RNG.nonNegativeInt).map { i => i / 10 }.run(rng)._1 should be(769497)
  }

  test("exercise 6.10, unit") {
    State.unit(10).run(rng)._1 should be(10)
  }

  test("exercise 6.10, map2") {
    State(RNG.nonNegativeInt).map2(State.unit(10)) { (nonNeg, ten) => nonNeg / ten }.run(rng)._1 should be(769497)
  }

  test("exercise 6.10, sequence") {
    State.sequence(List[State.Rand[Int]](State(RNG.nonNegativeInt), State(RNG.nonNegativeEven))).run(rng)._1 should be(List(7694978, 1630622802))
  }
}