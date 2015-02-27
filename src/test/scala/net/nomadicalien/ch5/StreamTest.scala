package net.nomadicalien.ch5

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/20/2015.
 */
class StreamTest extends FunSuite with Matchers {

  test("exercise 5.1, toList") {
    Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
  }

  test("exercise 5.2, take") {
    Stream(1, 2, 3, 4).take(2).toList should be(List(1, 2))
  }

  test("exercise 5.2, drop") {
    Stream(1, 2, 3, 4).drop(2).toList should be(List(3, 4))
  }

  test("exercise 5.3, dropWhile") {
    Stream(1, 2, 3, 4).takeWhile(_ <= 3).toList should be(List(1, 2, 3))
  }

  test("exercise 5.4, exists short-circuit") {
    Stream(2, 4, 6, 3, 10, 12, 14, 16).exists { e =>
      println(s"evaluating:$e")
      e % 2 != 0
    } should be(true)
  }

  test("exercise 5.4, forAll short-circuit") {
    Stream(2, 4, 6, 3, 10, 12, 14, 16).forAll { e =>
      println(s"evaluating:$e")
      e % 2 == 0
    } should be(false)
  }

  test("exercise 5.5, dropWhile via foldRight") {
    Stream(1, 2, 3, 4).takeWhile2(_ <= 3).toList should be(List(1, 2, 3))
  }

  test("exercise 5.6, headOption via foldRight - Some") {
    Stream(1, 2, 3, 4).headOption2 should be(Some(1))
  }

  test("exercise 5.6, headOption via foldRight - None") {
    Empty.headOption2 should be(None)
  }

  test("exercise 5.7, map via foldRight") {
    Stream(1, 2, 3, 4).map(_ * 2).toList should be(List(2, 4, 6, 8))
  }

  test("exercise 5.7, filter via foldRight") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList should be(List(2, 4))
  }

  test("exercise 5.7, append via foldRight") {
    Stream(1, 2, 3, 4).append(Stream(5)).toList should be(List(1, 2, 3, 4, 5))
  }

  test("exercise 5.7, flatMap via foldRight") {
    Stream(1, 2, 3, 4).flatMap(e => Stream(e * 2, e * 3)).toList should be(List(2, 3, 4, 6, 6, 9, 8, 12))
  }

  import Stream._

  test("exercise 5.8, constant") {
    constant(40).take(4).toList should be(List(40, 40, 40, 40))
  }

  test("exercise 5.9, from") {
    from(40).take(4).toList should be(List(40, 41, 42, 43))
  }

  test("exercise 5.10, fibbo") {
    fibbo().take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  test("exercise 5.11, unfold") {
    def next = { s: Int => Some((s + 1, s + 1)) }
    unfold(1)(next).take(7).toList should be(List(2, 3, 4, 5, 6, 7, 8))
  }

  test("exercise 5.12, ones via unfold") {
    ones2.take(3).toList should be(List(1, 1, 1))
  }

  test("exercise 5.12, constant via unfold") {
    constant2(5).take(3).toList should be(List(5, 5, 5))
  }

  test("exercise 5.12, from via unfold") {
    from2(5).take(3).toList should be(List(5, 6, 7))
  }

  test("exercise 5.12, fibbo via unfold") {
    fibbo2().take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }
}
