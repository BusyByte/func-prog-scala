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
}
