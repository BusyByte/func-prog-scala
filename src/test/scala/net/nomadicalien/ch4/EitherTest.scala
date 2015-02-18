package net.nomadicalien.ch4

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class EitherTest extends FunSuite with Matchers {
  test("exercise 4.6, map - Right") {
    Right(2).map(_ * 2) should be(Right(4))
  }

  test("exercise 4.6, map - Left") {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.map(_ * 2) should be(Left("ErrorMessage"))
  }

  test("exercise 4.6, flatMap - Right") {
    def timesTwo(a: Int) = {
      Right(a * 2)
    }
    Right(2).flatMap(timesTwo) should be(Right(4))
  }

  test("exercise 4.6, flatMap - Left") {
    val e: Either[String, Int] = Left("ErrorMessage")
    def timesTwo(a: Int) = {
      Right(a * 2)
    }
    e.flatMap(timesTwo) should be(Left("ErrorMessage"))
  }

  test("exercise 4.6, orElse - Right") {
    Right(2).orElse(Right(4)) should be(Right(2))
  }

  test("exercise 4.6, orElse - Left") {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.orElse(Right(4)) should be(Right(4))
  }

  test("exercise 4.6, map2 - Right") {
    Right(2).map2(Right(4))(_ + _) should be(Right(6))
  }

  test("exercise 4.6, map2 - Left") {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.orElse(Right(4)) should be(Right(4))
  }

  import Either._

  test("exercise 4.7, sequence - Left") {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Left("ErrorMessage"), Right(4))
    sequence(list) should be(Left("ErrorMessage"))
  }

  test("exercise 4.7, sequence - Right") {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    sequence(list) should be(Right(List(1, 2, 3, 4)))
  }

  test("exercise 4.7, traverse - Left") {
    val list = List(1, 2, 3, 4)
    val func: Map[Int, Either[String, Int]] = Map[Int, Either[String, Int]](1 -> Right(1), 2 -> Right(2), 3 -> Left("ErrorMessage"), 4 -> Right(4))
    traverse(list)(func) should be(Left("ErrorMessage"))
  }

  test("exercise 4.7, traverse - Right") {
    val list = List(1, 2, 3, 4)
    val func: Map[Int, Either[String, Int]] = Map[Int, Either[String, Int]](1 -> Right(1), 2 -> Right(2), 3 -> Right(3), 4 -> Right(4))
    traverse(list)(func) should be(Right(List(1, 2, 3, 4)))
  }

  test("exercise 4.7, sequence2 - Left") {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Left("ErrorMessage"), Right(4))
    sequence2(list) should be(Left("ErrorMessage"))
  }

  test("exercise 4.7, sequence2 - Right") {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    sequence2(list) should be(Right(List(1, 2, 3, 4)))
  }


}
