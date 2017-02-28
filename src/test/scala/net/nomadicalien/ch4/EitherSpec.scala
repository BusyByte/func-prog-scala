package net.nomadicalien.ch4

import org.specs2.mutable.Specification

class EitherSpec extends Specification {
  "exercise 4.6, map - Right" in {
    Right(2).map(_ * 2) must_== (Right(4))
  }

  "exercise 4.6, map - Left" in {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.map(_ * 2) must_== (Left("ErrorMessage"))
  }

  "exercise 4.6, flatMap - Right" in {
    def timesTwo(a: Int) = {
      Right(a * 2)
    }
    Right(2).flatMap(timesTwo) must_== (Right(4))
  }

  "exercise 4.6, flatMap - Left" in {
    val e: Either[String, Int] = Left("ErrorMessage")
    def timesTwo(a: Int) = {
      Right(a * 2)
    }
    e.flatMap(timesTwo) must_== (Left("ErrorMessage"))
  }

  "exercise 4.6, orElse - Right" in {
    Right(2).orElse(Right(4)) must_== (Right(2))
  }

  "exercise 4.6, orElse - Left" in {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.orElse(Right(4)) must_== (Right(4))
  }

  "exercise 4.6, map2 - Right" in {
    Right(2).map2(Right(4))(_ + _) must_== (Right(6))
  }

  "exercise 4.6, map2 - Left" in {
    val e: Either[String, Int] = Left("ErrorMessage")
    e.orElse(Right(4)) must_== (Right(4))
  }

  import Either._

  "exercise 4.7, sequence - Left" in {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Left("ErrorMessage"), Right(4))
    sequence(list) must_== (Left("ErrorMessage"))
  }

  "exercise 4.7, sequence - Right" in {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    sequence(list) must_== (Right(List(1, 2, 3, 4)))
  }

  "exercise 4.7, traverse - Left" in {
    val list = List(1, 2, 3, 4)
    val func: Map[Int, Either[String, Int]] = Map[Int, Either[String, Int]](1 -> Right(1), 2 -> Right(2), 3 -> Left("ErrorMessage"), 4 -> Right(4))
    traverse(list)(func) must_== (Left("ErrorMessage"))
  }

  "exercise 4.7, traverse - Right" in {
    val list = List(1, 2, 3, 4)
    val func: Map[Int, Either[String, Int]] = Map[Int, Either[String, Int]](1 -> Right(1), 2 -> Right(2), 3 -> Right(3), 4 -> Right(4))
    traverse(list)(func) must_== (Right(List(1, 2, 3, 4)))
  }

  "exercise 4.7, sequence2 - Left" in {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Left("ErrorMessage"), Right(4))
    sequence2(list) must_== (Left("ErrorMessage"))
  }

  "exercise 4.7, sequence2 - Right" in {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    sequence2(list) must_== (Right(List(1, 2, 3, 4)))
  }


}
