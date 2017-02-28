package net.nomadicalien.ch5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "exercise 5.1, toList" in {
    Stream(1, 2, 3, 4).toList must_== (List(1, 2, 3, 4))
  }

  "exercise 5.2, take" in {
    Stream(1, 2, 3, 4).take(2).toList must_== (List(1, 2))
  }

  "exercise 5.2, drop" in {
    Stream(1, 2, 3, 4).drop(2).toList must_== (List(3, 4))
  }

  "exercise 5.3, dropWhile" in {
    Stream(1, 2, 3, 4).takeWhile(_ <= 3).toList must_== (List(1, 2, 3))
  }

  "exercise 5.4, exists short-circuit" in {
    Stream(2, 4, 6, 3, 10, 12, 14, 16).exists { e =>
      println(s"evaluating:$e")
      e % 2 != 0
    } must_== (true)
  }

  "exercise 5.4, forAll short-circuit" in {
    Stream(2, 4, 6, 3, 10, 12, 14, 16).forAll { e =>
      println(s"evaluating:$e")
      e % 2 == 0
    } must_== (false)
  }

  "exercise 5.5, dropWhile via foldRight" in {
    Stream(1, 2, 3, 4).takeWhile2(_ <= 3).toList must_== (List(1, 2, 3))
  }

  "exercise 5.6, headOption via foldRight - Some" in {
    Stream(1, 2, 3, 4).headOption2 must_== (Some(1))
  }

  "exercise 5.6, headOption via foldRight - None" in {
    Empty.headOption2 must_== (None)
  }

  "exercise 5.7, map via foldRight" in {
    Stream(1, 2, 3, 4).map(_ * 2).toList must_== (List(2, 4, 6, 8))
  }

  "exercise 5.7, filter via foldRight" in {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList must_== (List(2, 4))
  }

  "exercise 5.7, append via foldRight" in {
    Stream(1, 2, 3, 4).append(Stream(5)).toList must_== (List(1, 2, 3, 4, 5))
  }

  "exercise 5.7, flatMap via foldRight" in {
    Stream(1, 2, 3, 4).flatMap(e => Stream(e * 2, e * 3)).toList must_== (List(2, 3, 4, 6, 6, 9, 8, 12))
  }

  import net.nomadicalien.ch5.Stream._

  "exercise 5.8, constant" in {
    constant(40).take(4).toList must_== (List(40, 40, 40, 40))
  }

  "exercise 5.9, from" in {
    from(40).take(4).toList must_== (List(40, 41, 42, 43))
  }

  "exercise 5.10, fibbo" in {
    fibbo().take(7).toList must_== (List(0, 1, 1, 2, 3, 5, 8))
  }

  "exercise 5.11, unfold" in {
    def next = { s: Int => Some((s + 1, s + 1)) }
    unfold(1)(next).take(7).toList must_== (List(2, 3, 4, 5, 6, 7, 8))
  }

  "exercise 5.12, ones via unfold" in {
    ones2.take(3).toList must_== (List(1, 1, 1))
  }

  "exercise 5.12, constant via unfold" in {
    constant2(5).take(3).toList must_== (List(5, 5, 5))
  }

  "exercise 5.12, from via unfold" in {
    from2(5).take(3).toList must_== (List(5, 6, 7))
  }

  "exercise 5.12, fibbo via unfold" in {
    fibbo2().take(7).toList must_== (List(0, 1, 1, 2, 3, 5, 8))
  }

  "exercise 5.13, map via unfold" in {
    Stream(1, 2, 3, 4).map2(_ * 2).toList must_== (List(2, 4, 6, 8))
  }

  "exercise 5.13, take via unfold" in {
    Stream(1, 2, 3, 4).take2(2).toList must_== (List(1, 2))
  }

  "exercise 5.13, dropWhile via unfold" in {
    Stream(1, 2, 3, 4).takeWhile3(_ <= 3).toList must_== (List(1, 2, 3))
  }

  "exercise 5.13, zipWith via unfold" in {
    Stream(1, 2, 3, 4).zipWith(Stream(2, 3, 4, 5))((a, b) => a + b).toList must_== (List(3, 5, 7, 9))
  }

  "exercise 5.13, zipAll via unfold" in {
    Stream(1, 2, 3, 4).zipAll(Stream(2, 3)).toList must_== (List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None), (Some(4), None)))
  }

  "exercise 5.14, startsWith via other methods" in {
    Stream(1,2,3) startsWith Stream(1,2) must_== (true)
  }

  "exercise 5.15, tails via unfold" in {
    Stream(1,2,3).tails.toList.map(_.toList) must_== (List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "exercise 5.16, scanRight" in {
    Stream(1,2,3).scanRight(0)(_ + _).toList must_== (List(6,5,3,0))
  }


}
