package net.nomadicalien.ch6

import org.specs2.matcher.{BetweenMatcher, Expectable, MatchResult, Matcher}
import org.specs2.mutable.Specification

import scala.annotation.tailrec


object SimpleRNGSpec {
  val ITERATIONS = 1000
}

class SimpleRNGSpec extends Specification {

  import RNG._
  import SimpleRNGSpec._

  val rng = new SimpleRNG(20l)

  "exercise 6.1, nonNegativeInt via nextInt" in {
    genN(rng, ITERATIONS, nonNegativeInt) must contain(bePositiveInt).forall
  }


  @tailrec
  private def genN[A](r: RNG, n: Int, f: (RNG) => (A, RNG), acc: List[A] = Nil): List[A] = {
    if(n <= 0) {
      acc
    } else {
      val (randNum, nextRNG) = f(r)
      genN(nextRNG, n - 1, f, randNum :: acc)
    }
  }

  "exercise 6.2, double" in {
    genN(rng, ITERATIONS, double) must contain(beBetween(0.0d, 1.0d))
  }

  private def beBetweenZeroAndOne: Matcher[Double] = beBetween(0.0d, 1.0d)
  private def bePositiveInt: Matcher[Int] = be_>(0)
  type IntDouble = (Int, Double)
  type DoubleInt = (Double, Int)
  type D3 = (Double, Double, Double)
  import org.specs2.matcher.MatchersImplicits._

//
//  /*
//    def apply[S <: T](a: Expectable[S]) = {
//    val r = a.value <= n
//    val isEqual = a.value == n
//    result(r,
//           if (isEqual) description(a) + " is equal to " + n.toString else description(a) + " is less than " + n.toString,
//           description(a) + " is greater than " + n.toString,
//           a)
//  }
//    */
//
//  val intDoubleMatcher = new Matcher[IntDouble] {
//    def apply[S <: (Int, Double)](a: Expectable[S]): MatchResult[S] = {
//      val (i, d) = a.value
//      bePositiveInt.apply(a.map(_._1))
//
//      result()
//    }
//  }
//  private def beIntDouble(i: IntDouble): Matcher[(Int,Double)] =
//    (i._1 must bePositiveInt) and (i._2 must beBetweenZeroAndOne)
  private def intDoubleMatcher: Matcher[IntDouble] = (id:IntDouble) => (id._1 must bePositiveInt) and (id._2 must beBetweenZeroAndOne)
  private def doubleIntMatcher: Matcher[DoubleInt] = (id:DoubleInt) =>  (id._1 must beBetweenZeroAndOne) and (id._2 must bePositiveInt)
  private def d3Matcher: Matcher[D3] = (id:D3) =>  (id._1 must beBetweenZeroAndOne) and (id._2 must beBetweenZeroAndOne) and (id._3 must beBetweenZeroAndOne)


  "exercise 6.3, combinations" in {
    genN(rng, ITERATIONS, intDouble) must contain(intDoubleMatcher).forall
    genN(rng, ITERATIONS, doubleInt) must contain(doubleIntMatcher).forall
    genN(rng, ITERATIONS, double3) must contain(d3Matcher).forall
  }

  "exercise 6.4, ints" in {
    ints(4)(rng)._1 must_== (List(664650556, 1500047067, 1630622802, 7694978))
  }


  "exercise 6.5, double via map" in {
    genN(rng, ITERATIONS, double2) must contain(beBetweenZeroAndOne).forall
  }

  "exercise 6.6, map2" in {
    val f = map2(nonNegativeInt, nonNegativeEven) { (a, b) =>
      a must_== (7694978)
      b must_== (1630622802)
      a + b
    }

    f(rng)._1 must_== (1638317780)
  }

  "exercise 6.7, sequence" in {
    val listOfRand = List(nonNegativeInt _, nonNegativeInt _)
    sequence(listOfRand)(rng)._1 must_== (List(7694978, 1630622802))
  }

  "exercise 6.7, ints" in {
    ints2(2)(rng)._1 must_== (List(7694978, -1630622802))
  }

  "exercise 6.8, nonNegativeLessThan" in {
    nonNegativeLessThan(100)(rng)._1 must_== (78)
  }


  "exercise 6.9, mapSingle" in {
    val f = mapSingle(nonNegativeInt) { a =>
      a must_== (7694978)
      a % 2 == 0
    }

    f(rng)._1 must_== (true)
  }

  "exercise 6.9, mapTwo" in {
    val f = mapTwo(nonNegativeInt, nonNegativeEven) { (a, b) =>
      a must_== (7694978)
      b must_== (1630622802)
      a + b
    }

    f(rng)._1 must_== (1638317780)
  }

}
