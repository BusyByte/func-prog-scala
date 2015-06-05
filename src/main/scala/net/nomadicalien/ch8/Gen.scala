package net.nomadicalien.ch8

import _root_.fpinscala.state.RNG
import _root_.fpinscala.testing.Prop
import _root_.fpinscala.testing.exhaustive.Gen
import net.nomadicalien.ch6.{RNG, State}
import net.nomadicalien.ch8.Prop._
import net.nomadicalien.ch5.Stream
import net.nomadicalien.ch5.Stream._
import org.scalacheck.Test.Failed


import scala.annotation.tailrec

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
/*
trait Prop {
  //def check: Unit
  //def check: Boolean
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  /*def &&(p: Prop): Prop = new Prop {
    self =>
    def check = self.check && p.check
  }*/
}*/

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (maxSize,numCases, rng) =>
      run(maxSize,numCases, rng) match {
        case Passed => p.run(maxSize,numCases, rng)
        case f => f
      }
  }
  def ||(p: Prop): Prop = Prop {
    (maxSize,numCases, rng) =>
      run(maxSize,numCases, rng) match {
        case Falsified(failureMessage,_) => p.tag(failureMessage).run(maxSize,numCases,rng)
        case p => p
      }
  }

  def tag(msg: String) = Prop {
    (maxSize,numCases,rng) => run(maxSize,numCases,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type MaxSize = Int
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
 // type Result = Option[(FailedCase, SuccessCount)]
 def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
   (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
     case (a, i) => try {
       if (f(a)) Passed else Falsified(a.toString, i)
     } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
   }.find(_.isFalsified).getOrElse(Passed)
 }
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    @tailrec def c(r: RNG): (Int, RNG) = {
      val nextPair = r.nextInt
      val getneratedInt: SuccessCount = nextPair._1
      if(getneratedInt >= start && getneratedInt < stopExclusive) {
        nextPair
      } else {
        c(nextPair._2)
      }
    }

    Gen[Int](
      State(c)
    )
  }
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))
  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap {useLeft =>
  if(useLeft)
    g1
  else
    g2
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val cutoff = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double)).flatMap { d =>
      if(d > cutoff)
        g2._1
      else
        g1._1

    }
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => listOfN(i, g))

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    (ns.isEmpty || nss.tail.isEmpty || !ns.zip(ns.tail).exists {
      case (a,b) => a > b
    })
  }
}

case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap {n => this.listOfN(n)}

  def unsized: SGen[A] = SGen(_ => this)


}

/*trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}*/
/*
trait SGen[+A] {

}*/
case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))
}
