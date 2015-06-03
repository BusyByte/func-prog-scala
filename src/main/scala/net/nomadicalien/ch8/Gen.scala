package net.nomadicalien.ch8

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

import net.nomadicalien.ch6.{RNG, State}

import scala.annotation.tailrec

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  //def check: Unit
  //def check: Boolean
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  /*def &&(p: Prop): Prop = new Prop {
    self =>
    def check = self.check && p.check
  }*/
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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
}
case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap {n => this.listOfN(n)}

}

/*trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}*/

trait SGen[+A] {

}

