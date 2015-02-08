package net.nomadicalien.ch3


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * Created by Shawn on 2/8/2015.
 */
object List {

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case l => l
  }

  def drop[A](n: Int, l: List[A]): List[A] = (l,n) match {
    case (Nil,_) => Nil
    case (x,0) => x
    case (Cons(x, xs),count) => drop(count - 1, xs)
  }

  def tail[A](As: List[A]): List[A] = As match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](head: A, As: List[A]):List[A] = As match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  def sum(As: List[Int]): Int = As match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
