package net.nomadicalien.ch3

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * Created by Shawn on 2/8/2015.
 */
object List {

  def concat[A](as: List[List[A]]):List[A] = {
    foldLeft(as, Nil:List[A])((b,a) => foldLeft(a,b)((b,a)=>Cons(a,b)))
  }

  def append[A](as: List[A], elem:A): List[A] = {
    foldRight(as, Cons(elem, Nil))(Cons(_,_))
  }

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
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)


  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product4(as : List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc,elem)=>acc+1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((acc,elem) => Cons(elem, acc))

  def length[A](as: List[A]): Int = foldRight(as, 0)((elem,acc)=>acc+1)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def product3(ns: List[Double]) = {
    val multi = (a:Double,b:Double) => {
      //println(s"multiplying $a and $b")
      a * b
    }
    def foldRightMult(l:List[Double], z:Double):Double = l match {
      case Nil => z
      case Cons(x, xs) if x != 0.0 => multi(x, foldRightMult(xs, z))
      case _ => 0
    }
    foldRightMult(ns, 1.0)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
