package net.nomadicalien.ch3

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x1,xs1), Cons(x2,xs2)) if x1 == x2 => hasSubsequence(xs1, xs2)
    case (Cons(x1,xs1), s) => hasSubsequence(xs1, s)
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B)=>C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1,xs1), Cons(x2,xs2)) => Cons(f(x1,x2), zipWith(xs1,xs2)(f))
  }

  def addTogether(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(x1,xs1), Cons(x2, xs2)) => Cons(x1 + x2, addTogether(xs1, xs2))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((a,b)=> foldRight(f(a),b)((a,b)=> Cons(a,b)))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a=> if(f(a)) List(a) else Nil)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((a:A,b:List[A]) => {
      if(f(a))
        Cons(a, b)
      else
        b
    })
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((a:A,b:List[B]) => Cons(f(a), b))
  }

  def dToString(as: List[Double]):List[String] = {
    foldRight(as, Nil:List[String])((a:Double,b:List[String]) => Cons(a.toString, b))
  }

  def add1(as: List[Int]):List[Int] = {
    foldRight(as, Nil:List[Int])((a:Int,b:List[Int]) => Cons(a + 1, b))
  }

  def concat[A](as: List[List[A]]):List[A] = {
    foldRight(as, Nil:List[A])(append)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
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
