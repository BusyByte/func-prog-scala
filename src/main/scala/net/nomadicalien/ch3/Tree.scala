package net.nomadicalien.ch3

import scala.annotation.tailrec

/**
 * Created by Shawn on 2/9/2015.
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(as: Tree[Int]): Int = {
    def maxIter(t: Tree[Int], maxValue: Int): Int = t match {
      case Leaf(v) => v.max(maxValue)
      case Branch(l,r) => maxIter(l, maxIter(r,maxValue))
    }
    maxIter(as, -1)//assumption is everything is greater or equal to zero
  }

  def depth[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](as: Tree[A]): Int = fold(as)(a=>1)(1 + _ + _)

  def maximum2(as: Tree[Int]): Int = fold(as)(a=>a)(_ max _)

  def depth2[A](as: Tree[A]): Int = fold(as)(a=>1)((d1,d2)=>1 + (d1 max d2) )

  def map2[A,B](as: Tree[A])(f: A => B): Tree[B] = fold(as)(a=>Leaf(f(a)):Tree[B])(Branch(_,_))

}