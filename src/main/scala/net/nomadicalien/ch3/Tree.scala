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
}