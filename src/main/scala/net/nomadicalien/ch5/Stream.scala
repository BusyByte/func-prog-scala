package net.nomadicalien.ch5

import net.nomadicalien.ch5.Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case s@Cons(_, _) if n == 0 => s
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((e, a) => Cons(() => f(e), () => a))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((e, a) => if (f(e)) Cons(() => e, () => a) else a)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((e, a) => Cons(() => e, () => a))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((e, a) => f(e).append(a))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold(this) {
      case Cons(h, t) if n > 0 => Some(h(), t().take2(n - 1))
      case _ => None
    }

  def takeWhile3(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t().takeWhile3(f))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty: Stream[B]))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty: Stream[A], t2()))
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case Cons(s,t) => Some(Cons(s,t), t())
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z,Stream(z))) { (elem, accum) =>
      lazy val prior = accum
      lazy val priorB = prior._1
      lazy val priorStreamOfB = prior._2
      val b2 = f(elem, priorB)
      (b2, cons(b2, priorStreamOfB))
    }._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    Cons(() => n, () => from(n + 1))

  def fibbo(): Stream[Int] = {
    def f(n0: Int, n1: Int): Stream[Int] =
      Cons(() => n0, () => f(n1, n0 + n1))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
      case None => Empty
    }

  val ones: Stream[Int] = Stream.cons(1, ones)

  val ones2: Stream[Int] =
    unfold(1)(s => Some(s, s))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibbo2(): Stream[Int] =
    unfold((0, 1)) { s =>
      val n0 = s._1
      val n1 = s._2
      val n2 = n0 + n1
      Some(n0, (n1, n2))
    }
}
