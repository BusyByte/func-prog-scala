package net.nomadicalien.ch4



sealed trait Option[+A] {
  //can use pattern matching
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  //can be written in terms of other functions
  def flatMap[B](f: A => Option[B]): Option[B]  =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap((a) => if(f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(aa => b map (bb => f(aa,bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case hMaybe :: tMaybe => hMaybe flatMap(theHead => sequence(tMaybe) map(theTails => theHead :: theTails))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case hMaybe :: tMaybe => map2(f(hMaybe), traverse(tMaybe)(f))(_::_)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)

}