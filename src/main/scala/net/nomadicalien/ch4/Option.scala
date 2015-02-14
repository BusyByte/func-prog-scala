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
}