package net.nomadicalien.ch4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(b) => Right(f(b))
    case e@Left(_) => e
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case e@Left(_) => e
    case Right(a) => f(a)
  }


  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case a@Right(_) => a
    case l@Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap { a => b map { bb => f(a, bb)}}


  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

case object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case headMaybe :: tailMaybe => headMaybe flatMap (h => sequence(tailMaybe) map (t => h :: t))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case hMaybe :: tMaybe => f(hMaybe).map2(traverse(tMaybe)(f))(_ :: _)
  }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)
}


