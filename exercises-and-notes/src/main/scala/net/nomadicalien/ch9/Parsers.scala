package net.nomadicalien.ch9

import org.scalacheck.Properties

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  implicit def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))


  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many[B>:A]: Parser[List[B]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  class Laws[+A] extends Properties("Parser"){
    import org.scalacheck.Prop.forAll
    property("equal") = forAll { (p1: Parser[A], p2: Parser[A], s: String) =>
        run(p1)(s) == run(p2)(s)
    }
    property("map") = forAll { (p1: Parser[A], s: String) =>
      val p2 = p1.map(identity)
      run(p1)(s) == run(p2)(s)
    }
  }
}