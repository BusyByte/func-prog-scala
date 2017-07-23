package net.nomadicalien.ch9

import org.scalacheck.Properties

import scala.util.matching.Regex

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart }
}

case class ParseError(stack: List[(Location,String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_,s)).toList)
  def latestLoc: Option[Location] = latest map (_._1)
  def latest: Option[(Location,String)] = stack.lastOption
}

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  implicit def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))


  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)((a:A) => succeed(f(a)))

  //def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)((a: A) => map(p2)((b: B) => (a, b)))

//  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
//    map(product[A,B](p, p2))((z:(A,B))=>f(z._1, z._2))
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      flatMap(p)((a: A) => map(p2)((b: B) => f(a,b)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many[B>:A]: Parser[List[B]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice[B>:A]: Parser[String] = self.slice(p)
    def **[B>:A](p2: =>Parser[B]): Parser[(A,B)] = self.product(p,p2)
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

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}


/**
  * Implement string, regex, succeed, and slice
  */
object Exercise9_13 {

  type Parser[+A] = Location => Result[A] //that’s either a success or a failure.
  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] =
      this match {
        case Failure(e) => Failure(f(e))
        case _ => this
      }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  object Example1Parsers extends Parsers[Parser] {
    implicit def string(s: String): Parser[String] =
    scope(s"Parsing string $s")({ loc: Location =>
      val subStr = loc.input.substring(loc.offset)
      if(subStr.contains(s)) {
        Success(s, s.length)
      } else {
        Failure(ParseError(List((loc, s"$subStr does not contain $s"))))
      }
    })
    implicit def regex(r: Regex): Parser[String] =  {
      loc: Location =>
      val subStr = loc.input.substring(loc.offset)
        r.findFirstMatchIn(subStr)
          .fold[Result[String]](Failure(ParseError(List((loc, s"$subStr does not contain ${r.pattern.pattern()}"))))) { regExMatch =>
             Success(regExMatch.source.subSequence(regExMatch.start, regExMatch.end).toString,regExMatch.end - regExMatch.start)
          }

    }
    def succeed[A](a: A): Parser[A] = {
      loc: Location =>
        Success(a, loc.input.length - loc.offset)
    }
    def slice[A](p: Parser[A]): Parser[String] = {
      loc: Location => p(loc) match {
        case Success(_, n) =>
          val subStr = loc.input.substring(loc.offset)
          Success(subStr.substring(0, n), n)
        case f: Failure => f
      }
    }


    def label[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.label(msg))
    def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.push(s, msg))

    def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
    def orString(s1: String, s2: String): Parser[String] = ???
    def or[A](s1: Parser[A], s2: =>Parser[A]): Parser[A] = ???
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???
    def many[A](p: Parser[A]): Parser[List[A]] = ???
    def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???
    def errorLocation(e: ParseError): Location = ???
    def errorMessage(e: ParseError): String = ???
  }
}