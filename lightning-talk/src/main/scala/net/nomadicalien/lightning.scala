package net.nomadicalien

import net.nomadicalien.lightning.totalFunctionExample.ValidDenominator

import scala.util.Try
import scalaz.\/


object lightning {

  object nonRTExample {
    /*
      * Throws java.lang.ArithmeticException: / by zero
      */
    def divideFourBy(denominator: Int): Int = {
      4 / denominator
    }
  }


  object optionExample {

    def divideFourBy(denominator: Int): Option[Int] = { // Loose information about what error happened with Option
      if(denominator > 0)
        Some(4 / denominator)
      else
        None
    }

  }

  object eitherExample {

    def divideFourBy(denominator: Int): Either[String, Int] = {
      if (denominator > 0)
        Right(4 / denominator)
      else
        Left("Denominator must be positive")
    }

  }

  object tryExample {

    def divideFourBy(denominator: Int): Try[Int] =
      Try(4 / denominator)

  }

  object basicDisjunctionExample {
    import scalaz._
    import Scalaz._
    import \/._

    def divideFourBy(denominator: Int): String \/ Int =
      fromTryCatchNonFatal(4 / denominator)
        .leftMap(e => e.getMessage) // could be nested exceptions with better message, could be losing both message and stack trace

  }

  object disjunctionExample {
    import scalaz._
    import Scalaz._

    sealed trait DivisionError
    case object DivisionByZeroError extends DivisionError
    case object ZeroResultError extends DivisionError



    def divideFourBy(denominator: Int): DivisionError \/ Int = {
      if(denominator == 0)
        DivisionByZeroError.left
      else if (denominator >= 4)
        ZeroResultError.left
      else
        (4 / denominator).right
    }


  }


  object totalFunctionExample {
    import scalaz._
    import Scalaz._

    sealed trait DivisionError
    case object DivisionByZeroError extends DivisionError
    case object ZeroResultError extends DivisionError

    final case class ValidDenominator private[totalFunctionExample] (denominator: Int) extends AnyVal


    object ValidDenominator {
      def create(denominator: Int): DivisionError \/ ValidDenominator = {
        if(denominator == 0)
          DivisionByZeroError.left
        else if (denominator >= 4)
          ZeroResultError.left
        else
          ValidDenominator(denominator).right
      }
    }


    def divideFourBy(validDenominator: ValidDenominator): Int = {
      4 / validDenominator.denominator
    }


  }


  object validationExample {
    import scalaz._
    import Scalaz._
    import Validation._

    sealed trait DivisionError
    case object DivisionByZeroError extends DivisionError
    case object ZeroResultError extends DivisionError

    def divideFourBy(denominator: Int): Validation[DivisionError, Int] = {
      if(denominator == 0)
        DivisionByZeroError.failure[Int]
      else if (denominator >= 4)
        ZeroResultError.failure[Int]
      else
        (4 / denominator).success[DivisionError]
    }



    implicit val errorSemiGroup = new Semigroup[DivisionError]{
      def append(f1: DivisionError, f2: => DivisionError): DivisionError = f1
    }


    def combineResults(den1: Int, den2: Int): Validation[DivisionError, Int] = {
      (divideFourBy(den1) |@| divideFourBy(den2))((a: Int, b: Int) => a + b)
    }

  }




}
