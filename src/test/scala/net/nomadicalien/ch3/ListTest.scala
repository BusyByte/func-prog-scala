package net.nomadicalien.ch3

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class ListTest extends FunSuite with Matchers {
  import net.nomadicalien.ch3.List._

  test("exercise 3.1, evaluate pattern match") {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x should be(3)
  }

  test("exercise 3.2, implement tail") {
    val x = List(1,2,3,4,5)
    tail(x) should be(List(2,3,4,5))
  }

  test("exercise 3.3, implement set head") {
    val x = List(1,2,3,4,5)
    setHead(20,x) should be(List(20, 2,3,4,5))
  }

  test("exercise 3.4, generalize tail to drop") {
    val x = List(1,2,3,4,5)
    drop(2,x) should be(List(3,4,5))
  }

  test("exercise 3.5, implement dropWhile") {
    val x = List(1,2,3,4,5)
    dropWhile(x, {i:Int=>i < 4} ) should be(List(4,5))
  }

  test("exercise 3.6, implement init") {
    val x = List(1,2,3,4)
    init(x) should be(List(1,2,3))
  }

}
