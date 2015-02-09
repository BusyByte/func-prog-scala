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

  test("exercise 3.7, implement product as fold right with short circuit") {
    val x = List(1.0,2.0,0.0,4.0)
    product3(x) should be(0.0)//only evaluates multiply function 2 times instead of 4
  }

  test("exercise 3.8, experiment with Nil and Cons") {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3))
  }

  test("exercise 3.9, length with fold right") {
    List.length(List(1,2,3)) should be(3)
  }

  test("exercise 3.10, implement stack-safe foldLight") {
    foldLeft[Int,Int](List(1,2,3),0)((acc, elem)=> acc + elem) should be(6)
  }

  test("exercise 3.11, implement sum with foldLeft") {
    sum3(List(1,2,3,4)) should be(10)
  }

  test("exercise 3.11, implement product with foldLeft") {
    product4(List(1.0,2.0,3.0,4.0)) should be(24.0)
  }

  test("exercise 3.11, implement length with foldLeft") {
    length2(List(1,2,3,4)) should be(4)
  }

  test("exercise 3.12, implement reverse") {
    reverse(List(1,2,3)) should be(List(3,2,1))
  }

  test("exercise 3.13 implement foldLeft in terms of foldRight") {
    //since functions evaluate arguments first each function composition causes the reversing of given order
    foldLeft2(List(1,2,3),0)((b,a)=> {
      //println(s"$a")
      a + b
    } ) should be(6)
  }

  test("exercise 3.13 implement foldRight in terms of foldLeft") {
    foldRight2(List(1,2,3),0)((a,b)=> {
      //println(s"$a")
      a + b
    } ) should be(6)
  }
}
