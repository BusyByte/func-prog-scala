package net.nomadicalien.ch3

import org.specs2.mutable.Specification

class ListSpec extends Specification {
  import net.nomadicalien.ch3.List.{map => listmap, _}

  "exercise 3.1, evaluate pattern match" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x must_== (3)
  }

  "exercise 3.2, implement tail" in {
    val x = List(1,2,3,4,5)
    tail(x) must_== (List(2,3,4,5))
  }

  "exercise 3.3, implement set head" in {
    val x = List(1,2,3,4,5)
    setHead(20,x) must_== (List(20, 2,3,4,5))
  }

  "exercise 3.4, generalize tail to drop" in {
    val x = List(1,2,3,4,5)
    drop(2,x) must_== (List(3,4,5))
  }

  "exercise 3.5, implement dropWhile" in {
    val x = List(1,2,3,4,5)
    dropWhile(x, {i:Int=>i < 4} ) must_== (List(4,5))
  }

  "exercise 3.6, implement init" in {
    val x = List(1,2,3,4)
    init(x) must_== (List(1,2,3))
  }

  "exercise 3.7, implement product as fold right with short circuit" in {
    val x = List(1.0,2.0,0.0,4.0)
    product3(x) must_== (0.0)//only evaluates multiply function 2 times instead of 4
  }

  "exercise 3.8, experiment with Nil and Cons" in {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) must_== (List(1,2,3))
  }

  "exercise 3.9, length with fold right" in {
    List.length(List(1,2,3)) must_== (3)
  }

  "exercise 3.10, implement stack-safe foldLight" in {
    foldLeft[Int,Int](List(1,2,3),0)((acc, elem)=> acc + elem) must_== (6)
  }

  "exercise 3.11, implement sum with foldLeft" in {
    sum3(List(1,2,3,4)) must_== (10)
  }

  "exercise 3.11, implement product with foldLeft" in {
    product4(List(1.0,2.0,3.0,4.0)) must_== (24.0)
  }

  "exercise 3.11, implement length with foldLeft" in {
    length2(List(1,2,3,4)) must_== (4)
  }

  "exercise 3.12, implement reverse" in {
    reverse(List(1,2,3)) must_== (List(3,2,1))
  }

  "exercise 3.13 implement foldLeft in terms of foldRight" in {
    //since functions evaluate arguments first each function composition causes the reversing of given order
    foldLeft2(List(1,2,3),0)((b,a)=> {
      //println(s"$a")
      a + b
    } ) must_== (6)
  }

  "exercise 3.13 implement foldRight in terms of foldLeft" in {
    foldRight2(List(1,2,3),0)((a,b)=> {
      //println(s"$a")
      a + b
    } ) must_== (6)
  }

  "exercise 3.14 implement append in terms of a fold" in {
    append(List(1,2,3),4) must_== (List(1,2,3,4))
  }

  "exercise 3.15 implement append in terms of a fold" in {
    val listOfLists = List(
    List(1,2,3),
    List(4,5,6),
    List(7,8,9)
    )
    concat(listOfLists) must_== (List(9,8,7,6,5,4,3,2,1))
  }

  "exercise 3.16 implement function which adds 1" in {
    add1(List(2,8,10)) must_== (List(3,9,11))
  }

  "exercise 3.17 implement function which converts double to string" in {
    dToString(List(2.0,8.0,10.0)) must_== (List("2.0","8.0","10.0"))
  }

  "exercise 3.18 implement map function" in {
    listmap(List(2.0,8.0,10.0))(_.toString) must_== (List("2.0","8.0","10.0"))
  }

  "exercise 3.19 implement filter function" in {
    filter(List(1,2,3,4,5,6,7,8,9))(_%2==0) must_== (List(2,4,6,8))
  }

  "exercise 3.20 implement flatmap" in {
    flatMap(List(1,2,3))(i=>List(i,i)) must_== (List(1,1,2,2,3,3))
  }

  "exercise 3.21 implement filter with flatMap" in {
    filter2(List(1,2,3,4,5,6,7,8,9))(_%2==0) must_== (List(2,4,6,8))
  }

  "exercise 3.22 implement method add numbers from two lists" in {
    addTogether(List(1,2,3),List(4,5,6)) must_== (List(5,7,9))
  }

  "exercise 3.23 generalize zipWith to apply function on two lists" in {
    zipWith(List(1,2,3),List(4,5,6))((a,b)=>a+b) must_== (List(5,7,9))
  }

  "exercise 3.24 implement subsequence" in {
    hasSubsequence(List(1,2,3,4), List(1,2)) must_== (true)
    hasSubsequence(List(1,2,3,4), List(2,3)) must_== (true)
    hasSubsequence(List(1,2,3,4), List(4)) must_== (true)
    hasSubsequence(List(1,2,3,4), List(5)) must_== (false)
  }
}
