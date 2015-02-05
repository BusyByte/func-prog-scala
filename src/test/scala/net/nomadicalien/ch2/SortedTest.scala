package net.nomadicalien.ch2

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Shawn on 2/4/2015.
 */
class SortedTest extends FunSuite with Matchers {
  import net.nomadicalien.ch2.Sorted.isSorted

  val ordered = {(a:Int,b:Int)=> a <= b}

  test("empty array is sorted") {
    isSorted[Int](Array[Int](), ordered) should be(true)
  }

  test("single element array is sorted") {
    isSorted[Int](Array[Int](1), ordered) should be(true)
  }

  test("all same value array is sorted") {
    isSorted[Int](Array[Int](1,1), ordered) should be(true)
  }

  test("array is sorted") {
    isSorted[Int](Array[Int](1,2,3), ordered) should be(true)
  }

  test("array is not sorted") {
    isSorted[Int](Array[Int](3,2,1), ordered) should be(false)
  }
}
