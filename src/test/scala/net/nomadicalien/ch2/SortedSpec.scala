package net.nomadicalien.ch2

import org.specs2.mutable.Specification

class SortedSpec extends Specification {
  import net.nomadicalien.ch2.Sorted.isSorted

  val ordered = {(a:Int,b:Int)=> a <= b}

  "empty array is sorted" in {
    isSorted[Int](Array[Int](), ordered) must_== (true)
  }

  "single element array is sorted" in {
    isSorted[Int](Array[Int](1), ordered) must_== (true)
  }

  "all same value array is sorted" in {
    isSorted[Int](Array[Int](1,1), ordered) must_== (true)
  }

  "array is sorted" in {
    isSorted[Int](Array[Int](1,2,3), ordered) must_== (true)
  }

  "array is not sorted" in {
    isSorted[Int](Array[Int](3,2,1), ordered) must_== (false)
  }
}
